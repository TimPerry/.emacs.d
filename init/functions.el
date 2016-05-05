(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defun jump-to-class ()
  "Find and go to the class at point"
  (interactive)
  (let ((class-name (thing-at-point 'symbol)))
    (ring-insert find-tag-marker-ring (point-marker))
    (save-excursion
      (beginning-of-buffer)
      (let ((start (search-forward-regexp "function\\s-*(" (point-max) t))
            (end (- (search-forward ")") 1)))
        (goto-char start)
        (if (search-forward class-name end t)
            (mapcar #'(lambda (lib-alist)
                        (let* ((id (car lib-alist))
                               (file-alist (cdr lib-alist))
                               (record (assoc (concat (downcase class-name) ".js") file-alist)))
                          (if (and record (= (length record) 2))
                            (let ((found-file (cadr record)))
                              (message "Found %s" found-file)
                              (find-file found-file))
                            nil)))
                    jpop-project-alist)
          nil)))))

(defun jump-to-require ()
  "Tries to load the require path assoscaited with a variable for node includes"
  (interactive)
  (save-excursion
    (ring-insert find-tag-marker-ring (point-marker))
    (goto-char (line-beginning-position))
    (let ((require-pos (search-forward "require" (line-end-position) t)))
      (when require-pos
        (if (search-forward-regexp "(['\"]\\(.*?\\)['\"])" (line-end-position) t)
            (mapcar #'(lambda (lib-alist)
                        (let* ((id (car lib-alist))
                               (file-alist (cdr lib-alist))
                               (file (file-name-base (match-string 1)))
                               (record (assoc (concat file ".js") file-alist)))
                          (if (and record (= (length record) 2))
                              (let ((found-file (cadr record)))
                                (message "Found %s" found-file)
                                (find-file found-file))
                            nil)))
                    jpop-project-alist)
          nil)))))

(defun jump-to-find-function ()
  "Go to the function definition for elisp"
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (find-function (intern (thing-at-point 'symbol))))

(defun jump-to-thing-at-point ()
  "Go to the thing at point assuming if it's not a class or function it's a variable."
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (p (point)))
    (if thing
        (when (and (eq nil (jump-to-require))
                   (or (eq nil (jump-to-class))
                       (equal '(nil) (jump-to-class))))
          (ignore-errors (js2-jump-to-definition))
          (when (and (eq p (point))
                     (not (eq nil (etags-select-find-tag-at-point))))
            (helm-swoop)))
      (etags-select-find-tag))))

(defun dgc-comment ()
  "comment or uncomment highlighted region or line"
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))


(defun save-all ()
  "Saving all buffers when losing focus."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)


(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
		  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
		  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

(defun mwheel-scroll-all-function-all (func arg)
  (if scroll-all-mode
      (save-selected-window
        (walk-windows 
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

(defun mwheel-scroll-all-scroll-up-all (arg)
  (mwheel-scroll-all-function-all 'scroll-up arg))

(defun mwheel-scroll-all-scroll-down-all (arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
(setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let (
        (deactivate-mark nil)
        p1 p2)
    (if (use-region-p)
        (setq p1 (region-beginning)
             p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq p1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region p1 p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region p1 p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region p1 p2)
      (put this-command 'state 0)))))


(defun toggle-cheatsheet ()
  "Toggle the cheatsheet buffer"
  (interactive)
  (if (get-buffer "*cheatsheet*")
      (progn (kill-buffer "*cheatsheet*")
	     (delete-window))
    (cheatsheet-show)))

(defun clear-terminal ()
  "Clear the terminal"
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defun multiply-pixels-in-buffer-by (by)
	"Multiplies all the numbers in the current buffer from 720 to 540 values"
	(interactive "Multiply by [e.g. 0.75, 1.5]: " 0.75)
	(goto-char 1)
	(while (search-forward-regexp "\\([0-9]+px\\)" nil t) 
		(replace-match
		 (concat (number-to-string
			 (floor (*
				 (string-to-number (match-string 1))
				 (string-to-number by))))
			"px") t nil)))
