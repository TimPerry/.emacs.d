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
