; os x
(setq mac-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

; app
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "<f6>") (lambda() (interactive)(load-file "~/.emacs.d/init.el")))

; buffers
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-k") '(lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "s-w") '(lambda () (interactive) (delete-window)))
(global-set-key (kbd "s-t") '(lambda () (interactive) (split-window-right)))
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "<f9>") 'kill-all-buffers)

; nav
(global-set-key (kbd "H-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "H-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

(global-set-key (kbd "H-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "H-<right>") 'end-of-line)

(global-set-key (kbd "M-<left>") '(lambda () (interactive) (forward-same-syntax -1)))
(global-set-key (kbd "M-<right>") 'forward-same-syntax)

(global-set-key (kbd "s-l") 'goto-line)

(global-set-key (kbd "H-<backspace>") 'delete-char)

(global-set-key (kbd "s-<backspace>") 'delete-current-line)
(global-set-key (kbd "s-d") 'duplicate-line)

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

; commenting
(global-set-key (kbd "s-/") 'dgc-comment)

; select, copy/paste
(global-set-key (kbd "s-c") 'pbcopy)
(global-set-key (kbd "s-v") 'pbpaste)
(global-set-key (kbd "s-x") 'pbcut)
(global-set-key (kbd "s-a") 'mark-whole-buffer)

;magit
(define-key global-map (kbd "<f2>") 'magit-status)
(define-key global-map (kbd "<f3>") 'vc-ediff)
(define-key global-map (kbd "<f4>") 'magit-push)

;; misc
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; toggle
(global-set-key (kbd "s-U") 'xah-toggle-letter-case)

;; dired
(define-key global-map (kbd "<f8>") 'find-name-dired)

