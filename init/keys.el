; os x
(setq mac-function-modifier 'hyper)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

; app
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

; buffers
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-k") '(lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-s") 'save-buffer)

; nav
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)

(global-set-key (kbd "A-<left>") 'left-word)
(global-set-key (kbd "A-<right>") 'right-word)

(global-set-key (kbd "s-l") 'goto-line)

(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "s-d") 'duplicate-line)

; commenting
(global-set-key (kbd "s-/") 'dgc-comment)

; copy/paste
(define-key global-map (kbd "s-v") 'yank)
(define-key global-map (kbd "s-c") 'kill-ring-save)

; undo
(define-key global-map (kbd "s-z") 'undo)
