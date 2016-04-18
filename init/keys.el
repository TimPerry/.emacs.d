; app
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

; buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-buffer (buffer-name))))
(global-set-key (kbd "M-u") 'revert-buffer)
(global-set-key (kbd "M-s") 'save-buffer)

; nav
(global-set-key (kbd "M-<left>") 'beginning-of-line)
(global-set-key (kbd "M-<right>") 'end-of-line)

(global-set-key (kbd "A-<left>") 'left-word)
(global-set-key (kbd "A-<right>") 'right-word)

; mini buffer
(global-set-key (kbd "M-x") 'helm-M-x)
