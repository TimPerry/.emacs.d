; app
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

; buffers
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "s-k") '(lambda () (interactive) (kill-buffer (buffer-name))))

; mini buffer
(global-set-key (kbd "M-x") 'helm-M-x)