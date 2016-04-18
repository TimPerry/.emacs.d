(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/keys")
(load "~/.emacs.d/init/packages")

(defconst base-path (file-name-directory load-file-name))

(require 'use-package)

(use-package rainbow-delimiters)
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
      helm-bookmark-show-location t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-quick-update t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t))

(add-to-list 'custom-theme-load-path (concat base-path "/themes"))
(load-theme 'base16-3024-dark t)

; reset
(setq inhibit-startup-message t)
(tool-bar-mode -1)
