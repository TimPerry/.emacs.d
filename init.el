(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/keys")
(load "~/.emacs.d/init/packages")

(defconst base-path (file-name-directory load-file-name))

(require 'use-package)

(use-package rainbow-delimiters)
(use-package helm)

; theme
(add-to-list 'custom-theme-load-path (concat base-path "/themes"))
(load-theme 'base16-3024-dark t)

(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands)
        ("C-c M-x" . execute-extended-command))

(use-package powerline
  :init
  (powerline-default-theme))

; reset
(setq inhibit-startup-message t)
(tool-bar-mode -1)
