(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/keys")
(load "~/.emacs.d/init/packages")

(defconst base-path (file-name-directory load-file-name))

(require 'use-package)

(use-package rainbow-delimiters)
(use-package helm)

; jpop
(use-package jpop
  :load-path "packages/jpop"
  :config
  (jpop-global-mode)
  (add-hook 'jpop-toggle-test-fallback-hook 'jpop-find-test)
  :bind
  ([C-tab] . jpop-find-file)
  ("C-S-<tab>" . jpop-git-find-file)
  ("C-x p f c" . jpop-change-and-find-file)
  ("C-x p c" . jpop-change)
  ("C-x C-b" . jpop-switch-buffer)
  ("C-x C-p" . jpop-switch-and-find-file))

; appearance
(global-linum-mode 1)
(add-to-list 'custom-theme-load-path (concat base-path "/themes"))
(load-theme 'aurora t)

(use-package drag-stuff
  :config (drag-stuff-global-mode 1)
  :bind ("<A-S-down>" . drag-stuff-down)
        ("<A-S-up>" . drag-stuff-up))

; autocomplete
(require 'ido)
(ido-mode t)

(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands)
        ("C-c M-x" . execute-extended-command))

;(use-package powerline
;  :init
;  (powerline-default-theme))

; reset
(setq inhibit-startup-message t)
(tool-bar-mode -1)
