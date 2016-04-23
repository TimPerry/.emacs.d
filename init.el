;;; init.el --- my custom setup

;;; Commentary:

;;; code:
(load "~/.emacs.d/init/custom")
(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/keys")
(load "~/.emacs.d/init/packages")

(defconst base-path (file-name-directory load-file-name))

(require 'use-package)

; rainbow delimters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; projectile
(projectile-global-mode)

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

; context colouring
(use-package context-coloring-mode
  :defer t
  :config (advice-add 'load-theme :requires
                      '(lambda (&rest args) (context-coloring-mode 0))))

(use-package drag-stuff
  :config (drag-stuff-global-mode 1)
  :bind ("<M-S-down>" . drag-stuff-down)
        ("<M-S-up>" . drag-stuff-up))

; autocomplete
(use-package ivy
  :init (ivy-mode)
          (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :bind ("C-c C-r" . ivy-resume)
  ("C-s" . swiper)
  :config
  (bind-keys :map ivy-minibuffer-map
	       ("RET" . ivy-alt-done)))

(use-package counsel
  :requires ivy
  :bind ("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file))

(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands)
        ("C-c M-x" . execute-extended-command))

;(use-package powerline
;  :init
;  (powerline-default-theme))

; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

; docker-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

; neo-tree
(require 'neotree)
(global-set-key [f1] 'neotree-toggle)

;flycheck
(global-flycheck-mode)

; popwin
(require 'popwin)(popwin-mode 1)

;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; sass-mode
(use-package sass-mode
  :init
  (setq exec-path (cons (expand-file-name "~/.rvm/gems/ruby-2.0.0-p481/bin/sass") exec-path))
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

; magit
(use-package magit
  :defer t
  :config (bind-keys :map magit-mode-map
                     ("o" . magit-open-file-other-window)
                     ("C-c c" . magit-whitespace-cleanup)
                     ("C-c e" . magit-vc-ediff)
                     ("C-<tab>" . jpop-find-file)))

(add-hook 'magit-mode-hook 'image-minor-mode)

; css colours
(use-package mon-css-color
  :load-path "elisp"
  :init (autoload 'css-color-mode "mon-css-color" "" t)
  :config (css-color-global-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind ("s-z" . undo)
  ("s-Z" . redo))

; misc
(setq tab-width 2) ; Default tab-width
(add-hook 'focus-out-hook 'save-all) ; Auto-saving when losing focus
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(setq-default cursor-type 'bar)             ; Change cursor to bar
(setq frame-title-format "Dr. Ian Malcolm: God help us, we're in the hands of engineers.")
(scroll-bar-mode -1)
(delete-selection-mode 1) ; Deleting selected text if typed in/pasted
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no
(setq make-backup-files nil) ; Preventing backup (~) files
(show-paren-mode 1) ; Always show matching parenthesis

;; diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-keep-variants nil)

(provide 'init)
;;; init.el ends here
