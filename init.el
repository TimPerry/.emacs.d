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
  :bind ("<A-S-down>" . drag-stuff-down)
        ("<A-S-up>" . drag-stuff-up))

; autocomplete
(use-package ido
  :demand
  :config
  (ido-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)                     ; For dired use C-j to quit at that path
  (setq ido-enable-regexp t)
  (setq ido-create-new-buffer 'always)

  (defalias 'ido-magic-forward-char 'ido-next-match)
  (defalias 'ido-magic-backward-char 'ido-prev-match)

  :bind
  ("C-x C-f" . ido-find-file)
  ("C-x f" . ido-find-file)
  ("C-x F" . ido-find-file-other-window)
  ("C-x B" . ido-switch-buffer-other-window)
  ("C-x b" . ido-switch-buffer))

(use-package ido-vertical-mode
  :requires ido
  :config (ido-vertical-mode 1)
  (add-hook 'ido-vertical-mode-hook
            '(lambda () (bind-keys :map ido-common-completion-map
                              ("C-f" . ido-next-match)
                              ("C-b" . ido-prev-match)))))


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
(require 'popwin)
(popwin-mode 1)

;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

; misc
(setq tab-width 2) ; Default tab-width
(add-hook 'focus-out-hook 'save-all) ; Auto-saving when losing focus
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(setq frame-title-format "Dr. Ian Malcolm: God help us, we're in the hands of engineers.")
(scroll-bar-mode -1)
(delete-selection-mode 1) ; Deleting selected text if typed in/pasted
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no
(setq make-backup-files nil) ; Preventing backup (~) files
(show-paren-mode 1) ; Always show matching parenthesis
