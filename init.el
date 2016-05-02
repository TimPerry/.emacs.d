;; init.el --- my custom setup

;;; Commentary:

;;; code:
(load "~/.emacs.d/init/custom")
(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/keys")
(load "~/.emacs.d/init/packages")
(load "~/.emacs.d/init/cheatsheets")

(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "init/custom.el"))

;; rainbow delimters
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; projectile
(use-package projectile
	:diminish projectile-mode
  :config (projectile-global-mode))

;; jpop
(use-package jpop
  :diminish jpop-mode
  :ensure f
  :load-path "packages/jpop"
  :config
  (jpop-global-mode)
  (add-hook 'jpop-toggle-test-fallback-hook 'jpop-find-test)
  :bind
  ([C-tab] . jpop-find-file)
  ("C-S-<tab>" . jpop-git-find-file)
  ("C-x p f c" . jpop-change-and-find-file)
  ("<f8>" . jpop-change)
  ("C-x C-b" . jpop-switch-buffer)
  ("C-x C-p" . jpop-switch-and-find-file))

;; appearance
(global-linum-mode 1)
(add-to-list 'custom-theme-load-path (concat base-path "/themes"))
(load-theme 'aurora t)

;; context colouring
(use-package context-coloring
  :defer t
  :config (advice-add 'load-theme :requires
                      '(lambda (&rest args) (context-coloring-mode 0))))

;; drag lines up and down
(use-package drag-stuff
	:diminish drag-stuff-mode
  :init (setq drag-stuff-modifier 'ctrl) ; hack to stop drag-stuff setting key mappings over ones our existing
  :config (drag-stuff-global-mode 1)
  (bind-keys :map drag-stuff-mode-map
	     ("<M-S-down>" . drag-stuff-down)
	     ("<M-S-up>" . drag-stuff-up)))

(eval-after-load "drag-stuff"
  '(define-key drag-stuff-mode-map (kbd "<M-left>") nil))

;; autocomplete minibuffer
(use-package ivy
	:diminish ivy-mode
  :ensure f
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

;; autocomplete editor
(use-package auto-complete
  :diminish auto-complete-mode
  :config (ac-config-default))
(use-package ac-js2 
  :requires auto-complete
  :config (add-hook 'js2-mode-hook 'ac-js2-mode))

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

;; poerline
(use-package powerline 
 :init
 (powerline-default-theme))
(add-hook 'after-init-hook 'powerline-reset)

;; window numbering
(use-package window-numbering)

;; web-mode
(use-package web-mode 
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package js2-mode 
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode)))

(use-package web-beautify)

(use-package jsx-mode 
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode)))

(use-package json-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

;; markdown mode
(use-package markdown-mode 
  :config (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; scss-mode
(use-package scss-mode 
  :init
  (setq exec-path (cons (expand-file-name "~/.rvm/gems/ruby-2.0.0-p481/bin/sass") exec-path))
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  :config
  (setq scss-compile-at-save nil))

;; docker-mode
(use-package docker  
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; php mode
(use-package php-mode)

;; neo-tree
(use-package neotree
  :config (setq neo-smart-open t)
  :bind ("<f1>" . neotree-toggle))

;; ibuffer
(use-package ibuffer
  :bind ("<f5>" . ibuffer))

;; flycheck
(use-package flycheck
	:diminish flycheck-mode
  :config (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

;; popwin
(use-package popwin
  :config (popwin-mode 1))

;; magit
(use-package magit
  :defer t
  :config (bind-keys :map magit-mode-map
                     ("o" . magit-open-file-other-window)
                     ("C-c c" . magit-whitespace-cleanup)
                     ("C-c e" . magit-vc-ediff)
                     ("C-<tab>" . jpop-find-file)))

(add-hook 'magit-mode-hook 'image-minor-mode)

;; git timemachine
(use-package git-timemachine)

;; git gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

;; better redo/undo
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  :bind ("s-z" . undo)
  ("s-Z" . redo))
(use-package undohist
  :config (undohist-initialize))

;; multi line edit
(use-package multiple-cursors 
  :bind ("M-<down>" . mc/mark-next-like-this)
  ("M-<up>" . mc/mark-previous-like-this))

;; expand region
(use-package expand-region 
  :bind ("s-e" . er/expand-region))

;; jump mode
(use-package ace-jump-mode
  :bind ("s-j" . ace-jump-mode)
  :bind ("s-i" . ace-jump-line-mode))

;; visual-regexp
(use-package visual-regexp
  :bind ("s-r" . vr/replace))

;; aggressive-indent
(use-package aggressive-indent
  :disabled t
  :config (global-aggressive-indent-mode 1))

;; terminal
(use-package shell-pop)
(use-package shell  
  :config
  (bind-keys :map shell-mode-map
	     ("s-k" . clear-terminal)))

;; grunt
(use-package grunt)

;; rake
(use-package rake)

;; vagrant
(use-package vagrant)
(use-package vagrant-tramp)

;; misc
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
(desktop-save-mode 1) ; Restore files on startup
(setq make-backup-files nil); don't make backup files
(setq create-lockfiles nil) ; don't make lock files
(setq auto-save-default nil) ; don't autosave
(setq visible-bell nil) ; disables beep and use visible bell
(setq ns-function-modifier 'hyper) ; set Hyper to Mac's Fn key
(delete-selection-mode 1) ; Allows for deletion when typing over highlighted text
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or no
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(put 'erase-buffer 'disabled nil)
(setq initial-scratch-message ";; Excuse me sir, do you have a moment to talk about our Lord, Savior, and the one true operating system, Emacs?\n");

;; date and time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
(setq ediff-keep-variants nil)

;; path fix
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; recent files
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind ("<f10>" . recentf-open-files))

;; games
(use-package pacmacs)

(provide 'init)
;;; init.el ends here
