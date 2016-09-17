;; init.el --- my custom setup

;;; Commentary:

;; my custom setup available at `https://github.com/TimPerry/.emacs.d`

;;; code:
(load "~/.emacs.d/init/functions")
(load "~/.emacs.d/init/packages")
(load "~/.emacs.d/init/hydra")

(defconst base-path (file-name-directory load-file-name))
(setq custom-file (concat base-path "init/custom.el"))

;; rainbow delimters
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-indexing-method 'alien)
  (projectile-global-mode)
  :bind ("M-<tab>" . projectile-find-file)
  ("<backtab>" . projectile-switch-project)
  ("s-O" . projectile-find-file))

;; appearance
(global-linum-mode 1)
(add-to-list 'custom-theme-load-path (concat base-path "/themes"))
(load-theme 'aurora t)

;; context colouring
(use-package context-coloring
  :defer t
  :config (advice-add 'load-theme :requires
                      '(lambda (&rest args) (context-coloring-mode 0))))

;; evil (vim) mode
(use-package evil
  :config (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(bar "white")
      evil-emacs-state-cursor '(bar "white")
      evil-normal-state-cursor '(hbar "#97C150"))
  (bind-keys :map evil-normal-state-map
	     ("r" . evilmr-replace-in-defun))
  (evil-mode t))

(use-package evil-mark-replace)

(use-package evil-easymotion
  :init (evilem-default-keybindings "SPC"))

(use-package evil-snipe
  :requires evil
  :diminish evil-snipe-local-mode
  :init (evil-snipe-mode 1))

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

;; duplication things
(use-package duplicate-thing
  :bind ("s-d" . duplicate-thing))

;; autocomplete minibuffer
(use-package ivy
  :diminish ivy-mode
  :ensure f
  :init (ivy-mode)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :bind ("C-c C-r" . ivy-resume)
  ("C-s" . swiper)
  ("s-f" . swiper)
  :config
  (bind-keys :map ivy-minibuffer-map
	     ("RET" . ivy-alt-done)))

(use-package counsel
  :requires ivy
  :bind ("C-x C-f" . counsel-find-file)
  ("s-g" . counsel-git-grep))

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

;; whitespace cleanup
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

;; powerline
(use-package powerline
  :init
  (powerline-evil-center-color-theme))
(add-hook 'after-init-hook 'powerline-reset)

(use-package powerline-evil)
(use-package evil-matchit
  :diminish t
  :init (global-evil-matchit-mode 1))

;; window numbering
(use-package wn-mode
  :init
  (setq wn-keybinding-format "s-%s")
  (wn-mode))

;; emmet-mode
(use-package emmet-mode
  :config (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; web-mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package vue-mode
  :config   (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

(use-package web-beautify)

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

;; hex colours
(use-package rainbow-mode
  :diminish rainbow-mode
  :init (rainbow-mode))

;; org-mode
(use-package org
  :config (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode)))

;; deft
(use-package deft
  :config (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/Dropbox/notes")
  :bind ("<f7>" . deft))

;; docker-mode
(use-package docker
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; ansible mode
(use-package ansible
  :init (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;; ruby mode
(use-package ruby-mode
  :config (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

;; sh mode
(use-package sh-mode
  :ensure nil
  :config (add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode)))

;; neo-tree
(use-package neotree
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  :bind ("<f1>" . neotree-project-dir))

;; ibuffer
(use-package ibuffer
  :bind ("<f5>" . ibuffer))

(use-package ibuffer-vc
  :requires ibuffer
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package flycheck-pos-tip
  :config (flycheck-pos-tip-mode))

;; popwin
(use-package popwin
  :config
  (popwin-mode 1))

;; smooth scrolling
(use-package smooth-scrolling
  :init (smooth-scrolling-mode))

;; magit
(use-package magit
  :config (bind-keys :map magit-mode-map
                     ("o" . magit-open-file-other-window)
                     ("C-c c" . magit-whitespace-cleanup)
                     ("C-c e" . magit-vc-ediff)))

(add-hook 'magit-mode-hook 'image-minor-mode)

;; git timemachine
(use-package git-timemachine)

;; git gutter
(use-package git-gutter-fringe
  :if window-system
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

;;indent-guide
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :init (highlight-indent-guides-mode))

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

;; jump to thing
(use-package dumb-jump
  :init (dumb-jump-mode)
  :bind ("s-b" . dumb-jump-go)
  ("S-b" . dumb-jump-back))

;; multi line edit
(use-package multiple-cursors
  :bind ("M-<down>" . mc/mark-next-like-this)
  ("M-<up>" . mc/mark-previous-like-this))

;; smart new line
(use-package smart-newline
  :bind ("RET" . smart-newline))

;; smartparens
(use-package smartparens
  :diminish smartparens-mode
  :init (smartparens-global-mode))

;; swap quotes
(use-package toggle-quotes
  :bind ("s-'" . toggle-quotes))

;; expand region
(use-package expand-region
  :bind ("s-e" . er/expand-region))

;; jump mode
(use-package ace-jump-mode
  :bind ("s-j" . ace-jump-mode)
  :bind ("s-i" . ace-jump-line-mode))

;; jump mode zap
(use-package ace-jump-zap
  :bind ("s-p" . ace-jump-zap-to-char))

;; visual-regexp
(use-package visual-regexp
  :bind ("s-r" . vr/replace))

;; restart emacs
(use-package restart-emacs)

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

;; fix ansi colours
(use-package ansi-color
  :init(add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

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

;;
(use-package tempbuf
  :diminish tempbuf-mode
  :ensure nil
  :load-path "packages/tempbuf"
  :init
  (add-hook 'after-change-major-mode-hook 'turn-on-tempbuf-mode))

;; load after everything else to make they get priority
(load "~/.emacs.d/init/custom")
(load "~/.emacs.d/init/keys")

;; go full screen
(toggle-frame-fullscreen)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; init.el ends here
