;; custom.el --- my custom setup

;;; Commentary:

;; my custom setup available at `https://github.com/TimPerry/.emacs.d`

;;; code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-week-start-day 1)
 '(custom-safe-themes
   (quote
    ("f146cf0feba4fed38730de65e924e26140b470a4d503287e9ddcf7cca0b5b3f0" "dc1a336969593b934cdea0b133930fe893d1a091cfeb3c0ca0bb5312cd8d27a5" default)))
 '(desktop-save (quote ask))
 '(evil-default-state (quote normal))
 '(evil-toggle-key "§")
 '(git-gutter:update-interval 2)
 '(indent-tabs-mode nil)
 '(ivy-display-style nil)
 '(ivy-height 25)
 '(jpop-completion-func (quote car))
 '(jpop-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "/\\.svn" "\\.elc$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.png$" "\\.svn*" "\\/node_modules\\/*" "\\.gif$" "\\.gem$" "\\.pdf$" "\\.swp$" "\\.iml$" "\\.jar$" "\\/build\\/" "/\\.git" "\\/jsdoc\\/" "\\.min\\.js$" "\\.tags$" "\\.filecache" "\\.cache$" "\\/.git\\/" "\\/report\\/" "\\.gcov\\.html$" "\\.func.*\\.html$" "\\/tmp\\/")))
 '(jpop-project-directory "~/Projects")
 '(js-indent-level 2)
 '(neo-after-create-hook nil)
 '(neo-banner-message nil)
 '(neo-cwd-line-style (quote text))
 '(neo-dont-be-alone t)
 '(neo-mode-line-type (quote neotree))
 '(neo-show-hidden-files t)
 '(neo-smart-open nil)
 '(neo-theme (quote nerd))
 '(neo-window-width 40)
 '(nyan-animate-nyancat t)
 '(nyan-bar-length 32)
 '(nyan-mode t)
 '(nyan-wavy-trail nil)
 '(package-selected-packages
   (quote
    (yaml-mode wn-mode whitespace-cleanup-mode web-mode web-beautify vue-mode visual-regexp vagrant-tramp vagrant use-package undohist toggle-quotes sublimity spray smooth-scrolling smex smartparens smart-newline shell-pop scss-mode restart-emacs request rake rainbow-mode rainbow-delimiters project-explorer powerline-evil popwin php-mode persp-projectile pacmacs nyan-mode neotree markdown-mode magit jsx-mode json-mode js2-refactor indent-guide ibuffer-vc hydra highlight-indent-guides grunt golden-ratio git-timemachine git-gutter-fringe flycheck-pos-tip expand-region exec-path-from-shell evil-snipe evil-matchit evil-mark-replace evil-easymotion emmet-mode duplicate-thing dumb-jump drag-stuff docker deft counsel-projectile counsel-dash context-coloring cheatsheet benchmark-init auto-complete ansible ace-jump-zap ac-js2 0blayout)))
 '(popwin:popup-window-height 25)
 '(powerline-color-alist (quote ((powerline-color1 1) (powerline-color2 1))) t)
 '(powerline-color0 "#3C444C")
 '(powerline-color1 "#96C348" t)
 '(powerline-color2 "#1F2127" t)
 '(powerline-foreground "#FFFFFF")
 '(powerline-height 30)
 '(projectile-completion-system (quote ivy))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(projectile-sort-order (quote modification-time))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (quote
            (lambda nil
              (byte-compile-file
               (buffer-file-name))))
           nil t))))
 '(shell-pop-universal-key "s-t")
 '(smex-flex-matching t)
 '(smex-prompt-string "Hold on to your butts: ")
 '(standard-indent 2)
 '(tempbuf-kill-message "%s")
 '(tempbuf-kill-message-function (quote my-tempbuf-message))
 '(tempbuf-minimum-timeout 300)
 '(undohist-ignored-files (quote ("COMMIT_EDITMSG")))
 '(vagrant-project-directory "~/vagrant/tool-tal-pal-sandbox")
 '(window-numbering-auto-assign-0-to-minibuffer t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#97C150"))))
 '(neo-expand-btn-face ((t (:foreground "#97C150"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#3C444C"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#96C348"))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "#2C3B40"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "#ebebeb" :foreground "#252525"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "#4C5725"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#1F2127"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#FFFFFF")))))

(provide 'custom)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; custom.el ends here
