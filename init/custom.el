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
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(desktop-save (quote ask))
 '(evil-default-state (quote normal))
 '(evil-toggle-key "§")
 '(git-gutter:update-interval 2)
 '(indent-tabs-mode nil)
 '(ivy-display-style nil)
 '(ivy-height 25)
 '(neo-after-create-hook nil)
 '(neo-banner-message nil)
 '(neo-cwd-line-style (quote text))
 '(neo-dont-be-alone t)
 '(neo-mode-line-type (quote neotree))
 '(neo-show-hidden-files t)
 '(neo-smart-open nil)
 '(neo-theme (quote nerd))
 '(neo-window-width 40)
 '(package-selected-packages
   (quote
    (yaml-mode wn-mode whitespace-cleanup-mode web-mode web-beautify vue-mode visual-regexp vagrant-tramp vagrant use-package undohist toggle-quotes sublimity spray smooth-scrolling smex smartparens smart-newline shell-pop scss-mode restart-emacs request rake rainbow-mode rainbow-delimiters project-explorer powerline-evil popwin php-mode persp-projectile pacmacs nyan-mode neotree markdown-mode magit jsx-mode json-mode js2-refactor indent-guide ibuffer-vc hydra highlight-indent-guides grunt golden-ratio git-timemachine git-gutter-fringe flycheck-pos-tip expand-region exec-path-from-shell evil-snipe evil-matchit evil-mark-replace evil-easymotion emmet-mode duplicate-thing dumb-jump drag-stuff docker deft counsel-projectile counsel-dash context-coloring cheatsheet benchmark-init auto-complete ansible ace-jump-zap ac-js2 0blayout)))
 '(popwin:popup-window-height 25)
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
 '(smex-flex-matching t)
 '(smex-prompt-string "Hold on to your butts: ")
 '(standard-indent 2)
 '(tempbuf-kill-message "%s")
 '(tempbuf-kill-message-function (quote my-tempbuf-message))
 '(tempbuf-minimum-timeout 300)
 '(undohist-ignored-files (quote ("COMMIT_EDITMSG")))
 '(window-numbering-auto-assign-0-to-minibuffer t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "#1BB0AC"))))
 '(neo-expand-btn-face ((t (:foreground "#1BB0AC"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#3C444C" :foreground "#ebebeb"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#1BB0AC" :foreground "#ebebeb"))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "#A537DF" :foreground "#ebebeb"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "#ebebeb" :foreground "#252525"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "#373B41" :foreground "#ebebeb"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#1F2127"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#FFFFFF")))))

(provide 'custom)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; custom.el ends here
