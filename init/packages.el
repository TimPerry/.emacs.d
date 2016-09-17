;; packages.el --- my custom package setup

;;; Commentary:

;; my custom setup available at `https://github.com/TimPerry/.emacs.d`

;;; code:

(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; activate all the packages
(package-initialize)

;; install use package, the rest of the time we use :ensure to install.
(package-install 'use-package);

;; bring in use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; packages.el ends here
