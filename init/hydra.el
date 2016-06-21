;; hydras.el --- my custom hydras

;;; Commentary:

;;; code:

(use-package hydra
  :bind ("s-k" . hydra-kill-buffers/body))

(defhydra hydra-kill-buffers (nil nil)
  "Manage Buffers"
  ("a" kill-all-buffers "kill all")
  ("k" kill-current-buffer "kill current")
  ("o" kill-other-buffers "kill others")
  ("n" next-buffer "next buffer")
  ("p" previous-buffer "previous buffer")
  ("q" nil "quit"))
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'after-save-hook '(lambda () (byte-compile-file (buffer-file-name))) nil t)
;; End:
;;; hydras.el ends here
