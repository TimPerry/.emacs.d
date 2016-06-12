(use-package hydra
  :bind ("s-k" . hydra-kill-buffers/body))

(defhydra hydra-kill-buffers (nil nil)
  "Kill Buffers"
  ("a" kill-all-buffers "all")
  ("c" kill-current-buffer "current")
  ("o" kill-other-buffers "other")
  ("q" nil "quit"))
