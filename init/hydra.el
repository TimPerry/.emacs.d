(use-package hydra
  :bind ("s-k" . hydra-kill-buffers/body))

(defhydra hydra-kill-buffers
  "Kill Buffers"
  ("a" kill-all-buffers "All")
  ("c" kill-current-buffer "Current")
  ("o" kill-other-buffers "Other")
  ("q" nil "quit"))
