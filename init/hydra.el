(use-package hydra)

(defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
			 :post (progn
				 (set-cursor-color "#ffffff")
				 (message
				  "Thank you, come again.")))
  "vi"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("q" nil "quit"))

