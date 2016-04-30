(use-package cheatsheet
  :bind ("<f12>" . toggle-cheatsheet))

(cheatsheet-add :group '"Function keys"
								:key "F1"
                :description "Open neotree (directory pane)")

(cheatsheet-add :group '"Function keys"
								:key "F2"
                :description "Magit status")

(cheatsheet-add :group '"Function keys"
								:key "F3"
                :description "Git pull")

(cheatsheet-add :group '"Function keys"
								:key "F4"
                :description "Git push")

(cheatsheet-add :group '"Function keys"
								:key "F6"
                :description "Reload init file")

(cheatsheet-add :group '"Function keys"
								:key "F7"
                :description "List recent files")

(cheatsheet-add :group '"Function keys"
								:key "F8"
                :description "JPOP change")

(cheatsheet-add :group '"Function keys"
								:key "F7"
                :description "Kill all buffers")

(cheatsheet-add :group '"Function keys"
								:key "F10"
                :description "List recent files")

(cheatsheet-add :group '"Function keys"
								:key "F12"
                :description "Show cheatsheat")

(cheatsheet-add :group 'Find
								:key "CTRL + s"
                :description "Find in file. Press RETURN or CTRL +  s to exit")

(cheatsheet-add :group 'Find
								:key "CMD + j"
                :description "Jump mode (don't forget to press space)")

(cheatsheet-add :group 'Find
								:key "CMD + i"
                :description "Jump line mode")

(cheatsheet-add :group 'Find
								:key "CMD + e"
                :description "Expand selection region")

(cheatsheet-add :group 'Find
								:key "CMD + r"
                :description "regex find and replace")

(cheatsheet-add :group 'edit
								:key "ALT + Shift + <up|down>"
                :description "drag line up/down")

(cheatsheet-add :group 'edit
								:key "ALT + <up|down>"
                :description "Multi cursor edit (press enter to exit)")

(cheatsheet-add :group 'JPOP
								:key "<CTRL + x> p f t"
                :description "Find a test file")

(cheatsheet-add :group 'JPOP
								:key "<CTRL + x> p f e"
                :description "Find a file in a library")


