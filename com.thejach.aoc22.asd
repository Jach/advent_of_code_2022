(defsystem "com.thejach.aoc22"
  :author "Jach"
  :license "Public Domain"

  :depends-on ("cl-ppcre" "alexandria" "let-plus" "str")

  :serial t
  :components ((:file "package")
               (:file "puzzle-inputs")
               (:file "main")))
