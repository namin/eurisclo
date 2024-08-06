(asdf:defsystem :eurisclo
  :description "A Common Lisp port of Doug Lenat's EURISKO"
  :version "0.0"
  :author "White Flame"
  :license "Do not use, in any way, for any purpose"
  
  :depends-on (:alexandria
               :bordeaux-threads)

  :serial t
  :components ((:file "eurisclo")
               (:file "units")
               (:file "heuristics")))
