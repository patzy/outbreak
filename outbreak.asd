;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem outbreak
  :description "Breakout clone"
  :depends-on (glaw glaw-imago glop)
  :serial t
  :components
  ((:file "package")
   (:file "main")))

