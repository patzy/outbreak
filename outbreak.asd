;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem outbreak
  :description "Breakout clone"
  :depends-on (glaw glaw-sdl lispbuilder-sdl)
  :serial t
  :components
  ((:file "package")
   (:file "main")))

