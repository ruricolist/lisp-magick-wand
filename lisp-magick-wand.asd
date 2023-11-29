
(defpackage :lisp-magick-system
  (:use :cl :asdf))
(in-package :lisp-magick-system)

(defsystem lisp-magick-wand
  :name "lisp-magick-wand"
  :author "Hans Bulfone"
  :licence "BSD"
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "ImageMagick binding"
  :serial t
  :components
  ((:file "packages")
   (:file "base")
   (:file "quantum")
   (:file "types")
   (:file "option")
   (:file "enums")
   (:file "magick")
   (:file "utils"))
  :depends-on (#:cffi #:alexandria))
