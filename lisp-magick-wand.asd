(defsystem lisp-magick-wand
  :name "lisp-magick-wand"
  :author "Hans Bulfone"
  :licence "BSD"
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "ImageMagick binding"
  :homepage "https://github.com/ruricolist/lisp-magick-wand"
  :source-control (:git "https://github.com/ruricolist/lisp-magick-wand.git")
  :serial t
  :depends-on (#:cffi #:alexandria)
  :components ((:file "packages")
               (:file "base")
               (:file "quantum")
               (:file "types")
               (:file "option")
               (:file "enums")
               (:file "magick")
               (:file "utils")))
