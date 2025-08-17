(defsystem "lisp-magick-wand"
  :name "lisp-magick-wand"
  :author "Hans Bulfone"
  :licence "BSD"
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "ImageMagick binding"
  :homepage "https://github.com/ruricolist/lisp-magick-wand"
  :source-control (:git "https://github.com/ruricolist/lisp-magick-wand.git")
  :serial t
  :in-order-to ((test-op (test-op "lisp-magick-wand/test")))
  :depends-on ("alexandria" "cffi")
  :components ((:file "packages")
               (:file "base")
               (:file "quantum")
               (:file "types")
               (:file "option")
               (:file "enums")
               (:file "magick")
               (:file "utils")))

(defsystem "lisp-magick-wand/test"
  :description "Test suite for lisp-magick-wand"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("lisp-magick-wand" "fiveam" "trivial-file-size")
  :perform (test-op (o c) (symbol-call :lisp-magick-wand/test :run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "test")))
