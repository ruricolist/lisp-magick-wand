(defpackage :lisp-magick-wand
  (:nicknames :magick)
  (:use :common-lisp)
  (:export :quantum-8 :quantum-16 :quantum-32 :quantum-64
           :byte->quantum :quantum->byte
           :with-pixel-wand :with-drawing-wand :with-magick-wand :give-wand
           :with-pixel-data :pixel :get-pixel :set-pixel
           :magick-wand-error))
