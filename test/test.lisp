(defpackage :lisp-magick-wand/test
  (:use :cl :fiveam :lisp-magick-wand :trivial-file-size))
(in-package :lisp-magick-wand/test)

(defun draw-a-few-lines (filename width height)
  "Create a new image with WIDTH x HEIGHT pixel containing 50 random lines
    and save it in FILENAME."
  (magick:with-magick-wand (wand :create width height :comp (0 0 0))
    (magick:with-drawing-wand (dw)
      (magick:with-pixel-wand (pw :comp (255 255 255))
        (magick:draw-set-stroke-color dw pw))
      (magick:draw-set-stroke-width dw 3d0)
      (dotimes (i 50)
        (magick:draw-line dw
                          (coerce (random width) 'double-float)
                          (coerce (random height) 'double-float)
                          (coerce (random width) 'double-float)
                          (coerce (random height) 'double-float)))
      (magick:draw-image wand dw))
    (magick:write-image wand filename)))

(def-suite lisp-magick-wand)
(in-suite lisp-magick-wand)

(defun run-tests ()
  (let ((*on-failure* :debug))
    (run! 'lisp-magick-wand)))

(test draw-a-few-lines
  (uiop:with-temporary-file (:pathname p :type "jpeg")
    (finishes
      (draw-a-few-lines (namestring p)
                        100 100))
    (is (uiop:file-exists-p p))
    (is (> (file-size-in-octets p) 0))))
