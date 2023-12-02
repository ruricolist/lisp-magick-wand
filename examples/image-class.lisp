(defpackage image
  (:use #:cl
        #:alexandria)
  (:import-from #:serapeum
                #:lret
                #:in))
(in-package #:image)

(defclass image ()
  ((wand :initarg :wand
         :accessor image-wand
         :initform (magick:new-magick-wand))))


(defmethod initialize-instance :after ((img image) &key)
  (trivial-garbage:finalize img
                            (lambda () (magick:destroy-pixel-wand (image-wand img)))))

(defmethod after-load ((img image))
  (magick:auto-orient-image (image-wand img))
  (magick:strip-image (image-wand img)))

;; PUBLIC
(defun make-from-file (path)
  (lret ((img (make-instance 'image)))
    (magick:read-image (image-wand img) path)
    (after-load img)))

(defun make-from-octets (data)
  (lret ((img (make-instance 'image)))
    (magick:read-image-blob (image-wand img) data)
    (after-load img)))

(defmethod clone ((img image))
  (make-instance 'image
                 :wand (magick:clone-magick-wand (image-wand img))))

(defmethod get-data ((img image) &optional (format "JPEG") (quality 90))
  (magick:set-image-compression-quality (image-wand img) quality)
  (magick:set-image-format (image-wand img) format)
  (magick:get-image-blob (image-wand img)))

(defmethod save-to-file ((img image) path &optional (quality 90))
  (magick:set-image-compression-quality (image-wand img) quality)
  (magick:write-image (image-wand img) path))

(defmethod crop ((img image) x y width height)
  (magick:crop-image (image-wand img) width height x y))

(defmethod resize ((img image) width height &optional (filter :lanczos))
  (magick:resize-image (image-wand img) width height filter))

(defmethod downscale-with-aspect-ratio ((img image) new-width new-height)
  (assert (or (numberp new-width) (numberp new-height)))
  (let* ((width (magick:get-image-width (image-wand img)))
         (height (magick:get-image-height (image-wand img)))
         (ratio (min (if (numberp new-width) (/ new-width width) 1000000)
                     (if (numberp new-height) (/ new-height height) 1000000))))
    (when (< ratio 1)
      (resize img (round (* width ratio)) (round (* height ratio))))))

(defmethod thumbnail ((img image) width height)
  (let* ((img-width (magick:get-image-width (image-wand img)))
         (img-height (magick:get-image-height (image-wand img)))
         (ratio (max (/ width img-width) (/ height img-height)))
         (tmp-width (round (* img-width ratio)))
         (tmp-height (round (* img-height ratio))))
    (when (< ratio 1)
      (resize img tmp-width tmp-height))
    (magick:crop-image (image-wand img)
                       width height
                       (round (/ (- tmp-width width) 2))
                       (round (/ (- tmp-height height) 2)))))
