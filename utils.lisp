;;;; ImageMagick binding for Common Lisp
;;;; Copyright (c) 2006, 2007, 2008, 2009  Hans Bulfone <hans@nil.at>
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright notice,
;;;;       this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;     * Neither the name of the author nor the names of his contributors may
;;;;       be used to endorse or promote products derived from this software
;;;;       without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :lisp-magick)

;;; Pixel Wand Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric %init-pixel-wand (wand type args))

(defmethod %init-pixel-wand (wand (type (eql :string)) args)
  (destructuring-bind (color) args
    `((pixel-set-color ,wand ,color))))

(defmethod %init-pixel-wand (wand (type (eql :components)) args)
  (destructuring-bind ((r g b &optional a) &key (format :rgb) (type :byte)) args
    (unless (or (eql format :rgb) (eql format :rgba))
      (error "invalid format ~a" format))
    (ecase type
      (:double
       `((pixel-set-red ,wand ,r)
	 (pixel-set-green ,wand ,g)
	 (pixel-set-blue ,wand ,b)
	 ,@(when a `((pixel-set-alpha ,wand ,a)))))
      (:quantum
       `((pixel-set-red-quantum ,wand ,r)
	 (pixel-set-green-quantum ,wand ,g)
	 (pixel-set-blue-quantum ,wand ,b)
	 ,@(when a `((pixel-set-alpha-quantum ,wand ,a)))))
      (:byte
       `((pixel-set-red-quantum ,wand (byte->quantum ,r))
	 (pixel-set-green-quantum ,wand (byte->quantum ,g))
	 (pixel-set-blue-quantum ,wand (byte->quantum ,b))
	 ,@(when a `((pixel-set-alpha-quantum ,wand (byte->quantum ,a)))))))))

(defmethod %init-pixel-wand (wand (type (eql :comp)) args)
  (%init-pixel-wand wand :components args))

(defmethod %init-pixel-wand (wand (type (eql :vector)) args)
  (destructuring-bind (v &key (format :rgb) (type :byte)) args
    (unless (or (eql format :rgb) (eql format :rgba))
      (error "invalid format ~a" format))
    (let ((g (gensym)))
      `((let ((,g ,v))
	  ,@ (ecase type
	       (:double
		`((pixel-set-red ,wand (svref ,g 0))
		  (pixel-set-green ,wand (svref ,g 1))
		  (pixel-set-blue ,wand (svref ,g 2))
		  ,@(when (eql format :rgba) `((pixel-set-alpha ,wand (svref ,g 3))))))
	       (:quantum
		`((pixel-set-red-quantum ,wand (svref ,g 0))
		  (pixel-set-green-quantum ,wand (svref ,g 1))
		  (pixel-set-blue-quantum ,wand (svref ,g 2))
		  ,@(when (eql format :rgba) `((pixel-set-alpha-quantum ,wand (svref ,g 3))))))
	       (:byte
		`((pixel-set-red-quantum ,wand (byte->quantum (svref ,g 0)))
		  (pixel-set-green-quantum ,wand (byte->quantum (svref ,g 1)))
		  (pixel-set-blue-quantum ,wand (byte->quantum (svref ,g 2)))
		  ,@(when (eql format :rgba) `((pixel-set-alpha-quantum ,wand (byte->quantum (svref ,g 3)))))))))))))

(defmacro with-pixel-wand ((var &optional type &rest args) &body body)
  `(let ((,var (new-pixel-wand)))
    (unwind-protect
	 (progn
	   ,@(when type (%init-pixel-wand var type args))
	   ,@body)
      (destroy-pixel-wand ,var))))


;;; Drawing Wand Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-drawing-wand ((var) &body body)
  `(let ((,var (new-drawing-wand)))
    (unwind-protect
	 (progn ,@body)
      (destroy-drawing-wand ,var))))


;;; Magick Wand Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric %create-magick-wand (wand init args))
(defgeneric %init-magick-wand (wand init args))

(defmethod %create-magick-wand (wand init args)
  `(new-magick-wand))

(defmethod %init-magick-wand (wand (init (eql :create)) args)
  (destructuring-bind (width height &rest color) args
    (if (or (null color) (keywordp (car color)))
	(let ((pw (gensym)))
	  `((with-pixel-wand (,pw ,@color)
	      (magick-new-image ,wand ,width ,height ,pw))))
	`((magick-new-image ,wand ,width ,height ,(car color))))))

(defmethod %init-magick-wand (wand (init (eql :load)) args)
  (destructuring-bind (filename) args
    `((magick-read-image ,wand ,filename))))

(defmethod %init-magick-wand (wand (init (eql :from)) args)
  nil)

(defmethod %create-magick-wand (wand (init (eql :from)) args)
  `(progn ,@args))

(defmacro with-magick-wand ((var &optional init &rest args) &body body)
  "execute the body with var bound to a wand.
the wand can optionally be initialized with an image loaded
from disk:

  (with-magick-wand (wand :load filename) body...)

or with a newly created image with the given size and color:

  (with-magick-wand (wand :create w h :components (0 0 0)) body...)"

  `(let ((,var ,(%create-magick-wand var init args)))
    (unwind-protect
	 (progn
	   ,@(when init (%init-magick-wand var init args))
	   ,@body)
      (when ,var
	(destroy-magick-wand ,var)))))

(defmacro give-wand (var)
  `(prog1 ,var (setf ,var nil)))

;;; Manipulating pixel data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pixel-data ()
  ((data   :initarg :data   :reader pd-data)
   (width  :initarg :width  :reader pd-width)
   (height :initarg :height :reader pd-height))
  (:documentation "a wrapper around a foreign object containing raw pixel data"))

(defmacro with-pixel-data ((var wand) &body body)
  (let ((g-wand (gensym)) (g-pd (gensym)) (g-w (gensym)) (g-h (gensym)))
    `(let* ((,g-wand ,wand)
	    (,g-w (magick-get-image-width ,g-wand))
	    (,g-h (magick-get-image-height ,g-wand))
	    (,g-pd (cffi:foreign-alloc :uchar :count (* ,g-w ,g-h 4))))
      (unwind-protect
	   (progn
	     (magick-get-image-pixels ,g-wand 0 0 ,g-w ,g-h "RGBA" :char ,g-pd)
	     (prog1
		 (let ((,var (make-instance 'pixel-data :data ,g-pd :width ,g-w :height ,g-h)))
		   ,@body)
	       (magick-set-image-pixels ,g-wand 0 0 ,g-w ,g-h "RGBA" :char ,g-pd)))
	(cffi:foreign-free ,g-pd)))))

(defun pixel (pd x y)
  (let ((data (pd-data pd))
	(i (+ (* (pd-width pd) y 4) (* x 4))))
    (vector
     (cffi:mem-ref data :uchar i)
     (cffi:mem-ref data :uchar (+ i 1))
     (cffi:mem-ref data :uchar (+ i 2))
     (cffi:mem-ref data :uchar (+ i 3)))))

(defun (setf pixel) (color pd x y)
  (let ((data (pd-data pd))
	(i (+ (* (pd-width pd) y 4) (* x 4))))
    (setf (cffi:mem-ref data :uchar i)       (svref color 0)
	  (cffi:mem-ref data :uchar (+ i 1)) (svref color 1)
	  (cffi:mem-ref data :uchar (+ i 2)) (svref color 2)
	  (cffi:mem-ref data :uchar (+ i 3)) (svref color 3))))

(defun get-pixel (pd x y)
  (let ((data (pd-data pd))
	(i (+ (* (pd-width pd) y 4) (* x 4))))
    (values
     (cffi:mem-ref data :uchar i)
     (cffi:mem-ref data :uchar (+ i 1))
     (cffi:mem-ref data :uchar (+ i 2))
     (cffi:mem-ref data :uchar (+ i 3)))))

(defun set-pixel (pd x y r g b a)
  (let ((data (pd-data pd))
	(i (+ (* (pd-width pd) y 4) (* x 4))))
    (setf (cffi:mem-ref data :uchar i)       r
	  (cffi:mem-ref data :uchar (+ i 1)) g
	  (cffi:mem-ref data :uchar (+ i 2)) b
	  (cffi:mem-ref data :uchar (+ i 3)) a)))
