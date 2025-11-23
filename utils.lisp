(in-package :lisp-magick-wand)

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


;;; Pixel Iterator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cannot be a macrolet within with-pixel-iterator
(defmacro %check-pixel-iter-error (var)
  `(when ,(%error-condition var 'pixel-iterator)
     ,(%error-signalling-code var 'pixel-iterator)))

(defmacro with-pixel-iterator ((var wand) &body body)
  (let ((wand-var (gensym "W")))
    `(let* ((,wand-var ,wand)
	    (,var (new-pixel-iterator ,wand-var)))
       ,(macroexpand-1 `(%check-pixel-iter-error ,wand-var))
       (unwind-protect
	    (progn
	      ,@body)
	 (destroy-pixel-iterator ,var)))))

;; pattern from alexandria bind
(defmacro with-pixel-iterator* (bindings &body body)
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(with-pixel-iterator ,(car bindings)
		      ,(bind (cdr bindings) body))
                   `(progn ,@body))))
      (bind binding-list body))))


(defun map-pixels (function magick-wand &rest more-magick-wands)
  "Repeatedly call FUNCTION on `pixel-wands' corresponding to each pixel
position of the supplied `image-wand's. The image-wands are assumed to
be of the same dimensions."
  (let* ((image-wands (cons magick-wand more-magick-wands)) (n (length image-wands))
	 (iters (make-list n)) iter (pws-list (make-list n)))
    (unwind-protect
	 (block outer
	   (loop for i below n
		 do (setq iter (setf (elt iters i) (new-pixel-iterator (elt image-wands i))))
		 if (cffi:null-pointer-p iter)
		 do (signal-pixel-iterator-error (elt image-wands i)))
	   (loop for y below (get-image-height (car image-wands))
		 with width
		 do (loop for i below n
			  do (multiple-value-bind (pixels-array width2)
				 (pixel-get-next-iterator-row (elt iters i))
			       (if (cffi:null-pointer-p pixels-array)
				   (return-from outer))
			       (if width
				   (with-simple-restart (cont "Cont")
				     (assert (= width width2)))
				   (setq width width2))
			       (setf (elt pws-list i) pixels-array)))
		 do (loop for x below width
			  for args = (mapcar (lambda (a)
					       (cffi:mem-aref a 'pixel-wand x))
					     pws-list)
			  do (let ((x x) (y y))
			       (declare (special x y))
			       (apply function args)))))
      (loop for i below n
	    when (and (setq iter (elt iters i))
		      (not (cffi:null-pointer-p iter)))
	    do (destroy-pixel-iterator iter)))))


;;; Drawing Wand Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-drawing-wand ((var) &body body)
  `(let ((,var (new-drawing-wand)))
    (unwind-protect
         (progn ,@body)
      (destroy-drawing-wand ,var))))

(defmacro with-cloned-drawing-wand ((var orig-wand) &body body)
  `(let ((,var (clone-drawing-wand ,orig-wand)))
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
              (new-image ,wand ,width ,height ,pw))))
        `((new-image ,wand ,width ,height ,(car color))))))

(defmethod %init-magick-wand (wand (init (eql :load)) args)
  (destructuring-bind (filename &key jpeg-size) args
    `(,@(when jpeg-size
          (list
           (alexandria:once-only (jpeg-size)
             `(set-option ,wand "jpeg:size"
                          (format nil "~dx~d"
                                  (car ,jpeg-size)
                                  (cdr ,jpeg-size))))))
      (read-image ,wand (namestring (truename ,filename))))))

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

  (with-magick-wand (wand :create w h :components (0 0 0)) body...)

When loading a JPEG image, you may want to specify the size for
libjpeg (as a cons):

  (with-magick-wand (wand :load filename :jpeg-size '(200 . 200) ...)
"

  `(let ((,var ,(%create-magick-wand var init args)))
    (unwind-protect
         (progn
           ,@(when init (%init-magick-wand var init args))
           ,@body)
      (when ,var
        (destroy-magick-wand ,var)))))

(defmacro give-wand (var)
  `(prog1 ,var (setf ,var nil)))

(defmacro with-cloned-magick-wand ((var orig-wand) &body body)
  `(let ((,var (clone-magick-wand ,orig-wand)))
    (unwind-protect
         (progn ,@body)
      (destroy-magick-wand ,var))))

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
            (,g-w (get-image-width ,g-wand))
            (,g-h (get-image-height ,g-wand))
            (,g-pd (cffi:foreign-alloc :uchar :count (* ,g-w ,g-h 4))))
      (unwind-protect
           (progn
             (export-image-pixels ,g-wand 0 0 ,g-w ,g-h "RGBA" :char ,g-pd)
             (prog1
                 (let ((,var (make-instance 'pixel-data :data ,g-pd :width ,g-w :height ,g-h)))
                   ,@body)
               (import-image-pixels ,g-wand 0 0 ,g-w ,g-h "RGBA" :char ,g-pd)))
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
