(in-package :lisp-magick-wand)

(defmagickfun "MagickRelinquishMemory" :pointer ((ptr :pointer)))

(defmacro defmagicktype (name base-type)
  `(cffi:define-foreign-type ,(type-name-to-class-name name) () ()
     (:actual-type ,base-type)
     (:simple-parser ,name)))

(defmacro defmagicktrans (method-name (value-arg (type-arg type-name)
                                       &rest other-args)
                          &body body)
  `(defmethod ,method-name (,value-arg (,type-arg ,(type-name-to-class-name type-name))
                            ,@other-args)
     ,@body))


;; size_t

(defmagicktype size-t :uint)
(defmagicktrans cffi:expand-to-foreign (value (type size-t))     value)
(defmagicktrans cffi:expand-from-foreign (value (type size-t))   value)


;; magick-double

(defmagicktype magick-double :double)
(defmagicktrans cffi:expand-to-foreign (value (type magick-double))
  `(coerce ,value 'double-float))
(defmagicktrans cffi:expand-from-foreign (value (type magick-double))
  value)
(defmagicktrans cffi:translate-to-foreign (value (type magick-double))
  (values (coerce value 'double-float) nil))


;; Quantum

(declaim (inline byte->quantum quantum->byte))

#+lisp-magick-wand:quantum-8
(progn
  (defmagicktype quantum :uint8)
  (defun byte->quantum (b) b)
  (defun quantum->byte (b) b))

#+lisp-magick-wand:quantum-16
(progn
  (defmagicktype quantum :uint16)
  (defun byte->quantum (b) (* b 257))
  (defun quantum->byte (b) (values (truncate b 257))))

#+lisp-magick-wand:quantum-32
(progn
  (defmagicktype quantum :uint32)
  (defun byte->quantum (b) (* b 16843009))
  (defun quantum->byte (b) (values (truncate b 16843009))))

#+(and lisp-magick-wand:quantum-64 cffi-features:no-long-long)
(error "your version of imagemagick uses a quantum size of 64bit,
but cffi doesn't support long long on your lisp implementation.")

#+(and lisp-magick-wand:quantum-64 (not cffi-features:no-long-long))
(progn
  (defmagicktype quantum :uint64)
  (defun byte->quantum (b) (* b 72340172838076673))
  (defun quantum->byte (b) (values (truncate b 72340172838076673))))

#-(or lisp-magick-wand:quantum-8 lisp-magick-wand:quantum-16
      lisp-magick-wand:quantum-32 lisp-magick-wand:quantum-64)
(error "quantum size feature not defined")

(defmagicktrans cffi:expand-to-foreign (value (type quantum))     value)
(defmagicktrans cffi:expand-from-foreign (value (type quantum))   value)


;; Boolean

(defmethod %error-condition (value (type (eql :boolean)))
  `(not ,value))

;; String Types

;; this is cffi:defctype (and not defmagicktype) on purpose
;; - we want to inherit :string's translators
(cffi:defctype magick-string :string)
(defmethod %error-condition (value (type (eql 'magick-string)))
  `(null ,value))

(defmagicktype magick-string/free :pointer)
(defmagicktrans cffi:translate-from-foreign (value (type magick-string/free))
  (prog1
      (cffi:foreign-string-to-lisp value)
    (unless (cffi:null-pointer-p value)
      (relinquish-memory value))))
(defmagicktrans cffi:translate-to-foreign (value (type magick-string/free))
  (values (cffi:foreign-string-alloc value) t))
(defmagicktrans cffi:free-translated-object (value (type magick-string/free) free-p)
  (when free-p
    (cffi:foreign-string-free value)))
(defmagicktrans cffi:expand-from-foreign (value (type magick-string/free))
  (let ((g (gensym)))
    `(let ((,g ,value))
      (prog1
          (cffi:foreign-string-to-lisp ,g)
        (unless (cffi:null-pointer-p ,g)
          (relinquish-memory ,g))))))

(defmethod %error-condition (value (type (eql 'magick-string/free)))
  `(null ,value))

;; MagickWand

(defmagicktype magick-wand :pointer)
(defmagicktrans cffi:expand-to-foreign (value (type magick-wand))     value)
(defmagicktrans cffi:expand-from-foreign (value (type magick-wand))   value)
(defmethod %error-condition (value (type (eql 'magick-wand)))
  `(cffi:null-pointer-p ,value))
(defmethod %error-signalling-code (wand (type (eql 'magick-wand)))
  `(signal-magick-wand-error ,wand))


;; PixelWand

(defmagicktype pixel-wand :pointer)
(defmagicktrans cffi:expand-to-foreign (value (type pixel-wand))      value)
(defmagicktrans cffi:expand-from-foreign (value (type pixel-wand))    value)
(defmethod %error-condition (value (type (eql 'pixel-wand)))
  `(cffi:null-pointer-p ,value))
(defmethod %error-signalling-code (wand (type (eql 'pixel-wand)))
  `(signal-pixel-wand-error ,wand))


;; DrawingWand

(defmagicktype drawing-wand :pointer)
(defmagicktrans cffi:expand-to-foreign (value (type drawing-wand))    value)
(defmagicktrans cffi:expand-from-foreign (value (type drawing-wand))  value)
(defmethod %error-condition (value (type (eql 'drawing-wand)))
  `(cffi:null-pointer-p ,value))
(defmethod %error-signalling-code (wand (type (eql 'drawing-wand)))
  `(signal-drawing-wand-error ,wand))
