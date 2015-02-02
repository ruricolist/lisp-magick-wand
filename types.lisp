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
