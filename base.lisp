(in-package :lisp-magick-wand)

(defgeneric %error-condition (value type)
  (:documentation "return code for checking if VALUE of type TYPE indicates an error"))
(defgeneric %error-signalling-code (wand type)
  (:documentation "return code for signaling an error reported by WAND of type TYPE"))
(defgeneric %special-argument-handling (name key &rest args)
  (:documentation "perform special argument handling for defmagickfun"))
(defgeneric %special-retval-handling (name key &rest args)
  (:documentation "perform special return value handling for defmagickfun"))

(defun %magick-lisp-name (c-name)
  ;; Trim the Magick- prefix.
  (when (= (mismatch "Magick" c-name) 6)
    (setf c-name (subseq c-name 6)))
  (intern
   (with-output-to-string (s)
     (loop for lcase = nil then case
           for ch across c-name
           for case = (if (upper-case-p ch) :upper :lower)
           do (progn
                (when (and (eql lcase :lower) (eql case :upper))
                  (write-char #\- s))
                (write-char (char-upcase ch) s))))))

(defun %internal-fn-name (sym &optional (prefix "%C%"))
  (intern (concatenate 'string prefix (symbol-name sym)) (symbol-package sym)))

(defun %add-outer-wrapper (fn)
  (declare (special *outer-wrap-code*))
  (setf *outer-wrap-code*
        (let ((old *outer-wrap-code*))
          #'(lambda (code)
              (funcall old (funcall fn code))))))

(defun %add-inner-wrapper (fn)
  (declare (special *inner-wrap-code*))
  (setf *inner-wrap-code*
        (let ((old *inner-wrap-code*))
          #'(lambda (code)
              (funcall old (funcall fn code))))))

(defun %set-return-type (type)
  (declare (special *ret-type*))
  (setf *ret-type* type))

(defun %add-result-value (form)
  (declare (special *result-values*))
  (push form *result-values*))

(defun %add-defc-param (form)
  (declare (special *defc-params*))
  (push form *defc-params*))

(defun %add-lisp-param (name)
  (declare (special *lisp-params*))
  (push name *lisp-params*))

(defun %add-inv-param (form)
  (declare (special *inv-params*))
  (push form *inv-params*))

(defun %get-state (key1 key2)
  (declare (special *state*))
  (getf (getf *state* key1) key2))

(defun (setf %get-state) (val key1 key2)
  (declare (special *state*))
  (setf (getf (getf *state* key1) key2) val))

(defmethod %special-argument-handling (name (key (eql :out)) &rest args)
  (destructuring-bind (real-type) args
    (let ((g (gensym)))
      (%add-outer-wrapper
       #'(lambda (code)
           `(cffi:with-foreign-object (,g ',real-type)
              ,code)))
      (%add-result-value `(cffi:mem-ref ,g ',real-type))
      (%add-defc-param (list name :pointer))
      (%add-inv-param g))))

#+sbcl
(defmethod %special-argument-handling (name (key (eql :dynarray)) &rest rest)
  (declare (ignorable rest))
  (let ((g-len (or (%get-state :dynarray-length name)
                   (setf (%get-state :dynarray-length name) (gensym))))
        (g-mem (gensym)))
    (%add-outer-wrapper
     #'(lambda (code)
         `(let ((,g-len (length ,name)))
            (unwind-protect
                 (cffi:with-pointer-to-vector-data (,g-mem (coerce ,name 'vector))
                   ,code)))))
    (%add-defc-param (list name :pointer))
    (%add-lisp-param name)
    (%add-inv-param g-mem)))

#-sbcl
(defmethod %special-argument-handling (name (key (eql :dynarray)) &rest args)
  (destructuring-bind (el-type) args
    (let ((g-len (or (%get-state :dynarray-length name)
                     (setf (%get-state :dynarray-length name) (gensym))))
          (g-mem (gensym)) (g-vals (gensym)) (g-params (gensym)) (g-idx (gensym)) (g-fv (gensym))
          (g-p (gensym)) (g-i (gensym))
          (real-type (cffi::canonicalize-foreign-type el-type)))
      (%add-outer-wrapper
       #'(lambda (code)
           `(let ((,g-len (length ,name))
                  (,g-vals nil) (,g-params nil)
                  (,g-mem (cffi:null-pointer)))
              (unwind-protect
                   (progn
                     (etypecase ,name
                       (null nil)
                       (vector
                        (setf ,g-mem (cffi:foreign-alloc ',real-type :count ,g-len))
                        (do ((,g-idx 0 (1+ ,g-idx)))
                            ((>= ,g-idx ,g-len))
                          (multiple-value-bind (,g-fv ,g-p)
                              (cffi:convert-to-foreign (aref ,name ,g-idx) ',el-type)
                            (setf (cffi:mem-aref ,g-mem ',real-type ,g-idx) ,g-fv)
                            (push ,g-fv ,g-vals)
                            (push ,g-p ,g-params))))
                       (list
                        (setf ,g-mem (cffi:foreign-alloc ',real-type :count ,g-len))
                        (do ((,g-idx 0 (1+ ,g-idx))
                             (,g-i ,name (cdr ,g-i)))
                            ((>= ,g-idx ,g-len))
                          (multiple-value-bind (,g-fv ,g-p)
                              (cffi:convert-to-foreign (car ,g-i) ',el-type)
                            (setf (cffi:mem-aref ,g-mem ',real-type ,g-idx) ,g-fv)
                            (push ,g-fv ,g-vals)
                            (push ,g-p ,g-params)))))
                     ,code)
                (unless (cffi:null-pointer-p ,g-mem)
                  (mapc #'(lambda (,g-fv ,g-p)
                            (cffi:free-converted-object ,g-fv ',el-type ,g-p))
                        ,g-vals ,g-params)
                  (cffi:foreign-free ,g-mem))))))
      (%add-defc-param (list name :pointer))
      (%add-lisp-param name)
      (%add-inv-param g-mem))))

(defmethod %special-argument-handling (name (key (eql :dynarray-length)) &rest args)
  (destructuring-bind (type seq-name &key expr) args
    (let ((g-len (or (%get-state :dynarray-length seq-name)
                     (setf (%get-state :dynarray-length seq-name) (gensym)))))
      (format t "length ~a~%" g-len)
      (%add-defc-param (list name type))
      (%add-inv-param (if expr
                          (subst g-len :l expr)
                          g-len)))))

(defmethod %special-argument-handling (name (key (eql :dynarray-ret-length)) &rest args)
  (destructuring-bind (type) args
    (let ((g (gensym)))
      (%add-outer-wrapper
       #'(lambda (code)
           `(cffi:with-foreign-object (,g ',type)
              ,code)))
      (%add-defc-param (list name :pointer))
      (%add-inv-param g)
      (setf (%get-state :dynarray-ret-length :name) g
            (%get-state :dynarray-ret-length :type) type))))

(defmethod %special-retval-handling (name (key (eql :dynarray)) &rest args)
  (destructuring-bind (type &key (err-val :error) (seq-type 'list)) args
    (unless (constantp err-val)
      (error "error value must be constant"))
    (let ((g-len (or (%get-state :dynarray-ret-length :name)
                     (error ":dynarray return type needs a :dynarray-ret-length parameter")))
          (len-type (or (%get-state :dynarray-ret-length :type)
                        (error ":dynarray return type needs a :dynarray-ret-length parameter")))
          (g-i (gensym)))
      (%add-inner-wrapper
       #'(lambda (code)
           `(let ((,name (if (cffi:null-pointer-p ,name)
                             ,err-val
                             ,(cond
                                ((eql seq-type 'list)
                                 `(loop for ,g-i from 0 below (cffi:mem-ref ,g-len ',len-type)
                                        collecting (cffi:mem-aref ,name ',type ,g-i)
                                        finally (relinquish-memory ,name)))
                                ((or (eql seq-type 'vector)
                                     (and (listp seq-type)
                                          (eql (first seq-type) 'vector)))
                                 (let ((eltype (if (listp seq-type) (second seq-type) t))
                                       (g-l (gensym))
                                       (g-seq (gensym)))
                                   `(let* ((,g-l (cffi:mem-ref ,g-len ',len-type))
                                           (,g-seq (make-array ,g-l :element-type ',eltype)))
                                      (dotimes (,g-i ,g-l)
                                        (setf (aref ,g-seq ,g-i) (cffi:mem-aref ,name ',type ,g-i)))
                                      (relinquish-memory ,name)
                                      ,g-seq)))
                                (t (error "unsupported sequence type: ~s" seq-type))))))
              ,code)))
      (%set-return-type :pointer)
      (setf (%get-state :dynarray-ret-length :err-val) err-val))))

(defmethod %error-condition (value (type (eql :dynarray)))
  `(eql ,value ,(%get-state :dynarray-ret-length :err-val)))

(defmethod %special-retval-handling (name (key (eql :array)) &rest args)
  (destructuring-bind (type len &key (err-val :error) (free-array-p t)) args
    (unless (or (null len) (integerp len))
      (error "length must be an integer or NIL"))
    (unless (constantp err-val)
      (error "error value must be constant"))
    (let ((g-i (gensym)))
      (%add-inner-wrapper
       #'(lambda (code)
           `(let ((,name (if (cffi:null-pointer-p ,name)
                             ,err-val
                             (loop for ,g-i from 0
                                   ,@(if len
                                         `(below ,len)
                                         `(until (cffi:null-pointer-p (cffi:mem-aref ,name :pointer ,g-i))))
                                   collecting (cffi:mem-aref ,name ',type ,g-i)
                                   ,@(if free-array-p
                                         `(finally (relinquish-memory ,name)))))))
              ,code)))
      (%set-return-type :pointer)
      (setf (%get-state :ret-array :err-val) err-val))))

(defmethod %error-condition (value (type (eql :array)))
  `(eql ,value ,(%get-state :ret-array :err-val)))

(defmacro defmagickfun (c-name ret-type args &key check-error (export-p t))
  (let ((lisp-name (%magick-lisp-name c-name))
        (simple-p (not check-error))
        (error-type (and check-error (second (assoc check-error args))))
        (retval (gensym))
        internal-name
        (*outer-wrap-code* #'(lambda (code) code))
        (*inner-wrap-code* #'(lambda (code) code))
        (*ret-type* ret-type)
        *result-values* *defc-params* *lisp-params* *inv-params* *state*)
    (declare (special *outer-wrap-code* *inner-wrap-code* *ret-type* *result-values*
                      *defc-params* *lisp-params* *inv-params* *state*))
    (unless (or (eql ret-type :void) (and (eql ret-type :boolean)
                                          check-error))
      (push retval *result-values*))
    (dolist (arg args)
      (destructuring-bind (name type) arg
        (cond
          ((consp type)
           (setf simple-p nil)
           (apply #'%special-argument-handling name type))
          (t
           (push arg *defc-params*)
           (push name *lisp-params*)
           (push name *inv-params*)))))

    (when (consp ret-type)
      (setf simple-p nil)
      (apply #'%special-retval-handling retval ret-type))

    (unless simple-p
      (setf internal-name (%internal-fn-name lisp-name)))

    `(progn
       (cffi:defcfun (,c-name ,(or internal-name lisp-name)) ,*ret-type* ,@(reverse *defc-params*))
       ,@ (unless simple-p
            `((defun ,lisp-name ,(reverse *lisp-params*)
                ,(funcall *outer-wrap-code*
                          `(let ((,retval (,internal-name ,@(reverse *inv-params*))))
                             ,(funcall *inner-wrap-code*
                                       `(progn
                                          ,@ (when check-error
                                               `((when ,(%error-condition retval (if (consp ret-type) (car ret-type) ret-type))
                                                   ,(%error-signalling-code check-error error-type))))
                                          (values ,@(reverse *result-values*)))))))))
       ,@ (when export-p `((export ',lisp-name))))))


(cffi:define-foreign-library lib-magick-wand
  (:darwin "libMagickWand.dylib")
  (:unix (:or "libMagickWand-7.Q16.so" "libMagickWand-7.Q16.so.2" "libMagickWand-7.Q16.so.3" "libMagickWand-7.Q16.so.4"
              "libMagickWand-7.Q16.so.5" "libMagickWand-7.Q16.so.6" "libMagickWand-7.Q16.so.7" "libMagickWand-7.Q16.so.8"
              "libMagickWand-7.Q16HDRI.so" "libMagickWand-7.Q16HDRI.so.2" "libMagickWand-7.Q16HDRI.so.3" "libMagickWand-7.Q16HDRI.so.4"
              "libMagickWand-7.Q16HDRI.so.5" "libMagickWand-7.Q16HDRI.so.6" "libMagickWand-7.Q16HDRI.so.7" "libMagickWand-7.Q16HDRI.so.8"
              "libMagickWand-6.Q16.so" "libMagickWand-6.Q16.so.2" "libMagickWand-6.Q16.so.3" "libMagickWand-6.Q16.so.4"
              "libMagickWand-6.Q16.so.5" "libMagickWand-6.Q16.so.6" "libMagickWand-6.Q16.so.7" "libMagickWand-6.Q16.so.8"
              "libMagickWand-6.Q16HDRI.so" "libMagickWand-6.Q16HDRI.so.2" "libMagickWand-6.Q16HDRI.so.3" "libMagickWand-6.Q16HDRI.so.4"
              "libMagickWand-6.Q16HDRI.so.5" "libMagickWand-6.Q16HDRI.so.6" "libMagickWand-6.Q16HDRI.so.7" "libMagickWand-6.Q16HDRI.so.8"
              "libMagickWand.so" "libWand.so.9" "libWand.so"))
  (t (:default "libWand")))
(cffi:use-foreign-library lib-magick-wand)

(defun type-name-to-class-name (name)
  (intern (concatenate 'string (symbol-name name) "-TYPE-CLASS")
          (symbol-package name)))
