(in-package :lisp-magick-wand)

(cffi:defcstruct option-info
  (mnemonic magick-string)
  (type :long))

(defmagickfun "GetCommandOptions" (:array magick-string/free nil :err-val nil)
  ((table :int)))
(defmagickfun "ParseCommandOption" :long
  ((table :int) (list-p :boolean) (options magick-string)))

(defparameter *list-options*
  (loop for i from 0 upto 100
     for opts = (get-command-options i)
     when (and (member "List" opts :test #'equal)
               (member "Orientation" opts :test #'equal))
     return i
     finally (error "Cannot find option list using GetMagickOptions")))

(defun magick-name-to-lisp-name (magick-name package)
  (intern
   (coerce
    (loop for prev = nil then ch
       for ch across magick-name
       when (or (and prev (not (upper-case-p prev))
                     (upper-case-p ch))
                (and prev (not (digit-char-p prev))
                     (digit-char-p ch)))
       collect #\-
       collect (char-upcase ch))
    'string)
   package))

(defun find-magick-option (table options)
  (dolist (opt (if (listp options) options (list options)))
    (let ((n (parse-command-option table nil opt)))
      (unless (eql n -1)
        (return n)))))

(defmacro define-enum-from-options (option-name lisp-name)
  (let ((id (or (find-magick-option *list-options* option-name)
                (error "cannot get info about ~s" option-name))))
    (let ((names (remove-if #'(lambda (s) (and (eql (length s) 1)
                                               (digit-char-p (char s 0))))
                            (remove-duplicates (get-command-options id) :test #'equal))))
      (unless names
        (error "unable to get option names for ~s/~s" option-name id))
      `(cffi:defcenum ,lisp-name
         ,@(loop for name in names
                collect (list (magick-name-to-lisp-name name :keyword)
                              (parse-command-option id nil name)))))))
