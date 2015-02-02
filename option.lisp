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
