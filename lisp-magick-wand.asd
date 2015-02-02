;;;; -*- Mode: lisp; -*-
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

(defpackage :lisp-magick-system
  (:use :cl :asdf))
(in-package :lisp-magick-system)

(defsystem lisp-magick-wand
  :name "lisp-magick-wand"
  :author "Hans Bulfone"
  :licence "BSD"
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "ImageMagick binding"
  :serial t
  :components
  ((:file "packages")
   (:file "base")
   (:file "quantum")
   (:file "types")
   (:file "option")
   (:file "enums")
   (:file "magick")
   (:file "utils"))
  :depends-on (#:cffi #:alexandria))
