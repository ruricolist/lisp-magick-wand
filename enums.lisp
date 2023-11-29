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

(cffi:defcenum exception-type
  (:undefined-exception 0)
  (:resource-limit-warning 300)
  (:type-warning 305)
  (:option-warning 310)
  (:delegate-warning 315)
  (:missing-delegate-warning 320)
  (:corrupt-image-warning 325)
  (:file-open-warning 330)
  (:blob-warning 335)
  (:stream-warning 340)
  (:cache-warning 345)
  (:coder-warning 350)
  (:module-warning 355)
  (:draw-warning 360)
  (:image-warning 365)
  (:wand-warning 370)
  (:xserver-warning 380)
  (:monitor-warning 385)
  (:registry-warning 390)
  (:configure-warning 395)
  (:resource-limit-error 400)
  (:type-error 405)
  (:option-error 410)
  (:delegate-error 415)
  (:missing-delegate-error 420)
  (:corrupt-image-error 425)
  (:file-open-error 430)
  (:blob-error 435)
  (:stream-error 440)
  (:cache-error 445)
  (:coder-error 450)
  (:module-error 455)
  (:draw-error 460)
  (:image-error 465)
  (:wand-error 470)
  (:xserver-error 480)
  (:monitor-error 485)
  (:registry-error 490)
  (:configure-error 495)
  (:resource-limit-fatal-error 700)
  (:type-fatal-error 705)
  (:option-fatal-error 710)
  (:delegate-fatal-error 715)
  (:missing-delegate-fatal-error 720)
  (:corrupt-image-fatal-error 725)
  (:file-open-fatal-error 730)
  (:blob-fatal-error 735)
  (:stream-fatal-error 740)
  (:cache-fatal-error 745)
  (:coder-fatal-error 750)
  (:module-fatal-error 755)
  (:draw-fatal-error 760)
  (:image-fatal-error 765)
  (:wand-fatal-error 770)
  (:xserver-fatal-error 780)
  (:monitor-fatal-error 785)
  (:registry-fatal-error 790)
  (:configure-fatal-error 795))

(define-enum-from-options ("Compression" "Compress") compression-type)
(define-enum-from-options "Interlace" interlace-type)
(define-enum-from-options ("Image" "Type") image-type)
(define-enum-from-options "Gravity" gravity-type)
(define-enum-from-options ("Composite" "Compose") composite-operator)
(define-enum-from-options "Colorspace" colorspace-type)
(define-enum-from-options "Dispose" dispose-type)
(define-enum-from-options "Filter" filter-type)
(define-enum-from-options "Resource" resource-type)
(define-enum-from-options "Stretch" stretch-type)
(define-enum-from-options "Style" style-type)
(define-enum-from-options "Storage" storage-type)
(define-enum-from-options "Align" align-type)
(define-enum-from-options "ClipPath" clip-path-units)
(define-enum-from-options "Decoration" decoration-type)
(define-enum-from-options "FillRule" fill-rule)
(define-enum-from-options "LineCap" line-cap)
(define-enum-from-options "LineJoin" line-join)
(define-enum-from-options "Method" paint-method)
(define-enum-from-options "Noise" noise-type)
(define-enum-from-options "Evaluate" magick-evaluate-operator)
(define-enum-from-options "Metric" metric-type)
(define-enum-from-options "Distort" distort-method)
(define-enum-from-options "Orientation" orientation-type)
(define-enum-from-options "Interpolate" pixel-interpolate-method)
(define-enum-from-options "AutoThreshold" auto-threshold-method)
(define-enum-from-options "Morphology" morphology-method)
(define-enum-from-options "Dither" dither-method)
(define-enum-from-options "SparseColor" sparse-color-method)
(define-enum-from-options "Statistic" statistic-type)
(define-enum-from-options "Endian" endian-type)
(define-enum-from-options "Intent" rendering-intent)
(define-enum-from-options "VirtualPixel" virtual-pixel-method)
(define-enum-from-options "Alpha" alpha-channel-type)


(cffi:defbitfield channel-type
  ;;  (:undefined  #x00)
  (:red        #x01)
  ;;  (:gray       #x01)
  ;;  (:cyan       #x01)
  (:green      #x02)
  ;;  (:magenta    #x02)
  (:blue       #x04)
  ;;  (:yellow     #x04)
  (:alpha      #x08)
  ;;  (:opacity    #x08)
  ;;  (:matte      #x08)			; deprecated
  ;;  (:black      #x20)
  (:index      #x20)
  (:all        #xff))
