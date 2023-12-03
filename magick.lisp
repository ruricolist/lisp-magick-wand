(in-package :lisp-magick-wand)

;;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition magick-wand-error (error)
  ((message :initarg :message :reader magick-wand-error-message)
   (type    :initarg :type    :reader magick-wand-error-type))
  (:report (lambda (c s)
             (format s "MagickWand Exception: ~a (~a)"
                     (magick-wand-error-message c)
                     (magick-wand-error-type c))))
  (:documentation "Encapsulates errors reported by magick wand operations."))

(defun signal-magick-wand-error (wand)
  (multiple-value-bind (msg type) (get-exception wand)
    (error 'magick-wand-error :message msg :type type)))

(defun signal-pixel-wand-error (wand)
  (multiple-value-bind (msg type) (pixel-get-exception wand)
    (error 'magick-wand-error :message msg :type type)))

(defun signal-drawing-wand-error (wand)
  (multiple-value-bind (msg type) (draw-get-exception wand)
    (error 'magick-wand-error :message msg :type type)))


(defmagickfun "MagickGetException" magick-string/free ((wand magick-wand)  (exception (:out exception-type))))
(defmagickfun "PixelGetException"  magick-string/free ((wand pixel-wand)   (exception (:out exception-type))))
(defmagickfun "DrawGetException"   magick-string/free ((wand drawing-wand) (exception (:out exception-type))))

(defmagickfun "MagickClearException" :boolean ((wand magick-wand)))
(defmagickfun "PixelClearException"  :boolean ((wand pixel-wand)))
(defmagickfun "DrawClearException"   :boolean ((wand drawing-wand)))


;;; ImageMagick Initialization / Termination / Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmagickfun "MagickWandGenesis" :void ())
(defmagickfun "MagickWandTerminus" :void ())

(defmagickfun "MagickGetHomeURL"           magick-string/free ())
(defmagickfun "MagickQueryConfigureOption" magick-string/free ((option magick-string)))
(defmagickfun "MagickQueryConfigureOptions" (:dynarray magick-string/free :err-val nil)
  ((pattern magick-string) (num (:dynarray-ret-length :ulong))))
(defmagickfun "MagickQueryFonts" (:dynarray magick-string/free :err-val nil)
  ((pattern magick-string) (num (:dynarray-ret-length :ulong))))
(defmagickfun "MagickQueryFormats" (:dynarray magick-string/free :err-val nil)
  ((pattern magick-string) (num (:dynarray-ret-length :ulong))))
(defmagickfun "MagickGetCopyright"         magick-string      ())
(defmagickfun "MagickGetPackageName"       magick-string      ())
(defmagickfun "MagickGetQuantumRange"      magick-string      ((range (:out :ulong))))
(defmagickfun "MagickGetReleaseDate"       magick-string      ())
(defmagickfun "MagickGetVersion"           magick-string      ((version (:out :ulong))))

(defmagickfun "MagickGetResource"      :ulong   ((type resource-type)))
(defmagickfun "MagickGetResourceLimit" :ulong   ((type resource-type)))
(defmagickfun "MagickSetResourceLimit" :boolean ((type resource-type) (limit :ulong)))


;;; Magick Wands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creation, destruction, etc.

(defmagickfun "NewMagickWand"              magick-wand ())
(defmagickfun "DestroyMagickWand"          magick-wand ((wand magick-wand)))
(defmagickfun "IsMagickWand"               :boolean    ((wand magick-wand)))
(defmagickfun "CloneMagickWand"            magick-wand ((wand magick-wand)))
(defmagickfun "ClearMagickWand"            :void       ((wand magick-wand)))
(defmagickfun "MagickCoalesceImages"       magick-wand ((wand magick-wand)))
(defmagickfun "MagickCombineImages"        magick-wand ((wand magick-wand) (colorspace colorspace-type)))
(defmagickfun "MagickCompareImagesLayers"  magick-wand ((wand magick-wand) (method layer-method)))
(defmagickfun "MagickCompareImages"        magick-wand ((wand magick-wand) (metric metric-type) (distortion magick-double)))
(defmagickfun "MagickComplexImages"        magick-wand ((wand magick-wand) (complex complex-operator)))
(defmagickfun "MagickDeconstructImages"    magick-wand ((wand magick-wand)))
(defmagickfun "MagickFxImage"              magick-wand ((wand magick-wand) (expression magick-string)))
(defmagickfun "MagickGetImageMask"         magick-wand ((wand magick-wand) (type pixel-mask)))
(defmagickfun "MagickGetImageRegion"       magick-wand ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long)))
(defmagickfun "MagickMergeImageLayers"     magick-wand ((wand magick-wand) (method layer-method)))
(defmagickfun "MagickMontageImage"         magick-wand ((wand magick-wand) (drawing-wand drawing-wand) (tile-geometry magick-string)
                                                        (thumbnail-geometry magick-string) (mode montage-mode)))
(defmagickfun "MagickMorphImages"          magick-wand ((wand magick-wand) (number-frames :ulong)))
(defmagickfun "MagickOptimizeImageLayers"  magick-wand ((wand magick-wand)))
(defmagickfun "MagickPreviewImages"        magick-wand ((wand magick-wand) (preview preview-type)))
(defmagickfun "MagickSmushImages"          magick-wand ((wand magick-wand) (stack :boolean) (offset :long)))
(defmagickfun "MagickSteganoImage"         magick-wand ((wand magick-wand) (watermark-wand magick-wand) (offset :long)))
(defmagickfun "MagickStereoImage"          magick-wand ((wand magick-wand) (offset-wand magick-wand)))
(defmagickfun "MagickTextureImage"         magick-wand ((wand magick-wand) (texture-wand magick-wand)))


;; Image Iterator

(defmagickfun "MagickResetIterator"    :void    ((wand magick-wand)))
(defmagickfun "MagickSetFirstIterator" :void    ((wand magick-wand)))
(defmagickfun "MagickSetLastIterator"  :void    ((wand magick-wand)))
(defmagickfun "MagickHasNextImage"     :boolean ((wand magick-wand)))
(defmagickfun "MagickHasPreviousImage" :boolean ((wand magick-wand)))
(defmagickfun "MagickNextImage"        :boolean ((wand magick-wand)))
(defmagickfun "MagickPreviousImage"    :boolean ((wand magick-wand)))


;; Attributes

(defmagickfun "MagickGetFilename"        magick-string/free ((wand magick-wand)))
(defmagickfun "MagickGetFormat"          magick-string/free ((wand magick-wand)))
(defmagickfun "MagickGetOption"          magick-string/free ((wand magick-wand) (key magick-string)))
(defmagickfun "MagickGetCompression"     compression-type   ((wand magick-wand)))
(defmagickfun "MagickGetSamplingFactors" (:dynarray :double :err-val nil)
  ((wand magick-wand) (num (:dynarray-ret-length :ulong))))
(defmagickfun "MagickQueryFontMetrics" (:array :double 7)
  ((wand magick-wand) (dwand drawing-wand) (text magick-string))
  :check-error wand)
(defmagickfun "MagickQueryMultilineFontMetrics" (:array :double 7)
  ((wand magick-wand) (dwand drawing-wand) (text magick-string))
  :check-error wand)
(defmagickfun "MagickGetInterlaceScheme" interlace-type     ((wand magick-wand)))
(defmagickfun "MagickGetPage"            :boolean
  ((wand magick-wand) (width (:out :ulong)) (height (:out :ulong)) (x (:out :long)) (y (:out :long)))
  :check-error wand)
(defmagickfun "MagickGetSize"            :boolean
  ((wand magick-wand) (columns (:out :ulong)) (rows (:out :ulong)))
  :check-error wand)
(defmagickfun "MagickGetCompressionQuality" :ulong ((wand magick-wand)))

(defmagickfun "MagickSetBackgroundColor"    :boolean ((wand magick-wand) (background pixel-wand))           :check-error wand)
(defmagickfun "MagickSetCompression"        :boolean ((wand magick-wand) (compression compression-type))    :check-error wand)
(defmagickfun "MagickSetCompressionQuality" :boolean ((wand magick-wand) (quality :ulong))                  :check-error wand)
(defmagickfun "MagickSetFilename"           :boolean ((wand magick-wand) (filename magick-string))          :check-error wand)
(defmagickfun "MagickSetFormat"             :boolean ((wand magick-wand) (format magick-string))            :check-error wand)
(defmagickfun "MagickSetInterlaceScheme"    :boolean ((wand magick-wand) (interlace-scheme interlace-type)) :check-error wand)
(defmagickfun "MagickSetOption"             :boolean ((wand magick-wand) (key magick-string)
                                                      (value magick-string))                                :check-error wand)
(defmagickfun "MagickSetPage"               :boolean ((wand magick-wand) (width :ulong) (height :ulong)
                                                      (x :long) (y :long))                                  :check-error wand)
(defmagickfun "MagickSetPassphrase"         :boolean ((wand magick-wand) (passphrase magick-string))        :check-error wand)
(defmagickfun "MagickSetResolution"         :boolean ((wand magick-wand) (x-resolution magick-double)
                                                      (y-resolution magick-double))                         :check-error wand)
(defmagickfun "MagickSetSamplingFactors"    :boolean ((wand magick-wand) (num (:dynarray-length :ulong factors))
                                                      (factors (:dynarray magick-double)))                  :check-error wand)
(defmagickfun "MagickSetSize"               :boolean ((wand magick-wand) (columns :ulong) (rows :ulong))    :check-error wand)
(defmagickfun "MagickSetType"               :boolean ((wand magick-wand) (type image-type))                 :check-error wand)


;; Image get params
(defmagickfun "MagickGetImageAlphaChannel" :boolean
  ((wand magick-wand)))
(defmagickfun "MagickGetImageBackgroundColor" :boolean
  ((wand magick-wand) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickGetImageBluePrimary" :boolean
  ((wand magick-wand) (x (:out :double)) (y (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageBorderColor" :boolean
  ((wand magick-wand) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickGetImageKurtosis" :boolean
  ((wand magick-wand) (kurtosis (:out :double)) (skewness (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageMean" :boolean
  ((wand magick-wand) (mean (:out :double)) (standard-deviation (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageRange" :boolean
  ((wand magick-wand) (minima (:out :double)) (maxima (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageColormapColor" :boolean
  ((wand magick-wand) (index :ulong) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickGetImageColors" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageColorspace" colorspace-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageCompose" composite-operator
  ((wand magick-wand)))
(defmagickfun "MagickGetImageCompression" compression-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageCompressionQuality" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageDelay" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageDepth" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageDispose" dispose-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageDistortion" :boolean
  ((wand magick-wand) (reference magick-wand)
   (metric metric-type) (distortion (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageDistortions" :double
  ((wand magick-wand) (reference magick-wand)(metric metric-type)))
(defmagickfun "MagickGetImageEndian" endian-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageFilename" magick-string/free
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickGetImageFilter" filter-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageFormat" magick-string/free
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickGetImageFuzz" :double
  ((wand magick-wand)))
(defmagickfun "MagickGetImageGamma" :double
  ((wand magick-wand)))
(defmagickfun "MagickGetImageGravity" gravity-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageGreenPrimary" :boolean
  ((wand magick-wand) (x (:out :double)) (y (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageHeight" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageInterlaceScheme" interlace-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageInterpolateMethod" pixel-interpolate-method
  ((wand magick-wand)))
(defmagickfun "MagickGetImageIterations" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageLength" :boolean
  ((wand magick-wand) (x (:out :ulong)))
  :check-error wand)
(defmagickfun "MagickGetImageMatteColor" :boolean
  ((wand magick-wand) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickGetImageOrientation" orientation-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImagePage" :boolean
  ((wand magick-wand) (width (:out :ulong)) (height (:out :ulong))
   (x (:out :long)) (y (:out :long)))
  :check-error wand)
(defmagickfun "MagickGetImagePixelColor" :boolean
  ((wand magick-wand) (x :long) (y :long) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickGetImageRedPrimary" :boolean
  ((wand magick-wand) (x (:out :double)) (y (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageRenderingIntent" rendering-intent
  ((wand magick-wand)))
(defmagickfun "MagickGetImageResolution" :boolean
  ((wand magick-wand) (x (:out :double))(y (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageScene" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageSignature" magick-string/free
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickGetImageTicksPerSecond" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageType" image-type
  ((wand magick-wand)))
(defmagickfun "MagickGetImageVirtualPixelMethod" virtual-pixel-method
  ((wand magick-wand)))
(defmagickfun "MagickGetImageWhitePoint" :boolean
  ((wand magick-wand) (x (:out :double)) (y (:out :double)))
  :check-error wand)
(defmagickfun "MagickGetImageWidth" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetNumberImages" :ulong
  ((wand magick-wand)))
(defmagickfun "MagickGetImageTotalInkDensity" :double
  ((wand magick-wand)))
(defmagickfun "MagickIdentifyImage" magick-string/free
  ((wand magick-wand))
  :check-error wand)
;; deprecated
(defmagickfun "MagickGetImageAttribute" magick-string/free
  ((wand magick-wand) (key magick-string)))
;; deprecated
(defmagickfun "MagickGetImageIndex" :long
  ((wand magick-wand)))
;; deprecated
(defmagickfun "MagickGetImageMatte" :boolean
  ((wand magick-wand)))
;; deprecated
(defmagickfun "MagickGetImageChannelDistortion" :boolean
  ((wand magick-wand) (reference magick-wand) (channel channel-type)
   (metric metric-type) (distortion (:out :double)))
  :check-error wand)
;; deprecated
(defmagickfun "MagickGetImageChannelExtrema" :boolean
  ((wand magick-wand) (channel channel-type)
   (min (:out :ulong)) (max (:out :ulong)))
  :check-error wand)
;; deprecated
(defmagickfun "MagickGetImageChannelMean" :boolean
  ((wand magick-wand) (channel channel-type)
   (mean (:out :double)) (std-dev (:out :double)))
  :check-error wand)
;; deprecated
(defmagickfun "MagickGetImageExtrema" :boolean
  ((wand magick-wand)
   (min (:out :ulong)) (max (:out :ulong)))
  :check-error wand)

;; Image set params
(defmagickfun "MagickSetImageAlphaChannel" :boolean
  ((wand magick-wand) (alpha-type alpha-channel-type))
  :check-error wand)
(defmagickfun "MagickSetImageBackgroundColor" :boolean
  ((wand magick-wand) (background pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageBluePrimary" :boolean
  ((wand magick-wand) (x magick-double) (y magick-double) (z magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageBorderColor" :boolean
  ((wand magick-wand) (border pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageChannelMask" :boolean
  ((wand magick-wand) (channel-mask channel-type))
  :check-error wand)
(defmagickfun "MagickSetImageColor" :boolean
  ((wand magick-wand) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageColormapColor" :boolean
  ((wand magick-wand) (index :ulong) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageColorspace" :boolean
  ((wand magick-wand) (colorspace colorspace-type))
  :check-error wand)
(defmagickfun "MagickSetImageCompose" :boolean
  ((wand magick-wand) (compose composite-operator))
  :check-error wand)
(defmagickfun "MagickSetImageCompression" :boolean
  ((wand magick-wand) (compression compression-type))
  :check-error wand)
(defmagickfun "MagickSetImageCompressionQuality" :boolean
  ((wand magick-wand) (quality :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageDelay" :boolean
  ((wand magick-wand) (delay :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageDepth" :boolean
  ((wand magick-wand) (depth :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageDispose" :boolean
  ((wand magick-wand) (dispose dispose-type))
  :check-error wand)
(defmagickfun "MagickSetImageEndian" :boolean
  ((wand magick-wand) (endian endian-type))
  :check-error wand)
(defmagickfun "MagickSetImageExtent" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageFilename" :boolean
  ((wand magick-wand) (filename magick-string))
  :check-error wand)
(defmagickfun "MagickSetImageFilter" :boolean
  ((wand magick-wand) (filter filter-type))
  :check-error wand)
(defmagickfun "MagickSetImageFormat" :boolean
  ((wand magick-wand) (format magick-string))
  :check-error wand)
(defmagickfun "MagickSetImageFuzz" :boolean
  ((wand magick-wand) (fuzz magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageGamma" :boolean
  ((wand magick-wand) (gamma magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageGravity" :boolean
  ((wand magick-wand) (gravity gravity-type))
  :check-error wand)
(defmagickfun "MagickSetImageGreenPrimary" :boolean
  ((wand magick-wand) (x magick-double) (y magick-double) (z magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageInterlaceScheme" :boolean
  ((wand magick-wand) (interlace interlace-type))
  :check-error wand)
(defmagickfun "MagickSetImageInterpolateMethod" :boolean
  ((wand magick-wand) (method pixel-interpolate-method))
  :check-error wand)
(defmagickfun "MagickSetImageIterations" :boolean
  ((wand magick-wand) (iterations :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageMatteColor" :boolean
  ((wand magick-wand) (matte pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageAlpha" :boolean
  ((wand magick-wand) (alpha magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageOrientation" :boolean
  ((wand magick-wand) (orientation orientation-type))
  :check-error wand)
(defmagickfun "MagickSetImagePage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickSetImagePixelColor" :boolean
  ((wand magick-wand) (x :long) (y :long) (color pixel-wand))
  :check-error wand)
(defmagickfun "MagickSetImageRedPrimary" :boolean
  ((wand magick-wand) (x magick-double) (y magick-double) (z magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageRenderingIntent" :boolean
  ((wand magick-wand) (rendering-intent rendering-intent))
  :check-error wand)
(defmagickfun "MagickSetImageResolution" :boolean
  ((wand magick-wand) (x-resolution magick-double) (y-resolution magick-double))
  :check-error wand)
(defmagickfun "MagickSetImageScene" :boolean
  ((wand magick-wand) (scene :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageTicksPerSecond" :boolean
  ((wand magick-wand) (ticks :ulong))
  :check-error wand)
(defmagickfun "MagickSetImageType" :boolean
  ((wand magick-wand) (image-type image-type))
  :check-error wand)
(defmagickfun "MagickSetImageVirtualPixelMethod" :boolean
  ((wand magick-wand) (method virtual-pixel-method))
  :check-error wand)
(defmagickfun "MagickSetImageWhitePoint" :boolean
  ((wand magick-wand) (x magick-double) (y magick-double) (z magick-double))
  :check-error wand)


;; Create/Read/write/remove images

(defmagickfun "MagickNewImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (background pixel-wand))
  :check-error wand)
(defmagickfun "MagickPingImage" :boolean
  ((wand magick-wand) (filename magick-string))
  :check-error wand)
(defmagickfun "MagickReadImage" :boolean
  ((wand magick-wand) (filename magick-string))
  :check-error wand)
(defmagickfun "MagickReadImageBlob" :boolean
  ((wand magick-wand)
   (blob (:dynarray :uint8))
   (length (:dynarray-length size-t blob)))
  :check-error wand)
(defmagickfun "MagickGetImageBlob" (:dynarray :uint8 :seq-type (vector (unsigned-byte 8)))
  ((wand magick-wand)
   (len (:dynarray-ret-length :ulong)))
  :check-error wand)
(defmagickfun "MagickRemoveImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickSetImage" :boolean
  ((wand magick-wand) (set-wand magick-wand))
  :check-error wand)
(defmagickfun "MagickWriteImage" :boolean
  ((wand magick-wand) (filename magick-string))
  :check-error wand)


;; Get/set pixel data

;; deprecated
(defmagickfun "MagickGetImagePixels" :boolean
  ((wand magick-wand) (x :long) (y :long) (columns :ulong) (rows :ulong)
   (map magick-string) (storage storage-type) (pixels :pointer))
  :check-error wand)
;; deprecated
(defmagickfun "MagickSetImagePixels" :boolean
  ((wand magick-wand) (x :long) (y :long) (columns :ulong) (rows :ulong)
   (map magick-string) (storage storage-type) (pixels :pointer))
  :check-error wand)


;; Image operations

(defmagickfun "MagickAdaptiveResizeImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickAdaptiveSharpenImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickAdaptiveThresholdImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (offset :long))
  :check-error wand)
(defmagickfun "MagickAddImage" :boolean
  ((wand magick-wand) (add-wand magick-wand))
  :check-error wand)
(defmagickfun "MagickAddNoiseImage" :boolean
  ((wand magick-wand) (type noise-type))
  :check-error wand)
(defmagickfun "MagickAffineTransformImage" :boolean
  ((wand magick-wand) (dwand drawing-wand))
  :check-error wand)
(defmagickfun "MagickAnnotateImage" :boolean
  ((wand magick-wand) (dwand drawing-wand)
   (x magick-double) (y magick-double) (angle magick-double) (text magick-string))
  :check-error wand)
(defmagickfun "MagickAnimateImages" :boolean
  ((wand magick-wand) (server-name magick-string))
  :check-error wand)
(defmagickfun "MagickAutoGammaImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickAutoLevelImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickAutoOrientImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickAutoThresholdImage" :boolean
  ((wand magick-wand) (method auto-threshold-method))
  :check-error wand)
(defmagickfun "MagickBilateralBlurImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double)
   (intensity-sigma magick-double) (spatial-sigma magick-double))
  :check-error wand)
(defmagickfun "MagickBlackThresholdImage" :boolean
  ((wand magick-wand) (threshold pixel-wand))
  :check-error wand)
(defmagickfun "MagickBlueShiftImage" :boolean
  ((wand magick-wand) (factor magick-double))
  :check-error wand)
(defmagickfun "MagickBlurImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickBlurImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickBorderImage" :boolean
  ((wand magick-wand) (color pixel-wand) (width :ulong) (height :ulong))
  :check-error wand)
(defmagickfun "MagickBrightnessContrastImage" :boolean
  ((wand magick-wand) (brightness magick-double) (contrast magick-double))
  :check-error wand)
(defmagickfun "MagickCannyEdgeImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double)
   (lower-percent magick-double) (upper-percent magick-double))
  :check-error wand)
(defmagickfun "MagickCharcoalImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickChopImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickCLAHEImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong)
   (number-bins magick-double) (clip-limit magick-double))
  :check-error wand)
(defmagickfun "MagickClampImage" :boolean
  ((wand magick-wand))
  :check-error wand)
;; deprecated
(defmagickfun "MagickClipImage" :boolean
  ((wand magick-wand))
  :check-error wand)
;; deprecated
(defmagickfun "MagickClipPathImage" :boolean
  ((wand magick-wand) (path-name magick-string) (inside :boolean))
  :check-error wand)
;; deprecated
(defmagickfun "MagickColorFloodfillImage" :boolean
  ((wand magick-wand) (fill pixel-wand) (fuzz magick-double)
   (border-color pixel-wand) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickColorizeImage" :boolean
  ((wand magick-wand) (colorize pixel-wand) (opacity pixel-wand))
  :check-error wand)
(defmagickfun "MagickColorThresholdImage" :boolean
  ((wand magick-wand) (start-color pixel-wand) (stop-color pixel-wand))
  :check-error wand)
(defmagickfun "MagickCommentImage" :boolean
  ((wand magick-wand) (comment magick-string))
  :check-error wand)
(defmagickfun "MagickCompositeImage" :boolean
  ((wand magick-wand) (src magick-wand) (compose composite-operator)
   (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickConstituteImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong) (map magick-string)
   (storage storage-type) (pixels :pointer))
  :check-error wand)
(defmagickfun "MagickContrastImage" :boolean
  ((wand magick-wand) (sharpen :boolean))
  :check-error wand)
(defmagickfun "MagickConvolveImage" :boolean
  ((wand magick-wand)
   (order (:dynarray-length :ulong kernel :expr (isqrt :l)))
   (kernel (:dynarray magick-double)))
  :check-error wand)
(defmagickfun "MagickConvolveImageChannel" :boolean
  ((wand magick-wand)
   (channel channel-type)
   (order (:dynarray-length :ulong kernel :expr (isqrt :l)))
   (kernel (:dynarray magick-double)))
  :check-error wand)
(defmagickfun "MagickCommentImage" :boolean
  ((wand magick-wand) (comment magick-string))
  :check-error wand)
(defmagickfun "MagickContrastImage" :boolean
  ((wand magick-wand) (sharpen :boolean))
  :check-error wand)
(defmagickfun "MagickContrastStretchImage" :boolean
  ((wand magick-wand) (black-point magick-double) (white-point magick-double))
  :check-error wand)
(defmagickfun "MagickCropImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickCycleColormapImage" :boolean
  ((wand magick-wand) (displace :long))
  :check-error wand)
(defmagickfun "MagickDecipherImage" :boolean
  ((wand magick-wand) (passphrase magick-string))
  :check-error wand)
(defmagickfun "MagickDeskewImage" :boolean
  ((wand magick-wand) (threshold pixel-wand))
  :check-error wand)
(defmagickfun "MagickDespeckleImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickDisplayImage" :boolean
  ((wand magick-wand) (server-name magick-string))
  :check-error wand)
(defmagickfun "MagickDisplayImages" :boolean
  ((wand magick-wand) (server-name magick-string))
  :check-error wand)
(defmagickfun "MagickDistortImage" :boolean
  ((wand magick-wand) (method distort-method)
   (num-args (:dynarray-length :ulong args))
   (args (:dynarray magick-double))
   (best-fit :boolean))
  :check-error wand)
(defmagickfun "MagickDrawImage" :boolean
  ((wand magick-wand) (dwand drawing-wand))
  :check-error wand)
(defmagickfun "MagickEdgeImage" :boolean
  ((wand magick-wand) (radius magick-double))
  :check-error wand)
(defmagickfun "MagickEmbossImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickEncipherImage" :boolean
  ((wand magick-wand) (passphrase magick-string))
  :check-error wand)
(defmagickfun "MagickEnhanceImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickEqualizeImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickEvaluateImage" :boolean
  ((wand magick-wand) (op magick-evaluate-operator) (constant magick-double))
  :check-error wand)
(defmagickfun "MagickEvaluateImageChannel" :boolean
  ((wand magick-wand) (channel channel-type)
   (op magick-evaluate-operator) (constant magick-double))
  :check-error wand)
(defmagickfun "MagickExtentImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickFlipImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickFloodfillPaintImage" :boolean
  ((wand magick-wand) (fill pixel-wand) (fuzz magick-double)
   (border-color pixel-wand) (x :long) (y :long) (invert :boolean))
  :check-error wand)
(defmagickfun "MagickFlopImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickForwardFourierTransformImage" :boolean
  ((wand magick-wand) (magnitude :boolean))
  :check-error wand)
(defmagickfun "MagickFrameImage" :boolean
  ((wand magick-wand) (matte-color pixel-wand)
   (width :ulong) (height :ulong) (inner-bevel :long) (outer-bevel :long))
  :check-error wand)
(defmagickfun "MagickGammaImage" :boolean
  ((wand magick-wand) (gamme magick-double))
  :check-error wand)
(defmagickfun "MagickGammaImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (gamme magick-double))
  :check-error wand)
(defmagickfun "MagickGaussianBlurImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickGaussianBlurImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickHaldClutImage" :boolean
  ((wand magick-wand) (hald-wand magick-wand))
  :check-error wand)
(defmagickfun "MagickHoughLineImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (threshold :long))
  :check-error wand)
(defmagickfun "MagickImplodeImage" :boolean
  ((wand magick-wand) (amount magick-double))
  :check-error wand)
(defmagickfun "MagickInterpolativeResizeImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong) (method pixel-interpolate-method))
  :check-error wand)
(defmagickfun "MagickInverseFourierTransformImage" :boolean
  ((magnitude-wand magick-wand) (phase-wand magick-wand) (magnitude :boolean))
  :check-error magnitude-wand)
(defmagickfun "MagickKmeansImage" :boolean
  ((wand magick-wand) (number-colors :ulong) (max-iterations :ulong) (tolerance magick-double))
  :check-error wand)
(defmagickfun "MagickKuwaharaImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickLabelImage" :boolean
  ((wand magick-wand) (label magick-string))
  :check-error wand)
(defmagickfun "MagickLevelImage" :boolean
  ((wand magick-wand) (black-point magick-double) (gamma magick-double)
   (white-point magick-double))
  :check-error wand)
(defmagickfun "MagickLevelImageChannel" :boolean
  ((wand magick-wand) (channel channel-type)
   (black-point magick-double) (gamma magick-double)
   (white-point magick-double))
  :check-error wand)
(defmagickfun "MagickLevelImageColors" :boolean
  ((wand magick-wand) (black-color pixel-wand) (white-color pixel-wand) (invert :boolean))
  :check-error wand)
(defmagickfun "MagickLevelizeImage" :boolean
  ((wand magick-wand) (black-point magick-double)
   (white-point magick-double) (gamma magick-double))
  :check-error wand)
(defmagickfun "MagickLinearStretchImage" :boolean
  ((wand magick-wand) (black-point magick-double) (white-point magick-double))
  :check-error wand)
(defmagickfun "MagickLiquidRescaleImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong)
   (delta-x magick-double) (rigidity magick-double))
  :check-error wand)
(defmagickfun "MagickLocalContrastImage" :boolean
  ((wand magick-wand) (radius magick-double) (strength magick-double))
  :check-error wand)
;; deprecated
(defmagickfun "MagickMagnifyImage" :boolean
  ((wand magick-wand))
  :check-error wand)
;; deprecated
(defmagickfun "MagickMapImage" :boolean
  ((wand magick-wand) (map-wand magick-wand) (dither :boolean))
  :check-error wand)
;; deprecated
(defmagickfun "MagickMatteFloodfillImage" :boolean
  ((wand magick-wand) (opacity quantum) (fuzz magick-double)
   (border-color pixel-wand) (x :long) (y :long))
  :check-error wand)
;; deprecated
(defmagickfun "MagickMedianFilterImage" :boolean
  ((wand magick-wand) (radius magick-double))
  :check-error wand)
(defmagickfun "MagickMeanShiftImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (color-distance magick-double))
  :check-error wand)
(defmagickfun "MagickMinifyImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickModulateImage" :boolean
  ((wand magick-wand) (brightness magick-double) (saturation magick-double) (hue magick-double))
  :check-error wand)
(defmagickfun "MagickMotionBlurImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double) (angle magick-double))
  :check-error wand)
(defmagickfun "MagickNegateImage" :boolean
  ((wand magick-wand) (gray :boolean))
  :check-error wand)
(defmagickfun "MagickNegateImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (gray :boolean))
  :check-error wand)
(defmagickfun "MagickNormalizeImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickNormalizeImageChannel" :boolean
  ((wand magick-wand) (channel channel-type))
  :check-error wand)
(defmagickfun "MagickOilPaintImage" :boolean
  ((wand magick-wand) (radius magick-double))
  :check-error wand)
(defmagickfun "MagickOpaquePaintImage" :boolean
  ((wand magick-wand) (target pixel-wand) (fill pixel-wand) (fuzz magick-double) (invert :boolean))
  :check-error wand)
(defmagickfun "MagickOptimizeImageTransparency" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickOrderedDitherImage" :boolean
  ((wand magick-wand) (threshold-map magick-string))
  :check-error wand)
;; deprecated
(defmagickfun "MagickPaintOpaqueImage" :boolean
  ((wand magick-wand) (target pixel-wand) (fill pixel-wand) (fuzz magick-double))
  :check-error wand)
;; deprecated
(defmagickfun "MagickPaintOpaqueImageChannel" :boolean
  ((wand magick-wand) (channel channel-type)
   (target pixel-wand) (fill pixel-wand) (fuzz magick-double))
  :check-error wand)
;; deprecated
(defmagickfun "MagickPaintTransparentImage" :boolean
  ((wand magick-wand) (target pixel-wand) (opacity quantum) (fuzz magick-double))
  :check-error wand)
(defmagickfun "MagickPingImage" :boolean
  ((wand magick-wand) (filename magick-string))
  :check-error wand)
(defmagickfun "MagickPingImageBlob" :boolean
  ((wand magick-wand)
   (blob (:dynarray :uint8))
   (length (:dynarray-length size-t blob)))
  :check-error wand)
(defmagickfun "MagickPolaroidImage" :boolean
  ((wand magick-wand) (drawing-wand drawing-wand) (caption magick-string)
   (angle magick-double) (method pixel-interpolate-method))
  :check-error wand)
(defmagickfun "MagickPolynomialImage" :boolean
  ((wand magick-wand) (num (:dynarray-length :ulong terms)) (terms (:dynarray magick-double)))
  :check-error wand)
(defmagickfun "MagickPosterizeImage" :boolean
  ((wand magick-wand) (levels :ulong) (dither :boolean))
  :check-error wand)
(defmagickfun "MagickProfileImage" :boolean
  ((wand magick-wand) (name magick-string)
   (profile (:dynarray :uint8))
   (length (:dynarray-length size-t profile)))
  :check-error wand)
(defmagickfun "MagickQuantizeImage" :boolean
  ((wand magick-wand) (num-colors :ulong) (colorspace colorspace-type)
   (tree-depth :ulong) (dither :boolean) (measure-error :boolean))
  :check-error wand)
(defmagickfun "MagickQuantizeImages" :boolean
  ((wand magick-wand) (num-colors :ulong) (colorspace colorspace-type)
   (tree-depth :ulong) (dither :boolean) (measure-error :boolean))
  :check-error wand)
;; deprecated
(defmagickfun "MagickRadialBlurImage" :boolean
  ((wand magick-wand) (angle magick-double))
  :check-error wand)
;; deprecated
(defmagickfun "MagickRadialBlurImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (angle magick-double))
  :check-error wand)
(defmagickfun "MagickRaiseImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong)
   (x :long) (y :long) (raise :boolean))
  :check-error wand)
(defmagickfun "MagickRandomThresholdImage" :boolean
  ((wand magick-wand) (low magick-double) (high magick-double))
  :check-error wand)
(defmagickfun "MagickRangeThresholdImage" :boolean
  ((wand magick-wand) (low-black magick-double) (low-white magick-double)
   (high-black magick-double) (high-white magick-double))
  :check-error wand)
;; deprecated
(defmagickfun "MagickReduceNoiseImage" :boolean
  ((wand magick-wand) (radius magick-double))
  :check-error wand)
(defmagickfun "MagickRemapImage" :boolean
  ((wand magick-wand) (remap-wand magick-wand) (method dither-method))
  :check-error wand)
(defmagickfun "MagickResampleImage" :boolean
  ((wand magick-wand) (x-resolution magick-double) (y-resolution magick-double)
   (filter filter-type))
  :check-error wand)
(defmagickfun "MagickResetImagePage" :boolean
  ((wand magick-wand) (page magick-string))
  :check-error wand)
(defmagickfun "MagickResizeImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong) (filter filter-type))
  :check-error wand)
(defmagickfun "MagickRollImage" :boolean
  ((wand magick-wand) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickRotateImage" :boolean
  ((wand magick-wand) (background pixel-wand) (degrees magick-double))
  :check-error wand)
(defmagickfun "MagickRotationalBlurImage" :boolean
  ((wand magick-wand) (angle magick-double))
  :check-error wand)
(defmagickfun "MagickSampleImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickScaleImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickSegmentImage" :boolean
  ((wand magick-wand) (colorspace colorspace-type) (verbose :boolean)
   (cluster-threshold magick-double) (smooth-threshold magick-double))
  :check-error wand)
(defmagickfun "MagickSelectiveBlurImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickSeparateImage" :boolean
  ((wand magick-wand) (channel channel-type))
  :check-error wand)
;; deprecated
(defmagickfun "MagickSeparateImageChannel" :boolean
  ((wand magick-wand) (channel channel-type))
  :check-error wand)
(defmagickfun "MagickSepiaToneImage" :boolean
  ((wand magick-wand) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickShadeImage" :boolean
  ((wand magick-wand) (gray :boolean) (asimuth magick-double) (elevation magick-double))
  :check-error wand)
(defmagickfun "MagickShadowImage" :boolean
  ((wand magick-wand) (alpha magick-double) (sigma magick-double) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickSharpenImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double))
  :check-error wand)
(defmagickfun "MagickShaveImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickShearImage" :boolean
  ((wand magick-wand) (background pixel-wand) (x-shear magick-double) (y-shear magick-double))
  :check-error wand)
(defmagickfun "MagickSigmoidalContrastImage" :boolean
  ((wand magick-wand) (sharpen :boolean) (alpha magick-double) (beta magick-double))
  :check-error wand)
(defmagickfun "MagickSketchImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double) (angle magick-double))
  :check-error wand)
(defmagickfun "MagickSolarizeImage" :boolean
  ((wand magick-wand) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickSparseColorImage" :boolean
  ((wand magick-wand) (method sparse-color-method)
   (num (:dynarray-length :ulong args)) (args (:dynarray magick-double)))
  :check-error wand)
(defmagickfun "MagickSpliceImage" :boolean
  ((wand magick-wand) (width :ulong) (height :ulong) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickSpreadImage" :boolean
  ((wand magick-wand) (method pixel-interpolate-method) (radius magick-double))
  :check-error wand)
(defmagickfun "MagickStatisticImage" :boolean
  ((wand magick-wand) (type statistic-type) (width :ulong) (height :ulong))
  :check-error wand)
(defmagickfun "MagickStripImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickSwirlImage" :boolean
  ((wand magick-wand) (degrees magick-double) (method pixel-interpolate-method))
  :check-error wand)
(defmagickfun "MagickThresholdImage" :boolean
  ((wand magick-wand) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickThresholdImageChannel" :boolean
  ((wand magick-wand) (channel channel-type) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickThumbnailImage" :boolean
  ((wand magick-wand) (columns :ulong) (rows :ulong))
  :check-error wand)
(defmagickfun "MagickTintImage" :boolean
  ((wand magick-wand) (tint pixel-wand) (blend pixel-wand))
  :check-error wand)
(defmagickfun "MagickTransformImageColorspace" :boolean
  ((wand magick-wand) (colorspace colorspace-type))
  :check-error wand)
(defmagickfun "MagickTransparentPaintImage" :boolean
  ((wand magick-wand) (target pixel-wand) (alpha magick-double)
   (fuzz magick-double) (invert :boolean))
  :check-error wand)
(defmagickfun "MagickTransposeImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickTransverseImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickTrimImage" :boolean
  ((wand magick-wand) (fuzz magick-double))
  :check-error wand)
(defmagickfun "MagickUniqueImageColors" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickUnsharpMaskImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double)
   (gain magick-double) (threshold magick-double))
  :check-error wand)
(defmagickfun "MagickVignetteImage" :boolean
  ((wand magick-wand) (radius magick-double) (sigma magick-double) (x :long) (y :long))
  :check-error wand)
(defmagickfun "MagickWaveImage" :boolean
  ((wand magick-wand) (amplitude magick-double)
   (wave-length magick-double) (method pixel-interpolate-method))
  :check-error wand)
(defmagickfun "MagickWaveletDenoiseImage" :boolean
  ((wand magick-wand) (threshold magick-double) (softness magick-double))
  :check-error wand)
(defmagickfun "MagickWhiteBalanceImage" :boolean
  ((wand magick-wand))
  :check-error wand)
(defmagickfun "MagickWhiteThresholdImage" :boolean
  ((wand magick-wand) (threshold pixel-wand))
  :check-error wand)




;;; Pixel Wands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creation, destruction, etc.

(defmagickfun "NewPixelWand"       pixel-wand ())
(defmagickfun "DestroyPixelWand"   pixel-wand ((wand pixel-wand)))
(defmagickfun "IsPixelWand"        :boolean   ((wand pixel-wand)))
(defmagickfun "ClearPixelWand"     :void      ((wand pixel-wand)))
(defmagickfun "IsPixelWandSimilar" :boolean   ((wand1 pixel-wand) (wand2 pixel-wand) (fuzz magick-double)))


;; Get / set color as string

(defmagickfun "PixelGetColorAsString" magick-string/free ((wand pixel-wand)))
(defmagickfun "PixelGetColorAsNormalizedString" magick-string/free ((wand pixel-wand)))
(defmagickfun "PixelSetColor"         :boolean           ((wand pixel-wand) (color magick-string)) :check-error wand)


;; Components as double

(defmagickfun "PixelGetAlpha"   :double ((wand pixel-wand)))
(defmagickfun "PixelGetBlack"   :double ((wand pixel-wand)))
(defmagickfun "PixelGetBlue"    :double ((wand pixel-wand)))
(defmagickfun "PixelGetCyan"    :double ((wand pixel-wand)))
(defmagickfun "PixelGetGreen"   :double ((wand pixel-wand)))
(defmagickfun "PixelGetMagenta" :double ((wand pixel-wand)))
(defmagickfun "PixelGetOpacity" :double ((wand pixel-wand)))
(defmagickfun "PixelGetRed"     :double ((wand pixel-wand)))
(defmagickfun "PixelGetYellow"  :double ((wand pixel-wand)))

(defmagickfun "PixelSetAlpha"   :void   ((wand pixel-wand) (alpha   magick-double)))
(defmagickfun "PixelSetBlack"   :void   ((wand pixel-wand) (black   magick-double)))
(defmagickfun "PixelSetBlue"    :void   ((wand pixel-wand) (blue    magick-double)))
(defmagickfun "PixelSetCyan"    :void   ((wand pixel-wand) (cyan    magick-double)))
(defmagickfun "PixelSetGreen"   :void   ((wand pixel-wand) (green   magick-double)))
(defmagickfun "PixelSetMagenta" :void   ((wand pixel-wand) (magenta magick-double)))
(defmagickfun "PixelSetOpacity" :void   ((wand pixel-wand) (opacity magick-double)))
(defmagickfun "PixelSetRed"     :void   ((wand pixel-wand) (red     magick-double)))
(defmagickfun "PixelSetYellow"  :void   ((wand pixel-wand) (yellow  magick-double)))


;; Components as quantum

(defmagickfun "PixelGetAlphaQuantum"   quantum ((wand pixel-wand)))
(defmagickfun "PixelGetBlackQuantum"   quantum ((wand pixel-wand)))
(defmagickfun "PixelGetBlueQuantum"    quantum ((wand pixel-wand)))
(defmagickfun "PixelGetCyanQuantum"    quantum ((wand pixel-wand)))
(defmagickfun "PixelGetGreenQuantum"   quantum ((wand pixel-wand)))
(defmagickfun "PixelGetMagentaQuantum" quantum ((wand pixel-wand)))
(defmagickfun "PixelGetOpacityQuantum" quantum ((wand pixel-wand)))
(defmagickfun "PixelGetRedQuantum"     quantum ((wand pixel-wand)))
(defmagickfun "PixelGetYellowQuantum"  quantum ((wand pixel-wand)))

(defmagickfun "PixelSetAlphaQuantum"   :void   ((wand pixel-wand) (alpha   quantum)))
(defmagickfun "PixelSetBlackQuantum"   :void   ((wand pixel-wand) (black   quantum)))
(defmagickfun "PixelSetBlueQuantum"    :void   ((wand pixel-wand) (blue    quantum)))
(defmagickfun "PixelSetCyanQuantum"    :void   ((wand pixel-wand) (cyan    quantum)))
(defmagickfun "PixelSetGreenQuantum"   :void   ((wand pixel-wand) (green   quantum)))
(defmagickfun "PixelSetMagentaQuantum" :void   ((wand pixel-wand) (magenta quantum)))
(defmagickfun "PixelSetOpacityQuantum" :void   ((wand pixel-wand) (opacity quantum)))
(defmagickfun "PixelSetRedQuantum"     :void   ((wand pixel-wand) (red     quantum)))
(defmagickfun "PixelSetYellowQuantum"  :void   ((wand pixel-wand) (yellow  quantum)))


;; Other accessors

(defmagickfun "PixelGetColorCount" :ulong ((wand pixel-wand)))
(defmagickfun "PixelSetColorCount" :void  ((wand pixel-wand) (count :ulong)))
(defmagickfun "PixelGetFuzz" :double ((wand pixel-wand)))
(defmagickfun "PixelSetFuzz" :void   ((wand pixel-wand) (fuzz magick-double)))
(defmagickfun "PixelGetIndex" quantum ((wand pixel-wand)))
(defmagickfun "PixelSetIndex" :void   ((wand pixel-wand) (index quantum)))


;;; Drawing Wands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creation, destruction, etc.

(defmagickfun "NewDrawingWand"     drawing-wand ())
(defmagickfun "DestroyDrawingWand" drawing-wand ((wand drawing-wand)))
(defmagickfun "IsDrawingWand"      :boolean     ((wand drawing-wand)))
(defmagickfun "CloneDrawingWand"   drawing-wand ((wand drawing-wand)))
(defmagickfun "ClearDrawingWand"   :void        ((wand drawing-wand)))


;; Attributes

(defmagickfun "DrawGetTextAlignment"    align-type         ((wand drawing-wand)))
(defmagickfun "DrawGetClipPath"         magick-string/free ((wand drawing-wand)))
(defmagickfun "DrawGetFont"             magick-string/free ((wand drawing-wand)))
(defmagickfun "DrawGetFontFamily"       magick-string/free ((wand drawing-wand)))
(defmagickfun "DrawGetTextEncoding"     magick-string/free ((wand drawing-wand)))
(defmagickfun "DrawGetVectorGraphics"   magick-string/free ((wand drawing-wand)))
(defmagickfun "DrawGetClipUnits"        clip-path-units    ((wand drawing-wand)))
(defmagickfun "DrawGetTextDecoration"   decoration-type    ((wand drawing-wand)))
(defmagickfun "DrawGetFillAlpha"        :double            ((wand drawing-wand))) ;deprecated
(defmagickfun "DrawGetFontSize"         :double            ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeDashOffset" :double            ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeAlpha"      :double            ((wand drawing-wand))) ;deprecated
(defmagickfun "DrawGetStrokeWidth"      :double            ((wand drawing-wand)))
(defmagickfun "DrawGetClipRule"         fill-rule          ((wand drawing-wand)))
(defmagickfun "DrawGetFillRule"         fill-rule          ((wand drawing-wand)))
(defmagickfun "DrawGetGravity"          gravity-type       ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeLineCap"    line-cap           ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeLineJoin"   line-join          ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeAntialias"  :boolean           ((wand drawing-wand)))
(defmagickfun "DrawGetTextAntialias"    :boolean           ((wand drawing-wand)))
(defmagickfun "DrawGetFontStretch"      stretch-type       ((wand drawing-wand)))
(defmagickfun "DrawGetFontStyle"        style-type         ((wand drawing-wand)))
(defmagickfun "DrawGetFontWeight"       :ulong             ((wand drawing-wand)))
(defmagickfun "DrawGetStrokeMiterLimit" :ulong             ((wand drawing-wand)))

(defmagickfun "DrawSetClipRule"         :void ((wand drawing-wand) (rule fill-rule)))
(defmagickfun "DrawSetClipUnits"        :void ((wand drawing-wand) (units clip-path-units)))
(defmagickfun "DrawSetFillAlpha"        :void ((wand drawing-wand) (alpha magick-double))) ;deprecated
(defmagickfun "DrawSetFillRule"         :void ((wand drawing-wand) (rule fill-rule)))
(defmagickfun "DrawSetFontSize"         :void ((wand drawing-wand) (size magick-double)))
(defmagickfun "DrawSetFontStretch"      :void ((wand drawing-wand) (stretch stretch-type)))
(defmagickfun "DrawSetFontStyle"        :void ((wand drawing-wand) (style style-type)))
(defmagickfun "DrawSetFontWeight"       :void ((wand drawing-wand) (weight :ulong)))
(defmagickfun "DrawSetGravity"          :void ((wand drawing-wand) (gravity gravity-type)))
(defmagickfun "DrawSetStrokeAntialias"  :void ((wand drawing-wand) (antialias :boolean)))
(defmagickfun "DrawSetStrokeDashOffset" :void ((wand drawing-wand) (offset magick-double)))
(defmagickfun "DrawSetStrokeLineCap"    :void ((wand drawing-wand) (cap line-cap)))
(defmagickfun "DrawSetStrokeLineJoin"   :void ((wand drawing-wand) (join line-join)))
(defmagickfun "DrawSetStrokeMiterLimit" :void ((wand drawing-wand) (limit :ulong)))
(defmagickfun "DrawSetStrokeAlpha"      :void ((wand drawing-wand) (alpha magick-double))) ;deprecated
(defmagickfun "DrawSetStrokeWidth"      :void ((wand drawing-wand) (width magick-double)))
(defmagickfun "DrawSetTextAlignment"    :void ((wand drawing-wand) (align align-type)))
(defmagickfun "DrawSetTextAntialias"    :void ((wand drawing-wand) (antialias :boolean)))
(defmagickfun "DrawSetTextDecoration"   :void ((wand drawing-wand) (deco decoration-type)))
(defmagickfun "DrawSetTextEncoding"     :void ((wand drawing-wand) (encoding magick-string)))
(defmagickfun "DrawSetViewbox"          :void ((wand drawing-wand) (x1 :ulong) (y1 :ulong) (x2 :ulong) (y2 :ulong)))

(defmagickfun "DrawGetFillColor"        :void ((dwand drawing-wand) (pwand pixel-wand)))
(defmagickfun "DrawGetStrokeColor"      :void ((dwand drawing-wand) (pwand pixel-wand)))
(defmagickfun "DrawGetTextUnderColor"   :void ((dwand drawing-wand) (pwand pixel-wand)))

(defmagickfun "DrawSetFillColor"        :void ((dwand drawing-wand) (pwand pixel-wand)))
(defmagickfun "DrawSetStrokeColor"      :void ((dwand drawing-wand) (pwand pixel-wand)))
(defmagickfun "DrawSetTextUnderColor"   :void ((dwand drawing-wand) (pwand pixel-wand)))

(defmagickfun "DrawSetFont"       :boolean ((dwand drawing-wand) (font-name magick-string)))
(defmagickfun "DrawSetFontFamily" :boolean ((dwand drawing-wand) (font-family magick-string)))

;; Draw operations

(defmagickfun "DrawAnnotation"     :void ((dwand drawing-wand) (x magick-double) (y magick-double)
                                          (text magick-string)))
(defmagickfun "DrawArc"            :void ((dwand drawing-wand) (sx magick-double) (sy magick-double)
                                          (ex magick-double) (ey magick-double)
                                          (sd magick-double) (ed magick-double)))
(defmagickfun "DrawCircle"         :void ((dwand drawing-wand) (ox magick-double) (oy magick-double)
                                          (px magick-double) (py magick-double)))
(defmagickfun "DrawColor"          :void ((dwand drawing-wand) (x magick-double) (y magick-double)
                                          (method paint-method)))
(defmagickfun "DrawComment"        :void ((dwand drawing-wand) (comment magick-string)))
(defmagickfun "DrawEllipse"        :void ((dwand drawing-wand) (ox magick-double) (oy magick-double)
                                          (rx magick-double) (ry magick-double)
                                          (start magick-double) (end magick-double)))
(defmagickfun "DrawLine"           :void ((dwand drawing-wand) (sx magick-double) (sy magick-double)
                                          (ex magick-double) (ey magick-double)))
(defmagickfun "DrawMatte"          :void ((dwand drawing-wand) (x magick-double) (y magick-double)
                                          (method paint-method)))
(defmagickfun "DrawPoint"          :void ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawRectangle"      :void ((dwand drawing-wand) (x1 magick-double) (y1 magick-double)
                                          (x2 magick-double) (y2 magick-double)))
(defmagickfun "DrawRoundRectangle" :void ((dwand drawing-wand) (x1 magick-double) (y1 magick-double)
                                          (x2 magick-double) (y2 magick-double)
                                          (rx magick-double) (ry magick-double)))


;; Path operations

(defmagickfun "DrawPathClose" :void ((dwand drawing-wand)))
(defmagickfun "DrawPathCurveToAbsolute" :void
  ((dwand drawing-wand)
   (x1 magick-double) (y1 magick-double) (x2 magick-double) (y2 magick-double)
   (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToRelative" :void
  ((dwand drawing-wand)
   (x1 magick-double) (y1 magick-double) (x2 magick-double) (y2 magick-double)
   (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToQuadraticBezierAbsolute" :void
  ((dwand drawing-wand) (x1 magick-double) (y1 magick-double) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToQuadraticBezierRelative" :void
  ((dwand drawing-wand) (x1 magick-double) (y1 magick-double) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToQuadraticBezierSmoothAbsolute" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToQuadraticBezierSmoothRelative" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToSmoothAbsolute" :void
  ((dwand drawing-wand) (x2 magick-double) (y2 magick-double) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathCurveToSmoothRelative" :void
  ((dwand drawing-wand) (x2 magick-double) (y2 magick-double) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathEllipticArcAbsolute" :void
  ((dwand drawing-wand)
   (rx magick-double) (ry magick-double) (x-axis-rotation magick-double)
   (large-arc-p :boolean) (sweep-p :boolean) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathEllipticArcRelative" :void
  ((dwand drawing-wand)
   (rx magick-double) (ry magick-double) (x-axis-rotation magick-double)
   (large-arc-p :boolean) (sweep-p :boolean) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathFinish" :void ((dwand drawing-wand)))
(defmagickfun "DrawPathLineToAbsolute" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathLineToRelative" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathLineToHorizontalAbsolute" :void
  ((dwand drawing-wand) (x magick-double)))
(defmagickfun "DrawPathLineToHorizontalRelative" :void
  ((dwand drawing-wand) (x magick-double)))
(defmagickfun "DrawPathLineToVerticalAbsolute" :void
  ((dwand drawing-wand) (y magick-double)))
(defmagickfun "DrawPathLineToVerticalRelative" :void
  ((dwand drawing-wand) (y magick-double)))
(defmagickfun "DrawPathMoveToAbsolute" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathMoveToRelative" :void
  ((dwand drawing-wand) (x magick-double) (y magick-double)))
(defmagickfun "DrawPathStart" :void ((dwand drawing-wand)))
