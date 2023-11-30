# Lisp-Magick-Wand

Common Lisp bindings to ImageMagick's
[MagicWand](http://www.imagemagick.org/script/magick-wand.php) API. \
This is a fork of [LISP-MAGICK][lisp-magick]. \
It is compatible with recent versions of ImageMagick (7.x) 
and has a slightly different API.

## Usage

For the most part, you can use the [C API][api] as a reference.
Translations follow a few simple rules:

- Function names are lisp-ified. For example, `MagickGetImageWidth`
  becomes `magick:get-image-width`. Note that we omit any redundant
  `Magick` prefix.
- Functions that return multiple results via pointer arguments return
  multiple values.
- In most cases, a failed operation is signaled as `magick-wand-error`
  condition.
- When a function expects an array of values and its length, a vector
  or a list can be passed in CL-MAGICK, and the length parameter is
  omitted. `nil` is passed as a `NULL` pointer (but `#()` is not).

## Example

``` lisp
(defpackage :lisp-magick-examples
  (:use :cl))
(in-package :lisp-magick-examples)

(defun create-thumbnail (filename thumbname width height)
  "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
    pixel (but with the original aspect ratio) and save it in THUMBNAME."
  (magick:with-magick-wand (wand :load filename)
    (let ((a (/ (magick:get-image-width wand)
                (magick:get-image-height wand))))
      (if (> a (/ width height))
          (magick:scale-image wand width (truncate (/ width a)))
          (magick:scale-image wand (truncate (* a height)) height)))
    (magick:write-image wand thumbname)))

(defun draw-a-few-lines (filename width height)
  "Create a new image with WIDTH x HEIGHT pixel containing 50 random lines
    and save it in FILENAME."
  (magick:with-magick-wand (wand :create width height :comp (0 0 0))
    (magick:with-drawing-wand (dw)
      (magick:with-pixel-wand (pw :comp (255 255 255))
        (magick:draw-set-stroke-color dw pw))
      (magick:draw-set-stroke-width dw 3d0)
      (dotimes (i 50)
        (magick:draw-line dw
                          (coerce (random width) 'double-float)
                          (coerce (random height) 'double-float)
                          (coerce (random width) 'double-float)
                          (coerce (random height) 'double-float)))
      (magick:draw-image wand dw))
    (magick:write-image wand filename)))
```

## Contributors

- [Hans Bulfone](http://www.nil.at/)
- [Paul M. Rodriguez](https://github.com/ruricolist)
- [Dmitrii Kosenkov](https://github.com/Junker)


[api]: http://www.imagemagick.org/script/magick-wand.php
[lisp-magick]: http://www.nil.at/software/lisp-magick.html
