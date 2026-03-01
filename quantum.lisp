(in-package :lisp-magick-wand)

(defmagickfun "MagickGetQuantumDepth" :string ((depth (:out :ulong))))

(let ((qdepth (nth-value 1 (get-quantum-depth))))
  (case qdepth
    (8  (pushnew 'quantum-8  *features*))
    (16 (pushnew 'quantum-16 *features*))
    (32 (pushnew 'quantum-32 *features*))
    (64 (pushnew 'quantum-64 *features*))
    (t  (error "quantum depth ~a not supported" qdepth))))
