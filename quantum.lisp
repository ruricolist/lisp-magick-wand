(in-package :lisp-magick-wand)

(defmagickfun "MagickGetQuantumDepth" :string ((depth (:out :ulong))))

(let ((qdepth (nth-value 1 (get-quantum-depth))))
  (case qdepth
    (8  (push 'quantum-8  *features*))
    (16 (push 'quantum-16 *features*))
    (32 (push 'quantum-32 *features*))
    (64 (push 'quantum-64 *features*))
    (t  (error "quantum depth ~a not supported" qdepth))))
