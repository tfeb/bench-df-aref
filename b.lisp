;;;; Performance of double-float dot products with offset
;;;

(in-package :cl-user)

(needs (:org.tfeb.tools.timing :compile t :use t))

;;; For all these fns the compiler will warn about consing a double
;;; for the return value.  That's ... fine.
;;;

(defun dot-offset-dotimes (u v ou ov n iters)
  (declare (type (simple-array double-float (*)) u v)
           (type fixnum ov ou n iters)
           (optimize speed (safety 0)))
  #+LispWorks
  (declare (optimize (float 0))
           (:explain :floats :boxing))
  (let ((r 0.0d0))
    (declare (type double-float r))
    (dotimes (iter iters)
      (declare (type fixnum iter))
      (dotimes (i n)
        (declare (type fixnum i))
        (incf r (* (aref u (+ i ou))
                   (aref v (+ i ov))))))
    r))

(defun dot-offset-do (u v ou ov n iters)
  ;; Better in LW!
  (declare (type (simple-array double-float (*)) u v)
           (type fixnum ov ou n iters)
           (optimize speed (safety 0)))
  #+LispWorks
  (declare (optimize (float 0))
           (:explain :floats :boxing))
  (let ((r 0.0d0))
    (declare (type double-float r))
    (dotimes (iter iters)
      (declare (type fixnum iter))
      (do ((i 0 (1+ i)))
          ((>= i n))
        (declare (type fixnum i))
        (incf r (* (aref u (+ i ou))
                   (aref v (+ i ov))))))
    r))

(defun dot-offset-do-u2 (u v ou ov n iters)
  ;; Unroll two iterations.  No better in either.
  (declare (type (simple-array double-float (*)) u v)
           (type fixnum ov ou n iters)
           (optimize speed (safety 0)))
  #+LispWorks
  (declare (optimize (float 0))
           (:explain :floats :boxing))
  (let ((r 0.0d0)
        (ou+1 (1+ ou))
        (ov+1 (1+ ov)))
    (declare (type double-float r)
             (type fixnum ou+1 ov+1))
    (dotimes (iter iters)
      (declare (type fixnum iter))
      (case n
        (0 nil)
        (1
         (incf r (* (aref u ou) (aref v ov))))
        (otherwise
         (do ((i 0 (+ i 2)))
             ((>= i n)
              (when (> i n)
                (incf r (* (aref u (+ i ou))
                           (aref v (+ i ov))))))
           (declare (type fixnum i))
           (incf r (* (aref u (+ i ou))
                      (aref v (+ i ov))))
           (incf r (* (aref u (+ i ou+1))
                      (aref v (+ i ov+1))))))))
      r))

(defun dot-offset-avoid-addition (u v ou ov n iters)
  ;; Same performance in SBCL, much slower in LW (pretty sure it's
  ;; lost track of the array being simple)
  (declare (type (simple-array double-float (*)) u v)
           (type fixnum ov ou n iters)
           (optimize speed (safety 0)))
  #+LispWorks
  (declare (optimize (float 0))
           (:explain :floats :boxing))
  (let ((r 0.0d0)
        (bu (+ ou n)))
    (declare (type double-float r)
             (type fixnum bu))
    (dotimes (iter iters)
      (declare (type fixnum iter))
      (do ((iu ou (1+ iu))
           (iv ov (1+ iv)))
          ((>= iu bu))
        (declare (type fixnum iu iv))
        (incf r (* (aref u iu)
                   (aref v iv)))))
    r))

(defparameter *best* 'dot-offset-do)

;;; This needs to be a whole lot better

(defmacro testing-dotters ((dotter dotters) &body forms)
  `(progn
     ,@(mapcar (lambda (d)
                 `(flet ((,dotter (&rest args)
                           (apply (function ,d) args)))
                    (unless (and ,@forms)
                      (error "~S fails" ',d))))
               dotters)
     ',dotters))

(defun tsit ()
  (testing-dotters (dotter (dot-offset dot-offset-avoid-addition
                                       dot-offset-do
                                       dot-offset-do-u2))
    (and
     (let ((a (make-array 100 :element-type 'double-float :initial-element 0.0d0)))
       (setf (aref a 0) 2.0d0)
       (and (= (dotter a a 0 0 10 1) 4.0d0)
            (= (dotter a a 1 1 9 1) 0.0d0)
            (= (dotter a a 0 0 1 1) 4.0d0)
            (= (dotter a a 0 0 1 10) 40.0d0)))
     (let ((a (make-array 1 :element-type 'double-float :initial-element 2.0d0)))
       (= (dotter a a 0 0 1 1) 4.0d0))
     (let ((a (make-array 3 :element-type 'double-float :initial-element 2.0d0)))
       (= (dotter a a 0 0 3 1) 12.0d0)))))

(defun benchit (l &key (trials 100) (iters 100) (report-every (/ trials 4))
                    (o1 0) (o2 0) (n l) (what *best*))
  (assert (and (>= o1 0)
               (>= o2 0)
               (>= n 0)
               (<= (+ o1 n) l)
               (<= (+ o2 n) l))
          (o2 o2 l)
          "out of bounds")
  (let ((u (make-array l :element-type 'double-float :initial-element 0.0d0))
        (v (make-array l :element-type 'double-float :initial-element 0.0d0)))
    (macrolet ((choose (&body names)
                 `(case what
                    ,@(mapcar (lambda (name)
                                `(,name
                                  (/ (timing (trials report-every)
                                             (,name u v o1 o2 n iters))
                                     n iters)))
                              names)
                    (otherwise
                     (error "~S isn't one of ~{~S~^, ~}" what ',names)))))
      (choose
       dot-offset
       dot-offset-avoid-addition
       dot-offset-do
       dot-offset-do-u2))))

(defun benchem ()
  (dolist (what '(dot-offset dot-offset-avoid-addition dot-offset-do
                  dot-offset-do-u2))
    (format t "~&~30S" what)
    (finish-output)
    (format t "~D~%"
            (benchit 1000000 :o1 300000 :o2 600000 :n 200000
                     :what what :report-every nil))))
