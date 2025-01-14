;;;; Performance of double-float dot products with offset
;;;

(in-package :cl-user)

(needs (:org.tfeb.tools.timing :compile t :use t))

(defun dot-offset (u v ou ov n iters)
  ;; Compiler will warn about consing return double
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

(defun tsit ()
  (let ((a (make-array 100 :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref a 0) 2.0d0)
    (and (= (dot-offset a a 0 0 10 1) 4.0d0)
         (= (dot-offset a a 1 1 9 1) 0.0d0)
         (= (dot-offset a a 0 0 1 1) 4.0d0)
         (= (dot-offset a a 0 0 1 10) 40.0d0))))

(defun benchit (l &key (trials 100) (iters 100) (report-every (/ trials 4))
                    (o1 0) (o2 0) (n l))
  (assert (and (>= o1 0)
               (>= o2 0)
               (>= n 0)
               (<= (+ o1 n) l)
               (<= (+ o2 n) l))
          (o2 o2 l)
          "out of bounds")
  (let ((u (make-array l :element-type 'double-float :initial-element 0.0d0))
        (v (make-array l :element-type 'double-float :initial-element 0.0d0)))
    (/ (timing (trials report-every)
         (dot-offset u v o1 o2 n iters))
       n iters)))
