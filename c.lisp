;;;; Double float dot product calling C: LW only
;;;

(in-package :cl-user)

(needs (:org.tfeb.tools.timing :compile t :use t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-LispWorks
  (error "not LW")
  #+LispWorks
  (use-package :fli))

(let ((m "dp.dylib"))
  (register-module
   m
   :file-name  (merge-pathnames (pathname m) *load-truename*)
   :connection-style ':immediate))

(define-foreign-function (dot-offset "dot_offset")
    ((u :lisp-array)
     (v :lisp-array)
     (ou :int)
     (ov :int)
     (n :int)
     (iters :int))
  :result-type :double
  :language :ansi-c)

(define-foreign-function (dot-offset-smarter "dot_offset_smarter")
    ((u :lisp-array)
     (v :lisp-array)
     (ou :int)
     (ov :int)
     (n :int)
     (iters :int))
  :result-type :double
  :language :ansi-c)

(defun tsit ()
  (let ((a (make-array 100 :element-type 'double-float :initial-element 0.0d0
                       :allocation ':pinnable)))
    (setf (aref a 0) 2.0d0)
    (with-pinned-objects (a)
      (and (= (dot-offset a a 0 0 10 1) 4.0d0)
           (= (dot-offset a a 1 1 9 1) 0.0d0)
           (= (dot-offset a a 0 0 1 1) 4.0d0)
           (= (dot-offset a a 0 0 1 10) 40.0d0)
           (= (dot-offset-smarter a a 0 0 10 1) 4.0d0)
           (= (dot-offset-smarter a a 1 1 9 1) 0.0d0)
           (= (dot-offset-smarter a a 0 0 1 1) 4.0d0)
           (= (dot-offset-smarter a a 0 0 1 10) 40.0d0)))))

(defun benchit (l &key (trials 100) (iters 100) (report-every (/ trials 4))
                    (o1 0) (o2 0) (n l) (smarter nil))
  (assert (and (>= o1 0)
               (>= o2 0)
               (>= n 0)
               (<= (+ o1 n) l)
               (<= (+ o2 n) l))
          (o2 o2 l)
          "out of bounds")
  (let ((u (make-array l :element-type 'double-float :initial-element 0.0d0
                       :allocation ':pinnable))
        (v (make-array l :element-type 'double-float :initial-element 0.0d0
                       :allocation ':pinnable)))
    (with-pinned-objects (u v)
      (if smarter
          (/ (timing (trials report-every)
           (dot-offset-smarter u v o1 o2 n iters))
         n iters)
        (/ (timing (trials report-every)
             (dot-offset u v o1 o2 n iters))
           n iters)))))
