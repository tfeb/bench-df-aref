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

(defparameter *best* 'dot-offset-smarter) ;just

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
  (testing-dotters (dotter (dot-offset dot-offset-smarter))
    (and
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
           (= (dot-offset-smarter a a 0 0 1 10) 40.0d0))))
     (let ((a (make-array 1 :element-type 'double-float :initial-element 2.0d0
                          :allocation ':pinnable)))
       (with-pinned-objects (a)
         (= (dotter a a 0 0 1 1) 4.0d0))
     (let ((a (make-array 3 :element-type 'double-float :initial-element 2.0d0
                          :allocation ':pinnable)))
       (with-pinned-objects (a)
         (= (dotter a a 0 0 3 1) 12.0d0)))))))

(defun benchit (l &key (trials 100) (iters 100) (report-every (/ trials 4))
                    (o1 0) (o2 0) (n l) (what *best*))
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
    (macrolet ((choose (&body names)
                 `(case what
                    ,@(mapcar (lambda (name)
                                `(,name
                                  (with-pinned-objects (u v)
                                    (/ (timing (trials report-every)
                                               (,name u v o1 o2 n iters))
                                       n iters))))
                                names)
                       (otherwise
                        (error "~S isn't one of ~{~S~^, ~}" what ',names)))))
      (choose
       dot-offset dot-offset-smarter))))

(defun benchem ()
  (tsit)
  (dolist (what '(dot-offset dot-offset-smarter))
    (format t "~&~30S" what)
    (finish-output)
    (format t "~D~%"
            (benchit 1000000 :o1 300000 :o2 600000 :n 200000
                     :what what :report-every nil))))
