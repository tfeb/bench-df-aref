;;;; Test of displaced array reference
;;;

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.iterate :org.tfeb.hax.utilities)
  :compile t))

(defpackage :ts-dar
  (:use :cl)
  (:use
   :org.tfeb.hax.iterate
   :org.tfeb.hax.utilities))

(in-package :ts-dar)

(defun call/base-array (f a)
  (iterate next ((b a) (o 0))
    (multiple-value-bind (bb oo) (array-displacement b)
      (if bb
          (next bb (+ o oo))
        (funcall f b o)))))

(defmacro with-base-array ((b o &optional accessor-macro-name) a
                           &body decls/forms)
  (if accessor-macro-name
      (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
        `(call/base-array
          (lambda (,b ,o)
            ,@decls
            (macrolet ((,accessor-macro-name (i)
                         `(aref ,',b (+ ,i ,',o))))
              ,@forms))
          ,a))
      `(call/base-array
        (lambda (,b ,o)
          ,@decls/forms)
        ,a)))

(defun tssa (a n)
  ;; Simple
  (declare (type (simple-array double-float (*)) a)
           (type fixnum n)
           (optimize speed (safety 0)
                     #+LispWorks (float 0)))
  (let ((s 0.0d0)
        (l (length a)))
    (declare (type double-float s)
             (type fixnum l))
    (dotimes (k n)
      (dotimes (i l)
        (declare (type fixnum i))
        (incf s (aref a i))))
    s))

(defun tsoa (a n o l)
  ;; Simple, offset
  (declare (type (simple-array double-float (*)) a)
           (type fixnum n o l)
           (optimize speed (safety 0)
                     #+LispWorks (float 0)))
  (let ((s 0.0d0))
    (declare (type double-float s)
             (type fixnum l))
    (dotimes (k n)
      (dotimes (i l)
        (declare (type fixnum i))
        (incf s (aref a (+ i o)))))
    s))

(defun tsga (a n)
  ;; General
  (declare (type (array double-float (*)) a)
           (type fixnum n)
           (optimize speed (safety 0)
                     #+LispWorks (float 0)))
  (let ((s 0.0d0)
        (l (length a)))
    (declare (type double-float s)
             (type fixnum l))
    (dotimes (k n)
      (dotimes (i l)
        (declare (type fixnum i))
        (incf s (aref a i))))
    s))

(defun tsda (a n)
  ;; Displaced, perhaps: slow in LW.
  (declare (type (array double-float (*)) a) ;in fact displaced
           (type fixnum n)
           (optimize speed (safety 0)
                     #+LispWorks (float 0)))
  (with-base-array (b o ref) a
    (declare (type (simple-array double-float (*)) b)
             (type fixnum o)
             (optimize speed (safety 0)
                       #+LispWorks (float 0)))
    (let ((s 0.0d0)
          (l (length a)))
      (declare (type double-float s)
               (type fixnum l))
      (dotimes (k n)
        (dotimes (i l)
          (declare (type fixnum i))
          (incf s (ref i))))
      s)))

(defun tsda/tsoa (a n)
  ;; Displaced, perhaps, call tsoa to do the work: fast in LW.
  (declare (type (array double-float (*)) a) ;in fact displaced
           (type fixnum n)
           (optimize speed (safety 0)
                     #+LispWorks (float 0)))
  (with-base-array (b o) a
    (declare (type (simple-array double-float (*)) b)
             (type fixnum o))
    (tsoa b n o (length a))))


#||
(org.tfeb.tools.require-module:needs
 (:org.tfeb.tools.timing :compile t :use t))

(defparameter *a* (make-array 100000 :element-type 'double-float
                                     :initial-element 0.0d0))

(defparameter *b* (make-array 100000 :element-type 'double-float
                                    :displaced-to *a*
                                    :displaced-index-offset 0))

(values
 (timing (600 30)
   (tssa *a* 10000))
 (timing (600 30)
   (tsda *b* 10000)))
||#
