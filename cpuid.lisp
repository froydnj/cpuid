; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :cpuid)

(defvar *cached-results* (make-hash-table))

(defun find-result (function-id)
  (gethash function-id *cached-results*))

(defun (setf find-result) (value function-id)
  (setf (gethash function-id *cached-results*) value))

(defclass result ()
  ((eax :initarg :eax)
   (ebx :initarg :ebx)
   (ecx :initarg :ecx)
   (edx :initarg :edx))
  (:default-initargs :eax 0 :ebx 0 :ecx 0 :edx 0))

(defun %cpuid (function-id)
  #+(and sbcl x86-64)
  (%%cpuid function-id)
  #-(and sbcl x86-64)
  (values nil 0 0 0 0))

(defun query-function (function-id)
  (let ((result (find-result function-id)))
    (cond
      (result result)
      (t
       (multiple-value-bind (supports-cpuid-p eax ebx ecx edx)
           (%cpuid function-id)
         (if (not supports-cpuid-p)
             (let ((null-result
                     (load-time-value
                      (make-instance 'result))))
               null-result)
             (setf (find-result function-id)
                   (make-instance 'result
                                  :eax eax :ebx ebx
                                  :ecx ecx :edx edx))))))))

(defun featurep (name)
  (let ((fd (find-feature-descriptor name)))
    (cond
      ((null fd) nil)
      (t
       (let* ((function-id (function-id fd))
              (result (query-function function-id)))
         (funcall (extractor fd) result))))))
