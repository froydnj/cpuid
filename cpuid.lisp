(in-package :cpuid)

(defvar *feature-descriptors*)

(defclass feature-descriptor ()
  ((extractor :reader extractor :initarg :extractor)
   (id :reader function-id :initarg id)))

(defun find-feature-descriptor (name &optional (table *feature-descriptors*))
  (gethash name table))

(defun (setf find-feature-descriptor) (name value
                                       &optional (table *feature-descriptors*))
  (setf (gethash name table) value))

(defvar *cached-results* (make-hash-table))

(defun find-result (function-id)
  (gethash function-id *cached-results*))

(defun (setf find-result) (function-id value)
  (setf (gethash function-id *cached-results*) value))

(defclass result ()
  ((eax :initarg :eax)
   (ebx :initarg :ebx)
   (ecx :initarg :ecx)
   (edx :initarg :edx)))

(defun query-function (function-id)
  (let ((result (find-result function-id)))
    (cond
      (result result)
      (t
       (multiple-value-bind (eax ebx ecx edx)
           (%cpuid function-id)
         (setf (find-result function-id)
               (make-instance 'result
                              :eax eax :ebx ebx
                              :ecx ecx :edx edx)))))))

(defun featurep (name)
  (let ((fd (find-feature-descriptor name)))
    (cond
      ((null fd) nil)
      (t
       (let* ((function-id (function-id fd))
              (result (query-function function-id)))
         (funcall (extractor fd) result))))))

(defun build-feature-descriptors (sexp)
  (labels ((build-extractor (register-name bit)
             #'(lambda (result)
                 (logbitp bit (slot-value result register-name))))
           (install-feature-descriptor (table feature-name register bit)
             (let ((extractor (build-extractor register bit)))
               (setf (find-feature-descriptor feature-name table)
                     (make-instance 'feature-descriptor
                                    :extractor extractor
                                    :id feature-id)))))
    (loop with table = (make-hash-table)
          for (feature-id register-descriptors) in sexp
          do (loop for (register bit-names) in register-descriptors
                   do (loop for (feature-name bit) in bit-names
                            do (install-feature-descriptor table feature-name
                                                           register bit)))
          finally (return table))))
