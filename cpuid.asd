; -*- mode:lisp; indent-tabs-mode: nil -*-

(cl:defpackage :cpuid-system
  (:use :cl))

(cl:in-package :cpuid-system)

(defclass cpuid-source-file (asdf:cl-source-file)
  ((feature-depends-on :initarg :feature-depends-on)))

(defun has-feature (f)
  (labels ((sub-has-feature (x)
             (cond
               ((consp x)
                (ecase (car x)
                  ((:and and) (every #'sub-has-feature (cdr x)))
                  ((:or or) (some #'sub-has-feature (cdr x)))))
               ((symbolp x)
                (find x *features*))
               (t
                nil))))
    (sub-has-feature f)))

;; I'm not sure that INPUT-FILES and OUTPUT-FILES definitions are needed
;; once you have PERFORM, but include them for completeness anyway.
(defmethod asdf:input-files ((op asdf:compile-op) (c cpuid-source-file))
  (if (and (slot-boundp c 'feature-depends-on)
           (has-feature (slot-value c 'feature-depends-on)))
      (call-next-method)
      nil))

(defmethod asdf:output-files ((op asdf:compile-op) (c cpuid-source-file))
  (if (and (slot-boundp c 'feature-depends-on)
           (has-feature (slot-value c 'feature-depends-on)))
      (call-next-method)
      nil))

(defmethod asdf:perform ((op asdf:compile-op) (c cpuid-source-file))
  (if (and (slot-boundp c 'feature-depends-on)
           (has-feature (slot-value c 'feature-depends-on)))
      (call-next-method)
      nil))

;; This should really be all that's necessary, but OPERATION-DONE-P on
;; modules doesn't recurse on the module's components.  Which does make
;; sense, but it's inconvenient in our case.  Nevertheless, leave this
;; here for completeness.
(defmethod asdf:operation-done-p ((op asdf:operation) (c cpuid-source-file))
  (if (slot-boundp c 'feature-depends-on)
      (if (not (has-feature (slot-value c 'feature-depends-on)))
          t
          (call-next-method))
      (call-next-method)))

(asdf:defsystem :cpuid
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for accessing feature bits from the CPUID instruction on x86 processors"
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "feature-descriptors" :depends-on ("package"))
               (:cpuid-source-file "fndb" :depends-on ("package")
                      :feature-depends-on (and :sbcl :x86-64))
               (:cpuid-source-file "x86-64-vm" :depends-on ("fndb")
                      :feature-depends-on (and :sbcl :x86-64))
               (:file "cpuid" :depends-on ("feature-descriptors"))))
