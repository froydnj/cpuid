; -*- mode: lisp; indent-tabs-mode: nil -*-

(defvar *feature-descriptors*)

(defclass feature-descriptor ()
  ((extractor :reader extractor :initarg :extractor)
   (id :reader function-id :initarg :id)))

(defun find-feature-descriptor (name &optional (table *feature-descriptors*))
  (gethash name table))

(defun (setf find-feature-descriptor) (value name
                                       &optional (table *feature-descriptors*))
  (setf (gethash name table) value))

(defun build-feature-descriptors (sexp)
  (labels ((build-extractor (register-name bit)
             #'(lambda (result)
                 (logbitp bit (slot-value result register-name))))
           (install-feature-descriptor (table feature-id
                                        feature-name register bit)
             (let ((extractor (build-extractor register bit)))
               (setf (find-feature-descriptor feature-name table)
                     (make-instance 'feature-descriptor
                                    :extractor extractor
                                    :id feature-id)))))
    (loop with table = (make-hash-table)
          for (feature-id . register-descriptors) in sexp
          do (loop for (register . bit-names) in register-descriptors
                   do (loop for (feature-name bit) in bit-names
                            do (install-feature-descriptor table feature-id
                                                           feature-name
                                                           register bit)))
          finally (return table))))

(defvar *feature-table*
  '(
    (1                                  ; EAX = 1 bit meanings.
     (ecx
      (:sse3 0)
      (:pclmuldq 1)
      (:dtes64 2)
      (:monitor 3)
      (:ds-cpl 4)
      (:vmx 5)
      (:smx 6)
      (:eist 7)
      (:sm2 8)
      (:ssse3 9)
      (:cnxt-id 10)
                                        ; 11 is Reserved
      (:fma 12)
      (:cx16 13)
      (:xtpr 14)
      (:pdcm 15)
                                        ; 16 is Reserved
      (:pcid 17)
      (:dca 18)
      (:sse4.1 19)
      (:sse4.2 20)
      (:x2apic 21)
      (:movbe 22)
      (:popcnt 23)
      (:tsc-deadline 24)
      (:aes 25)
      (:xsave 26)
      (:osxsave 27)
      (:avx 28)
      (:f16c 29)
      (:rdrand 30)
                                        ; Bit 31 returns 0 when running
                                        ; on physical hardware, 1 if
                                        ; running in a hypervisor.
      )
     (edx
      (:fpu 0)
      (:vme 1)
      (:de 2)
      (:pse 3)
      (:tsc 4)
      (:msr 5)
      (:pae 6)
      (:mce 7)
      (:cx8 8)
      (:apic 9)
                                        ; 10 is Reserved.
      (:sep 11)
      (:msrr 12)
      (:pge 13)
      (:mca 14)
      (:cmov 15)
      (:pat 16)
      (:pse-36 17)
      (:psn 18)
      (:clfsh 19)
                                        ; 20 is Reserved.
      (:ds 21)
      (:acpi 22)
      (:mmx 23)
      (:fxsr 24)
      (:sse 25)
      (:sse2 26)
      (:ss 27)
      (:htt 28)
      (:tm 29)
                                        ; 30 is Reserved.
      (:pbe 31)))
    (7
     (ebx
      (:fsgsbase 0)
      (:bmi 3)
      (:hle 4)
      (:avx2 5)
      (:bmi2 8)
      (:rtm 11)
      (:rdseed 18)
      (:adx 19)))))

(setf *feature-descriptors* (build-feature-descriptors *feature-table*))
