; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package :sb-vm)

(define-vop (%cpuid)
  (:translate cpuid::%%cpuid)
  (:policy :fast-safe)
  (:args (function-id :scs (unsigned-reg) :target eax))
  (:arg-types unsigned-num)
  (:results (supportedp :scs (descriptor-reg))
            (eax-result :scs (unsigned-reg))
            (ebx-result :scs (unsigned-reg))
            (ecx-result :scs (unsigned-reg))
            (edx-result :scs (unsigned-reg)))
  (:result-types t
                 unsigned-num unsigned-num
                 unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :target eax-result :offset rax-offset) eax)
  (:temporary (:sc unsigned-reg :target ebx-result :offset rbx-offset) ebx)
  (:temporary (:sc unsigned-reg :target ecx-result :offset rcx-offset) ecx)
  (:temporary (:sc unsigned-reg :target edx-result :offset rdx-offset) edx)
  (:generator 100
    (inst mov eax function-id)
    (inst cpuid)
    (inst lea supportedp
          (make-ea :qword
                   :disp (+ nil-value (static-symbol-offset t))))
    (inst mov eax-result eax)
    (inst mov ebx-result ebx)
    (inst mov ecx-result ecx)
    (inst mov edx-result edx)))
