; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :cpuid)

(deftype cpuid-word () '(unsigned-byte 32))

(sb-c:defknown %%cpuid (cpuid-word)
    (values t cpuid-word cpuid-word cpuid-word cpuid-word))
