;;; -*- Mode:LISP; Package:VINCULUM; Readtable:CL; Base:10. -*-

;;;*****************************************************************************************************
;;;*****   This file should be identical to conversions(-for-k-debugger) at all times WKF 5/5/88   *****
;;;*****************************************************************************************************

;;; These duplicate versions are needed to fool make-system since COMPILER-FOR-K and K-DEBUGGER
;;;  both use this file and k-debugger loads it in the k-user hierarchy.  WKF

(export '(
          %%quantum-number-in-cluster

          cluster->address
          cluster-number
          cluster-quantum
          quantum->address
          quantum-number
          quantum->cluster
          ))

(defextractor cluster-number %%cluster-number)

(defsubst cluster->address (cluster)
  (hw:dpb cluster %%cluster-number 0.))

(defextractor quantum-number %%quantum-number)

(defsubst quantum->address (quantum)
  (hw:dpb-unboxed quantum %%quantum-number (hw:unboxed-constant 0.)))

(defconstant %%quantum-number-in-cluster (byte (byte-size %%quantum-number)
                                               (- (byte-position %%quantum-number)
                                                  (byte-position %%cluster-number))))


(defsubst cluster-quantum (cluster)
  (hw:ldb cluster %%quantum-number-in-cluster 0))

(defsubst quantum->cluster (quantum)
  (hw:dpb quantum %%quantum-number-in-cluster 0))
