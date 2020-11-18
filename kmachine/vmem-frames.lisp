;;; -*- Mode:LISP; Package:VIRTUAL-MEMORY; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Virtual memory SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-global-frame vmem)

(define-global-variable vmem *temporary-map-entry*)

(define-global-variable vmem *physical-cluster-free-pointer*)
(define-global-variable vmem *physical-cluster-table*)
(define-global-variable vmem *physical-cluster-free-list*)
(define-global-variable vmem *physical-cluster-free-clusters*)
(define-global-variable vmem *physical-cluster-initially-wired-pointer*)

(define-global-variable vmem *quantum-map*)

(define-global-variable vmem *region-bits*)

(define-global-variable vmem *swapping-quantum*)
