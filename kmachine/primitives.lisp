;;; -*- Mode:LISP; Package:HARDWARE; Base:10; Readtable:CL -*-

;;; >>>>>>>> THIS PACKAGE DOES NOT "USE" GLOBAL. <<<<<<<<<<<<
;;; It would trash a number of lisp functions if it did.
;;; Certain global symbols are imported here for convenience.

;;; The following are the primitives of the K machine.
;;; They are in alphabetical order because it seems just as
;;; ad hoc as any other order.  At least this way it is easy
;;; to find something.

(deff 32=                       'sim:k-32=)

(deff dpb                       'sim:k-dpb)

(deff dpb-and                   'sim:k-dpb-and)
(deff dpb-and-boxed             'sim:k-dpb-and-boxed)
(deff dpb-and-boxed-left        'sim:k-dpb-and-boxed-left)
(deff dpb-and-unboxed           'sim:k-dpb-and-unboxed)

(deff dpb-boxed                 'sim:k-dpb-boxed)
(deff dpb-boxed-left            'sim:k-dpb-boxed-left)

(deff dpb-ior                   'sim:k-dpb-ior)
(deff dpb-ior-boxed             'sim:k-dpb-ior-boxed)
(deff dpb-ior-boxed-left        'sim:k-dpb-ior-boxed-left)
(deff dpb-ior-unboxed           'sim:k-dpb-ior-unboxed)

(deff dpb-not                   'sim:k-dpb-not)
(deff dpb-not-boxed             'sim:k-dpb-not-boxed)
(deff dpb-not-boxed-left        'sim:k-dpb-not-boxed-left)
(deff dpb-not-unboxed           'sim:k-dpb-not-unboxed)

(deff dpb-unboxed               'sim:k-dpb-unboxed)

(deff dpb-xor                   'sim:k-dpb-xor)
(deff dpb-xor-boxed             'sim:k-dpb-xor-boxed)
(deff dpb-xor-boxed-left        'sim:k-dpb-xor-boxed-left)
(deff dpb-xor-unboxed           'sim:k-dpb-xor-unboxed)

(deff ldb                       'sim:k-ldb)

(deff ldb-and                   'sim:k-ldb-and)
(deff ldb-and-boxed             'sim:k-ldb-and-boxed)
(deff ldb-and-boxed-left        'sim:k-ldb-and-boxed-left)
(deff ldb-and-unboxed           'sim:k-ldb-and-unboxed)

(deff ldb-boxed                 'sim:k-ldb-boxed)
(deff ldb-boxed-left            'sim:k-ldb-boxed-left)

(deff ldb-ior                   'sim:k-ldb-ior)
(deff ldb-ior-boxed             'sim:k-ldb-ior-boxed)
(deff ldb-ior-boxed-left        'sim:k-ldb-ior-boxed-left)
(deff ldb-ior-unboxed           'sim:k-ldb-ior-unboxed)

(deff ldb-not                   'sim:k-ldb-not)
(deff ldb-not-boxed             'sim:k-ldb-not-boxed)
(deff ldb-not-boxed-left        'sim:k-ldb-not-boxed-left)
(deff ldb-not-unboxed           'sim:k-ldb-not-unboxed)

(deff ldb-unboxed               'sim:k-ldb-unboxed)

(deff ldb-xor                   'sim:k-ldb-xor)
(deff ldb-xor-boxed             'sim:k-ldb-xor-boxed)
(deff ldb-xor-boxed-left        'sim:k-ldb-xor-boxed-left)
(deff ldb-xor-unboxed           'sim:k-ldb-xor-unboxed)

(deff md-start-write-no-gc-write-test           'sim:k-md-start-write-option-0)
(deff md-start-write                            'sim:k-md-start-write-option-1)
(deff md-start-write-unboxed-no-gc-write-test   'sim:k-md-start-write-unboxed-option-0)
(deff md-start-write-unboxed                    'sim:k-md-start-write-unboxed-option-1)

(deff read-gc-ram               'sim:k-read-gc-ram)
(deff read-map                  'sim:k-read-map)
(deff read-md                   'sim:k-read-md)
(deff read-memory-control       'sim:k-read-memory-control)
(deff read-memory-status        'sim:k-read-memory-status)
(deff read-processor-control    'sim:k-read-processor-control)
(deff read-processor-status     'sim:k-read-processor-status)
(deff read-transporter-ram      'sim:k-read-transporter-ram)
(deff read-trap                 'sim:k-read-trap)
(deff read-vma                  'sim:k-read-vma)

(deff vma-start-read-cdr                    'sim:k-vma-start-read-cdr-option-0)
(deff vma-start-read-cdr-no-transport                  'sim:k-vma-start-read-cdr-option-1)
(deff vma-start-read-cdr-visible-evcp                    'sim:k-vma-start-read-cdr-option-2)
(deff vma-start-read-cdr-will-write                    'sim:k-vma-start-read-cdr-option-3)

(deff vma-start-read-md-unboxed-cdr         'sim:k-vma-start-read-md-unboxed-cdr-option-0)
(deff vma-start-read-md-unboxed-cdr-no-transport         'sim:k-vma-start-read-md-unboxed-cdr-option-1)
(deff vma-start-read-md-unboxed-cdr-visible-evcp         'sim:k-vma-start-read-md-unboxed-cdr-option-2)
(deff vma-start-read-md-unboxed-cdr-will-write         'sim:k-vma-start-read-md-unboxed-cdr-option-3)

(deff vma-start-read-md-unboxed             'sim:k-vma-start-read-md-unboxed-option-0)
(deff vma-start-read-md-unboxed-no-transport             'sim:k-vma-start-read-md-unboxed-option-1)
(deff vma-start-read-md-unboxed-visible-evcp             'sim:k-vma-start-read-md-unboxed-option-2)
(deff vma-start-read-md-unboxed-will-write             'sim:k-vma-start-read-md-unboxed-option-3)

(deff vma-start-read                        'sim:k-vma-start-read-option-0)
(deff vma-start-read-no-transport                        'sim:k-vma-start-read-option-1)
(deff vma-start-read-visible-evcp                        'sim:k-vma-start-read-option-2)
(deff vma-start-read-will-write                        'sim:k-vma-start-read-option-3)

(deff vma-start-read-unboxed-cdr            'sim:k-vma-start-read-unboxed-cdr-option-0)
(deff vma-start-read-unboxed-cdr-no-transport            'sim:k-vma-start-read-unboxed-cdr-option-1)
(deff vma-start-read-unboxed-cdr-visible-evcp            'sim:k-vma-start-read-unboxed-cdr-option-2)
(deff vma-start-read-unboxed-cdr-will-write            'sim:k-vma-start-read-unboxed-cdr-option-3)

(deff vma-start-read-unboxed-md-unboxed-cdr
             'sim:k-vma-start-read-unboxed-md-unboxed-cdr-option-0)
(deff vma-start-read-unboxed-md-unboxed-cdr-no-transport
             'sim:k-vma-start-read-unboxed-md-unboxed-cdr-option-1)
(deff vma-start-read-unboxed-md-unboxed-cdr-visible-evcp
             'sim:k-vma-start-read-unboxed-md-unboxed-cdr-option-2)
(deff vma-start-read-unboxed-md-unboxed-cdr-will-write
             'sim:k-vma-start-read-unboxed-md-unboxed-cdr-option-3)

(deff vma-start-read-unboxed-md-unboxed     'sim:k-vma-start-read-unboxed-md-unboxed-option-0)
(deff vma-start-read-unboxed-md-unboxed-no-transport     'sim:k-vma-start-read-unboxed-md-unboxed-option-1)
(deff vma-start-read-unboxed-md-unboxed-visible-evcp     'sim:k-vma-start-read-unboxed-md-unboxed-option-2)
(deff vma-start-read-unboxed-md-unboxed-will-write     'sim:k-vma-start-read-unboxed-md-unboxed-option-3)

(deff vma-start-read-unboxed                'sim:k-vma-start-read-unboxed-option-0)
(deff vma-start-read-unboxed-no-transport                'sim:k-vma-start-read-unboxed-option-1)
(deff vma-start-read-unboxed-visible-evcp                'sim:k-vma-start-read-unboxed-option-2)
(deff vma-start-read-unboxed-will-write                'sim:k-vma-start-read-unboxed-option-3)

(deff vma-start-write-no-gc-trap               'sim:k-vma-start-write-option-0)
(deff vma-start-write                          'sim:k-vma-start-write-option-1)
(deff vma-start-write-unboxed-no-gc-trap       'sim:k-vma-start-write-unboxed-option-0)
(deff vma-start-write-unboxed                  'sim:k-vma-start-write-unboxed-option-1)

(deff write-gc-ram              'sim:k-write-gc-ram)
(deff write-map                 'sim:k-write-map)
(deff write-md-boxed            'sim:k-write-md-boxed)
(deff write-md-unboxed          'sim:k-write-md-unboxed)
(deff write-memory-control      'sim:k-write-memory-control)
(deff write-processor-control   'sim:k-write-processor-control)
(deff write-transporter-ram     'sim:k-write-transporter-ram)
(deff write-vma-boxed           'sim:k-write-vma-boxed)
(deff write-vma-unboxed         'sim:k-write-vma-unboxed)
