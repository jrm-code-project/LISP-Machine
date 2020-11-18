;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-



(defsystem netpkg
  (:pathname-default "netpkg:source;")
  (:module xdr "xdr")
  (:module rpc "rpc")
  (:module applications ("port-mapper" "mount" "nfs" "nfs-server" "fs-extensions"))
  (:compile-load xdr)
  (:compile-load rpc (:fasload xdr) (:fasload xdr))
  (:compile-load applications (:fasload rpc) (:fasload rpc)))
