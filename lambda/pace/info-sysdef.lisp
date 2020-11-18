;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defsystem info
  (:package "ZWEI")
  (:pathname-default "dj:pace;")
  (:module info "info")
  (:compile-load info))
