;;;-*- Mode:LISP; Base:10; Readtable:ZL -*-

(defsystem falcon-debugger
  (:name "K-Debugger")
  (:pathname-default "jb:youcef.k;")
  (:module K-GEneric "kbug-generic-patch")
  (:module K-BUG2 "Kbug2-patch")
  (:module Wind "Falcon-debugger")
  (:compile-load k-generic)
  (:compile-load k-bug2)
  (:compile-load Wind ((:fasload k-generic k-bug2)))
  )
