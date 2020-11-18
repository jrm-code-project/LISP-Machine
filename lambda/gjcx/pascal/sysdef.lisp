;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-



(defsystem pascal
  (:pathname-default "pascal:source;")
  (:module parser "parser")
  (:module grammar "grammar")
  (:module tokenizer "tokenizer")
  (:module translator "translator")
  (:compile-load "parser")
  (:compile-load "grammar" (:fasload parser) (:fasload parser))
  (:compile-load "tokenizer")
  (:compile-load "translator"))

;(load "parser")
;(load-grammar "pascal-grammar")
;(load "trans")

;(load "pascal-reader")
;(initialize-ptol-readtable)
