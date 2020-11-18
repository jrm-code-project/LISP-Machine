;;;
;;;
;;; RING-SEMANTICS.LISP
;;;
;;; S = size
;;; O = overflow flag
;;; N = index of next slot
;;; V = number of valid slots (count them backwards from the one before N)
;;; E[i], 0  i < S = contents of slot i
;;;
;;; Create:
;;;   O  NIL
;;;   N  0
;;;   V  0
;;;
;;; Push onto ring:
;;;   ~O & N<S  E[N]  value
;;;              N  N+1
;;;              V  V+1
;;;   ~O & N=S  E[0]  value
;;;              N  1
;;;              O  T
;;;   O         E[N]  value
;;;              N  N+1 mod S
;;;              V  min(V+1, S)
;;;
;;; Pop from ring:
;;;   ~O & N=0  error
;;;   ~O & N>0  N  N-1
;;;              V  V-1
;;;              return E[N]
;;;   O & N=0   V=0  error,
;;;              N  S-1
;;;              V  V-1
;;;              return E[N]
;;;   O & N>0   V=0  error,
;;;              N  N-1
;;;              V  V-1
;;;              return E[N]
;;;
;;; Fast pop algorithm:
;;;   V=0  error,
;;;     N  (N=0  S-1, N-1)
;;;     V  V-1
;;;     return E[N]
