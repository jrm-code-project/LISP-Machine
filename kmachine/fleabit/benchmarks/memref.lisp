

Memory References

TAK    .037s 500K     0

takl   .5    6.7M     867k reads

iterative-div2   .23 3.1M 360k reads (cddr) 240k writes (cons)
recursive-div2   .23 3.1M  "                  "

boyer  1.39s 18.5 M   2.5M reads (car,cdr)+ 240K (get)+ ~16k (equal member)    454K write (cons)

browse (estimated)
       16M (seems wrong) say 24M   1.8M (car,cdr) 300K (nconc) 454k (char1)     writes  480K (cons) 70k (nconc)

deriv .39s 5M  195k reads 130k writes

triang  15.8s 210M   27M 6M  (50M)=

inst/mref

tak    +inf
takl   8
div2   5
boyer  6
browse 8
deriv 15
