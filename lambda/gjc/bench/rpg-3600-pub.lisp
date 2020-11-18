;;-*- Mode:LISP; Package:USER; Base:10 -*-

;; these are 3600 cpu times from the published gabriel report.

(or (boundp '*benchmark-result-symbols*)
    (setq *benchmark-result-symbols* nil))

(or (memq '*rpg-3600-pub-BENCHMARK* *benchmark-result-symbols*)
    (push '*rpg-3600-pub-BENCHMARK* *benchmark-result-symbols*))

(or (memq '*rpg-3600ifu-pub-BENCHMARK* *benchmark-result-symbols*)
    (push '*rpg-3600ifu-pub-BENCHMARK* *benchmark-result-symbols*))

(or (memq '*rpg-3600fpa-pub-BENCHMARK* *benchmark-result-symbols*)
    (push '*rpg-3600fpa-pub-BENCHMARK* *benchmark-result-symbols*))

(DEFCONST *rpg-3600-pub-BENCHMARK*
          '((boyer-1 18.49)

            (browse-1 41.01)

            (destru-1 3.7)

            (traverse-1 9.45)
            (traverse-2 50.12)

            (tak-1 0.59)

            (stak-1 2.58)

            (ctak-1 7.7)

            (takl-1 6.45)

            (takr-1 0.59)

            (deriv-1 17.54)

            (dderiv-1 18.21)

            (div2-1 5.53)
            (div2-2 6.66)

            (fft-1 4.31)

            (puzzle-1 13.94)

            (triang-1 151.97)

            (fprint-1 2.6)

            (fread-1 4.6)

            (tprint-1 4.89)

            (frpoly-1 0.01)
            (frpoly-2 0.02)
            (frpoly-3 0.01)
            (frpoly-4 0.13)
            (frpoly-5 0.32)
            (frpoly-6 0.13)
            (frpoly-7 1.28)
            (frpoly-8 4.37)
            (frpoly-9 1.29)
            (frpoly-10 7.43)
            (frpoly-11 33.81)
            (frpoly-12 8.09)))




(DEFCONST *rpg-3600ifu-pub-BENCHMARK*
          '((boyer-1 14.6)

            (browse-1 30.13)

            (destru-1 2.64)

            (traverse-1 7.23)
            (traverse-2 35.48)

            (tak-1 0.43)

            (stak-1 2.3)

            (ctak-1 5.08)

            (takl-1 4.95)

            (takr-1 0.43)

            (deriv-1 13.48)

            (dderiv-1 13.87)

            (div2-1 4.39)
            (div2-2 5.41)

            (fft-1 3.5)

            (puzzle-1 11.1)

            (triang-1 116.68)

            (frpoly-1 0.01)
            (frpoly-2 0.02)
            (frpoly-3 0.01)
            (frpoly-4 0.1)
            (frpoly-5 0.25)
            (frpoly-6 0.1)
            (frpoly-7 0.93)
            (frpoly-8 3.3)
            (frpoly-9 1.0)
            (frpoly-10 5.7)
            (frpoly-11 25.08)
            (frpoly-12 6.3)))


(DEFCONST *rpg-3600fpa-pub-BENCHMARK*
          '((boyer-1 18.49)

            (browse-1 41.01)

            (destru-1 3.7)

            (traverse-1 9.45)
            (traverse-2 50.12)

            (tak-1 0.59)

            (stak-1 2.58)

            (ctak-1 7.7)

            (takl-1 6.45)

            (takr-1 0.59)

            (deriv-1 17.54)

            (dderiv-1 18.21)

            (div2-1 5.53)
            (div2-2 6.66)

            (fft-1 3.19)

            (puzzle-1 13.94)

            (triang-1 151.97)

            (fprint-1 2.6)

            (fread-1 4.6)

            (tprint-1 4.89)

            (frpoly-1 0.01)
            (frpoly-2 0.02)
            (frpoly-3 0.01)
            (frpoly-4 0.13)
            (frpoly-5 0.32)
            (frpoly-6 0.13)
            (frpoly-7 1.28)
            (frpoly-8 4.37)
            (frpoly-9 1.29)
            (frpoly-10 7.43)
            (frpoly-11 33.81)
            (frpoly-12 8.09)))
