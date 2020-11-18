

                            Gabriel Benchmarks
                            ==================


Benchmark         3600  w/IFU   Sun 3/260M    Fastest     K             x

TAK               0.60   0.43      0.24       0.044 C    0.031  (1)    1.4
STAK              2.58   2.30      1.14       1.13  C    0.1    (3)   11
CTAK              7.65   5.04      0.90       0.59  C
TAKL              6.44   4.95      1.32       0.30  C    0.5    (1)    0.6
TAKR              0.60   0.43      0.36       0.06  C
BOYER            11.99   9.40      6.58       1.85  C    1.39   (1,2)  1.3
BROWSE           30.80  21.43     22.1        3.84  I
DESTRU            3.03   2.18      1.20       0.44  C
TRAVERSE Init     8.62   6.37      4.60       1.80  I
TRAVERSE         49.95  35.34     25.24       9.89  I
DERIV             5.12   3.79      1.98       1.06  I    0.39   (1)    2.7
DDERIV            5.24   3.89      2.62       1.22  I
DIV2 Iterative    1.85   1.51      0.6        0.45  I    0.23   (1)    1.9
DIV2 Recursive    2.89   2.50      0.88       0.42  I    0.23   (1)    1.8
FFT               4.75   3.87     69.46       1.57  D
PUZZLE           13.89  11.04      4.30       1.00  C
TRIANG          151.70 116.99     71.83      14.44  C   15.8    (3)    0.9
FPRINT            2.60    -        0.82       0.34  I
FREAD             4.60    -        2.20       0.40  I
TPRINT            4.89    -        0.98       0.20  C


C = PSL Cray-XMP
I = PSL IBM 3081
D = Xerox Dorado

(1) Compiled and simulated, with primitive instruction counts:
      ATOM = 2
      CAR = 3
      CADR,CDDR = 5
      CADDR = 7
      RPLACD = 3
      CONS = 11
      NCONS = 11
      LIST(n) = 11n
(2) Not including GC, 226K conses
(3) Calculated from meter data
