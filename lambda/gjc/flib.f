
C**************************************
C* Copyright LISP Machine, Inc. 1985  *
C*  See filename "Copyright" for      *
C* licensing and release information. *
C**************************************
       SUBROUTINE VADD(X,Y,Z,N)
       REAL X(N),Y(N),Z(N)
       INTEGER N
       DO 100 J=1,N
100    Z(J) = X(J) + Y(J)
       RETURN
       END
       SUBROUTINE VMULT(X,Y,Z,N)
       REAL X(N),Y(N),Z(N)
       INTEGER N
       DO 100 J=1,N
100    Z(J) = X(J) * Y(J)
       RETURN
       END
       SUBROUTINE VPRINT(X,N)
       REAL X(N)
       INTEGER N
       DO 100 J=1,N
100    WRITE(6,200) J,X(J)
200    FORMAT(' X(',I3,')=',E13.3)
       RETURN
       END
