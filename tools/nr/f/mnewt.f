      SUBROUTINE MNEWT(NTRIAL,X,N,TOLX,TOLF)
      PARAMETER (NP=15)
      DIMENSION X(NP),ALPHA(NP,NP),BETA(NP),INDX(NP)
      DO 13  K=1,NTRIAL
        CALL USRFUN(X,ALPHA,BETA)
        ERRF=0.
        DO 11 I=1,N
          ERRF=ERRF+ABS(BETA(I))
11      CONTINUE
        IF(ERRF.LE.TOLF)RETURN
        CALL LUDCMP(ALPHA,N,NP,INDX,D)
        CALL LUBKSB(ALPHA,N,NP,INDX,BETA)
        ERRX=0.
        DO 12 I=1,N
          ERRX=ERRX+ABS(BETA(I))
          X(I)=X(I)+BETA(I)
12      CONTINUE
        IF(ERRX.LE.TOLX)RETURN
13    CONTINUE
      RETURN
      END