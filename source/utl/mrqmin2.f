C
C Some hacked up routines from Numerical Recipes here.  It's been changed
C from a 1-D to a 2-D fitting routine.  (This is simpler than it sounds.
C all the hard bits are in handling the Chi Squared.)  Some other NR routines
C (unmodified) will be pulled in from the NR library.
C
C An initial lambda can now be passed in.  The initial lambda will be set
C to abs(alamda) on the initialization
C
C The matrix solution for the estimated delta parameter has been changed
C from Gauss-Jordan to SVD, since there will be times when this routine will
C be called and not constrain parameters properly, resulting in a singular
C matrix.  Since this commonly happens with circular fits and position
C angles, we don't care that the PA is unconstrained, so long as it doesn't
C shoot down the rest of the fit.
C
C For the moment, I haven't bothered to make the Covariance work with the
C SVD code, since we've not yet had a need for it.  If needed, just set
C the covariance matrix to the pseudo inverse as determined via U, V, & W.
C
      SUBROUTINE MRQMIN2(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     $   COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
      PARAMETER (MMAX=10)
      DIMENSION X1(NDATA),X2(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),
     $   COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX),
     $   U(MMAX,MMAX), W(MMAX), V(MMAX,MMAX)
      IF(ALAMDA.LT.0.)THEN
        KK=MFIT+1
        DO 12 J=1,MA
          IHIT=0
          DO 11 K=1,MFIT
            IF(LISTA(K).EQ.J)IHIT=IHIT+1
11        CONTINUE
          IF (IHIT.EQ.0) THEN
            LISTA(KK)=J
            KK=KK+1
          ELSE IF (IHIT.GT.1) THEN
            PAUSE 'Improper permutation in LISTA'
          ENDIF
12      CONTINUE
        IF (KK.NE.(MA+1)) PAUSE 'Improper permutation in LISTA'
        ALAMDA=ABS(ALAMDA)
        CALL MRQCOF2(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,
     $     CHISQ,FUNCS)
        OCHISQ=CHISQ
        DO 13 J=1,MA
          ATRY(J)=A(J)
13      CONTINUE
      ENDIF
      DO 15 J=1,MFIT
        DO 14 K=1,MFIT
          COVAR(J,K)=ALPHA(J,K)
14      CONTINUE
        COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)
        DA(J)=BETA(J)
15    CONTINUE
C
C      CALL SDGAUSSJ(COVAR,MFIT,NCA,DA,1,1)
C
c      do j = 1, 3
c        print *, (covar(i,j), i=1,3), '    ',beta(j)
c      end do
      DO 110 J = 1, MFIT
         DO 100 I = 1, MFIT
            U(I,J) = COVAR(I,J)
 100     CONTINUE
 110  CONTINUE
      CALL SVDCMP (U, MFIT, MFIT, MMAX, MMAX, W, V)
      WMAX = 0.0
      DO 120 I = 1, MFIT
         IF (W(I).GT.WMAX) WMAX = W(I)
 120  CONTINUE
      WMIN = 1.E-6
      DO 130 I = 1, MFIT
         IF (W(I).LT.WMIN) W(I) = 0.0
 130  CONTINUE
      CALL SVBKSB (U, W, V, MFIT, MFIT, MMAX, MMAX, BETA, DA)
c      print *, 'DA:', (da(i), i=1,3)
C
C End of SVD hacking
C
C
C Careful!  The covariance currently isn't implemented properly.
C
      IF(ALAMDA.EQ.0.)THEN
        CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)
        RETURN
      ENDIF
      DO 16 J=1,MFIT
        ATRY(LISTA(J))=A(LISTA(J))+DA(J)
16    CONTINUE
      CALL MRQCOF2(X1,X2,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,
     $   CHISQ,FUNCS)
      IF(CHISQ.LT.OCHISQ)THEN
        ALAMDA=0.1*ALAMDA
        OCHISQ=CHISQ
        DO 18 J=1,MFIT
          DO 17 K=1,MFIT
            ALPHA(J,K)=COVAR(J,K)
17        CONTINUE
          BETA(J)=DA(J)
          A(LISTA(J))=ATRY(LISTA(J))
18      CONTINUE
      ELSE
        ALAMDA=10.*ALAMDA
        CHISQ=OCHISQ
      ENDIF
      RETURN
      END
C
      SUBROUTINE MRQCOF2(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,
     $   NALP,CHISQ,FUNCS)
      PARAMETER (MMAX=20)
      DIMENSION X1(NDATA),X2(NDATA),Y(NDATA),SIG(NDATA),
     $   ALPHA(NALP,NALP),BETA(MA),DYDA(MMAX),LISTA(MFIT),A(MA)
      DO 12 J=1,MFIT
        DO 11 K=1,J
          ALPHA(J,K)=0.
11      CONTINUE
        BETA(J)=0.
12    CONTINUE
      CHISQ=0.
      DO 15 I=1,NDATA
        CALL FUNCS(X1(I),X2(I),A,YMOD,DYDA,MA)
        SIG2I=1./(SIG(I)*SIG(I))
        DY=Y(I)-YMOD
        DO 14 J=1,MFIT
          WT=DYDA(LISTA(J))*SIG2I
          DO 13 K=1,J
            ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
13        CONTINUE
          BETA(J)=BETA(J)+DY*WT
14      CONTINUE
        CHISQ=CHISQ+DY*DY*SIG2I
15    CONTINUE
      DO 17 J=2,MFIT
        DO 16 K=1,J-1
          ALPHA(K,J)=ALPHA(J,K)
16      CONTINUE
17    CONTINUE
      RETURN
      END
C
C Mild hacks for SDE style error reporting
C
      SUBROUTINE SDGAUSSJ(A,N,NP,B,M,MP)
      PARAMETER (NMAX=50)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                 CALL ERRREPOR ('Fatal Error', 'GAUSSJ',
     $              'Singular matrix')
                 GO TO 999
              END IF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) THEN
           CALL ERRREPOR ('Fatal Error', 'GAUSSJ',
     $        'Singular matrix')
           GO TO 999
        END IF
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
 999  RETURN
      END
