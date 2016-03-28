C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matinv.f	1.2    3/29/94
C
      SUBROUTINE MATINV (IN, OUT)
C
CD Rock stupid Gauss Jordan matrix inverse.
C
C	IN	CH*(*)	input	Name of input array
C	OUT	CH*(*)	input	Name of output array
C
C Audit trail:
C	Initial version
C				D.S. Briggs	5 Nov 1992
C	Fix namespace collision with modified version of NR routine.
C				D.S.Briggs	March 29 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATINV')
C
      CHARACTER*1	ATYPE
      INTEGER		IERR, NAX, RNAX, NAXIS(SYSMXDIM),
     $			IADD, OADD
C
      INTEGER		CRDRNAX, DATADD
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, IADD)
      RNAX = CRDRNAX(NAX,NAXIS)
C
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input array has bad number of axes')
         GO TO 999
      END IF
C
C Make output array if it does not exist, and set the initial pixels
C
      IF (OUT.NE.IN) CALL ARRCOPY (IN, OUT)
      OADD = DATADD(OUT)
      IF (ERROR) GO TO 999
C
C Now call the NR code
C
      IF (ATYPE.EQ.'R') THEN
         CALL MGAUSSJ (MEMR(OADD), NAXIS(1), NAXIS(1), MEMR(1),
     $      0, 0, IERR)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
      IF (IERR.GT.0) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Singular matrix')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
C Code from Numerical Recipes
C
      SUBROUTINE MGAUSSJ(A,N,NP,B,M,MP,IERR)
      PARAMETER (NMAX=128)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      IERR = 0
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
                IERR = 1
                GO TO 999
              ENDIF
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
          IERR = 1
          GO TO 999
        ENDIF
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
999   RETURN
      END



