C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)svdsolv.f	1.1	 5/4/93
C
      SUBROUTINE SVDSOLV (MAT, XX, BB, N, ND)
C
CD Finds X as in MAT * X = B
C
C	MAT	R(ND,ND)	in/out	In Matrix, Returns Inverse Matrix
C	N	INT		input	Size of Matrix (square)
C	ND	INT		input	Dimensioned size of array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	Sept 9 1992
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N, ND
      REAL		MAT(ND, *), XX(*), BB(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SVDSOLV')
C
      INTEGER		I, J
      INTEGER		NMAX, NMAX2
      PARAMETER		(NMAX=80)
      PARAMETER		(NMAX2=80*80)
C      
      REAL		A(NMAX, NMAX), U(NMAX,NMAX)
      REAL		W(NMAX), V(NMAX, NMAX), B(NMAX), X(NMAX)
      REAL		WMAX, WMIN
C=====================================================================
      IF (ERROR) GOTO 999
C
      IF (N .GT. NMAX) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input matrix too big to handle')
         GOTO 999
      ENDIF
C
C blank out work arrays
C
      CALL PIXRSETC (A, 0,0, NMAX2)
      CALL PIXRSETC (U, 0,0, NMAX2)
      CALL PIXRSETC (V, 0,0, NMAX2)
      CALL PIXRSETC (W, 0,0, NMAX)
      CALL PIXRSETC (B, 0,0, NMAX)
      CALL PIXRSETC (X, 0,0, NMAX)
C
C Fill input data into work arrays
C
      DO 200 J=1,N
         B(J) = BB(J)
         DO 100 I=1,N
            A(I,J) = MAT(I,J)
 100     CONTINUE
 200  CONTINUE
C
      DO 312 I=1,N
         DO 311 J=1,N
            U(I,J) = A(I,J)
 311     CONTINUE
 312  CONTINUE
      CALL SVDCMP (U,N,N,NMAX,NMAX,W,V)
      WMAX = 0.
      DO 313 J=1,N
         IF(W(J).GT.WMAX)WMAX=W(J)
 313  CONTINUE
      WMIN = WMAX*1.0E-2
      DO 314 J=1,N
         IF (W(J).LT.WMIN) W(J)=0.0
 314  CONTINUE
      CALL SVBKSB (U,W,V,N,N,NMAX,NMAX,B,X)
C
C fill work array into output array
C
      DO 500 J=1,N
         XX(J) = X(J)
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

