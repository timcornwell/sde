C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlftqdr.f	1.2    6/8/94
C
      SUBROUTINE UTLFTQDR (X, Y, N, A, B, C, D, E, F)
C
CD Fit a quadratic form to a set of data point
C
C                          2             2
C The quadratic form is A X  + B XY + C Y  + D X + E Y + F = 0
C
C There are actually five independent parameters in this fit, A & C
C being related by A = 1+a and C = 1-a
C
C This routine is based around the routine FITQDR posted to
C sci.math.num-analysis by Daniel Pfenniger of Geneva Observatory
C in approximately April of 1992
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	10 July 1993
C	Lawson & Hanson routines broken out to utl/lawson.f
C				D.S.Briggs	June 8 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER	N
      REAL	X(N), Y(N)
      REAL	A, B, C, D, E, F
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLFTQDR')
C
      REAL	RNORM
      INTEGER	TADD, KRANK
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATMAKAR ('UTLFTQDR-TMP', 1, 6*N, 'R', TADD)
      IF (ERROR) GO TO 990
C
      CALL FITQDR (N, N, MEMR(TADD+N), MEMR(TADD), X, Y, KRANK, RNORM)
C
      IF (KRANK.NE.5) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Data is not full rank')
         GO TO 999
      END IF
C
      A = 1. + MEMR(TADD)
      B = MEMR(TADD+1)
      C = 1. - MEMR(TADD)
      D = MEMR(TADD+2)
      E = MEMR(TADD+3)
      F = MEMR(TADD+4)
C
      CALL DATDELET ('UTLFTQDR-TMP')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

C-----------------------------------------------------------------------
C
      SUBROUTINE FITQDR(N,NMAX,A,B,X,Y,KRANK,RNORM)
C
C     Fortran subroutine using the linear least squares routine HFTI 
C     finding the parameters  a, c, d, e, f  defining the best 
C     quadratic form 
C
C            (1+a) X^2 + (1-a) Y^2 + c X Y + d X + e Y + f = 0
C
C     passing through a set of data points {Xi,Yi} i=1,..N 
C     The result is invariant by rotation and translation.
C     If N<6, this program finds an exact solution passing through 5
C             points
C     If N>5, it finds the quadratic form which minimizes
C
C            SUM(i=1,N)  [ (1+a)X^2 + (1-a)Y^2 + c X Y + d X + e Y + f ]^2
C
C     N     : Actual number of data points
C     NMAX  : Maximum number of data points
C     NPAR  : Number of parameters to find (5)
C     A, B  : Work arrays
C     X, Y  : Data point coordinates
C     KRANK : Rank of the data matrix A (should be 5)
C     RNORM : Norm of the residual
C
C     TAU   : tolerance for a zero in HFTI
C
C     On outpout  a,c,d,e,f  are in B(1),B(2), ... B(5) respectively
C
C     Daniel Pfenniger, Geneva Observatory, 6/1991
C
      PARAMETER (NPAR=5, TAU=1E-7)
C
      DIMENSION A(NMAX,NPAR), B(NMAX), X(NMAX), Y(NMAX)
C
C     H, G, IP : Work arrays for HFTI
C
      DIMENSION H(NPAR), G(NPAR), IP(NPAR)
C
C     Building the matrices A and B 
C
      DO 10 I = 1, N
        A(I,1) = X(I)**2-Y(I)**2
        A(I,2) = X(I)*Y(I)
        A(I,3) = X(I)
        A(I,4) = Y(I)
        A(I,5) = 1. 
   10   B(I) = -(X(I)**2+Y(I)**2)
C
C     Solving the LS problem: ||A Z - B|| minimum
C     Call HFTI, the result Z is in the first NPAR rows of B
C     TAU is the tolerance for zero
C     KRANK is the rank of A (should be NPAR)
C     RNORM is the norm of the residual
C     See Lawson & Hanson (1974) for detail
      CALL HFTI(A,NMAX,N,NPAR,B,NMAX,1,TAU,KRANK,RNORM,H,G,IP)
C
      RETURN
      END
