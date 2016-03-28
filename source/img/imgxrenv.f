C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgxrenv.f	1.1	 8/10/92
C
      SUBROUTINE IMGXRENV (XFR, THRESH, X, Y, N, NBINS)
C
CD Calculate envelope of transfer function
C	XFR	CHAR	input	Input transfer function
C	THRESH	REAL	input	Threshold for significance in amplitude
C	X, Y	REAL	output	X & Y coordinates of enclosing polygon
C	N	INT	input	Number of points desired
C	NBINS	INT	input	Number of points used in binning (<=N)
C
C Notes:
C
C Input array is real.  Use ARRABS if necessary.  Output is in pixels,
C which is suitable for a subsequent call to PIXDRFPL, but be careful
C of other uses in cases where DU != DV.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	June 20 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGXRENV')
C
      CHARACTER*(*)	XFR
      INTEGER		N, NBINS
      REAL		THRESH, X(N), Y(N)
C
      INTEGER		XADD, NAX, NAXIS(SYSMXDIM), SCRADD
      CHARACTER		ATYPE*1
      CHARACTER*8	TYPE(8)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
C
      INTEGER		CRDRNAX
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (XFR, NAX, NAXIS, ATYPE, XADD)
      IF (ERROR) GO TO 990
C
      CALL CRDGET (XFR, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      IF (CRDRNAX(NAX, NAXIS).NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array must be 2-D')
         GO TO 999
      END IF
C
      CALL DATMAKAR ('EnvelopeScratch', 1, 2*NBINS, 'R', SCRADD)
      CALL IMGXRENP (MEMR(XADD), NAXIS(1), NAXIS(2), RPIX(1), RPIX(2),
     $   THRESH, X, Y, N, MEMR(SCRADD), MEMR(SCRADD+NBINS), NBINS)
      CALL DATDELET ('EnvelopeScratch')
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgxrenv.f	1.1	 8/10/92
C
C Audit trail:
      SUBROUTINE IMGXRENP (A, NX, NY, XC, YC, ATHR, X, Y, NP,
     $   BR2, BTHETA, NB)
C
CD Calculate envelope of half plane transfer function, pixel level
C
C	A	REAL	input	Input transfer function
C	NX, NY	INT	input	Size of A
C	XC, YC	REAL	input	Center of A in pixels
C	ATHR	REAL	input	Min ampl for significance
C	X, Y	REAL	output	Bounding polygon
C	NP	INT	input	Size of X, Y
C	BR2	REAL	scratch	Binned radius^2
C	BTHETA	REAL	scratch	Binned theta
C	NB	INT	input	Number of bins
C
C Audit trail:
C	Original version:
C				D.S.Briggs	June 20 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGXRENP')
C
      INTEGER		NX, NY, NP, NB
      REAL		A(NX,NY), XC, YC, ATHR, X(NP), Y(NP),
     $   		BR2(NB), BTHETA(NB)
C
      REAL		R, R2, THETA, PI, M, B, UNDEF
      INTEGER		IPT, I, J, NR
      LOGICAL		ISHALF
C
      DATA		UNDEF/-1.E10/
C==================================================================
      IF (ERROR) GO TO 999
C
      PI = 4.0 * ATAN(1.0)
      DO 10 I = 1, NB
         BR2(I) = 0.0
 10   CONTINUE
C
      ISHALF = XC.LT.1.5
      IF (ISHALF) THEN
         M = NB / PI
      ELSE
         M = NB / (2.0*PI)
      END IF
      B = NB / 2.0
C
      DO 101 I = 1, NX
         DO 100 J = 1, NY
            IF (ABS(A(I,J)).GE.ATHR) THEN
               IF ((REAL(I).NE.XC).AND.(REAL(J).NE.YC)) THEN
                  THETA = ATAN2(J-YC,I-XC)
                  IPT = MAX(1, MIN(INT(M*THETA+B)+1, NB))
                  R2 = (I-XC)*(I-XC) + (J-YC)*(J-YC)
                  IF (R2.GT.BR2(IPT)) THEN
                     BR2(IPT) = R2
                     BTHETA(IPT) = THETA
                  END IF
               END IF
            END IF
 100     CONTINUE
 101  CONTINUE
C
C Now interpolate in theta to a more reasonable number of points
C
      DO 200 I = 1, NP
         X(I) = UNDEF
         Y(I) = UNDEF
 200  CONTINUE
C
      IF (ISHALF) THEN
         NR = NP - 2
         J = 2
         M = NR / PI
         B = NR / 2.0 + 1
         X(1) = -PI/2.0 - 0.001
         X(NP) = PI/2.0 + 0.001
      ELSE
         NR = NP
         J = 1
         M = NR / (2.0*PI)
         B = NR / 2.0
      END IF
C
      DO 210 I = 1, NB
         IF (BR2(I).GT.0.0) THEN
            IPT = MAX(J, MIN(INT(M*BTHETA(I)+B)+1, NR))
            X(IPT) = BTHETA(I)
            Y(IPT) = SQRT(BR2(I))
         END IF
 210  CONTINUE
C
      CALL UTLRINTP (X, Y, NP, UNDEF)
C
C Final conversion to Cartesian coords
C
      DO 220 I = 1, NP
         THETA = X(I)
         R = Y(I) + .25
         X(I) = R * COS(THETA) + XC
         Y(I) = R * SIN(THETA) + YC
 220  CONTINUE
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
