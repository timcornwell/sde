C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixbmshx.f	1.1    9/25/93
C
      SUBROUTINE PIXBMSHX (A, N, NAXIS, CELL, IPEAK, NROW, 
     $   AMIN, BMAJ, BMIN, BPA, BZ, BAMP)
C
CD OUTDATED!!!  Fits a Gaussian beam to section of A centered on IPEAK
C
C	A	REAL	input	array to fit
C	N	INT	input	number of axes
C	NAXIS	INT	input	cells per axis (or dimensions outside SDE)
C	CELL	REAL	input	cell size, degrees
C	IPEAK	INT	input	position of peak of gaussian to fit
C	NROW	INT	input	how far out from peak to fit
C	AMIN	INT	input	minimum (normed)beam amp for sampling ~.5
C	BMAJ	REAL	output	Gaussian major axis, in degrees
C	BMIN	REAL	output	Gaussian minor axis, in degrees
C	BPA	REAL	output	Gaussian position angle
C	BZ	REAL	output	Z axis of 3-D Gaussian
C	BAMP	REAL	output	Value of the array at IPEAK
C
C This is PIXBMSHP 1.3, renamed for archival comparison purposes.  This
C routine contains a bug wherein the major axis of the beam can be
C substantially underestimated for elliptical beams.  (It seems to get
C the minor axis correct, and BMIN <= BMAJ(estimated) <= BMAJ(actual).
C This bug appears to be present in all versions of AIPS until at least
C 15JUL93.  Use this routine only to estimate the error present in
C historical maps/beams.
C
C Audit trail:
C	Retrofitted the VTESS BMSHP routine to SDE
C	Only works for 2-D
C				M.A.Holdaway	Aug 29 1989
C	PIXBMSHP rewritten, and this routine preserved as PIXBMSHX
C				D.S.Briggs	Sept 1 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE='PIXBMSHX')
C
      REAL	A(*), CELL(*), AMIN, BMAJ, BMIN, BPA, BZ, BAMP
      INTEGER	N, NAXIS(*), IPEAK(*), NROW(*)
C
      INTEGER   TRY, I, IFLIP, JFLIP, IJK, JKL,  JLAST, LARRIND
      INTEGER   JROW, J, K, L
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, AMIN2
      REAL	CC1, CC2, CC3, B1, B2, B3, B4, D1, D2, TEMP, RTEMP
C
C
C=======================================================================
C
      IF (ERROR) GO TO 999
C
C	We sample the central part of A, 2*NROW(1)+1 by 2*NROW(2)+1
C
      BZ = 0.
      TRY = 0
      IFLIP = 1
      JFLIP = 1
      XFACT = ABS (CELL(1))
C                                       Zero work arrays.
      DO 25 I = 1,3
         Y(I) = 0.0
         DO 20 J = 1,3
            X(I,J) = 0.0
 20      CONTINUE
 25   CONTINUE
C                                       See if center is 1.0
C					If not, worry about norming
      BAMP = A(NAXIS(1)*(IPEAK(2)-1) + IPEAK(1) ) 
      AMIN2 = AMIN * BAMP
C                                       Loop through rows.
C					Include both above and below
C					in case we are fitting an image feature
      DO 75 IJK = 0, 1
         IFLIP = - IFLIP
         DO 70 I = IJK, NROW(2)
C                                       LARRIND indexes left side of Ith row
            LARRIND = NAXIS(1)*(IPEAK(2) +IFLIP*I-1 ) 
     $         					+(IPEAK(1) - NROW(1))
C                                       Loop down row doing alternate
C                                       halves. go only to first
C                                       decending to AMIN from center.
            DO 65 JKL = 0,1
               JFLIP = - JFLIP
               JLAST = LARRIND + NROW(1)
               DO 60 J = JKL, NROW(1)
                  JROW = LARRIND + NROW(1) + J*JFLIP
                  IF ( (A(JROW) .LT. AMIN2) .AND. 
     $                 (A(JROW) .LT. A(JLAST)) ) GO TO 65
                  IF (A(JROW) .GT. AMIN2 ) THEN 
                     JLAST = JROW
C                                       Compute displacements from
C                                       center.
                     DX = J * JFLIP * CELL(1) / XFACT
                     DY = I * IFLIP * CELL(2) / XFACT
C                                       Compute partials WRT CC1,CC2,CC3
                     P(1) = DX * DX
                     P(2) = DY * DY
                     P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                     DO 55 K = 1,3
                        Y(K) = Y(K) - LOG (A(JROW)/BAMP) * P(K)
                        DO 50 L = 1,3
                           X(K,L) = X(K,L) + P(K) * P(L)
 50                     CONTINUE
 55                  CONTINUE
                  ENDIF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
 75   CONTINUE
C                                       Solve linear equations.
      D1 = X(3,3) * Y(1) - X(1,3) * Y(3)
      D2 = X(3,3) * Y(2) - X(2,3) * Y(3)
      B1 = X(1,1) * X(3,3) - X(3,1) * X(1,3)
      B2 = X(1,2) * X(3,3) - X(3,2) * X(1,3)
      B3 = X(3,3) * X(2,1) - X(2,3) * X(3,1)
      B4 = X(3,3) * X(2,2) - X(2,3) * X(3,2)
C                                       Set values in case of failure.
C                                       default = 1 cell circular.
      CC1 = 0.5
      CC2 = 0.5
      CC3 = 0.0
C                                       Check.
      IF (ABS (B1*B4-B3*B2).EQ.0.0) GO TO 120
      IF ((B2.EQ.0.0) .AND. (B4.EQ.0.0)) GO TO 120
         CC1 = (D1*B4 - D2*B2) / (B1*B4 - B3*B2 )
         IF (B2.NE.0.0) CC2 = (D1 - B1 * CC1) / B2
         IF (B2.EQ.0.0) CC2 = (D2 - B3 * CC1) / B4
 90      IF ((ABS (X(1,3)).LE.ABS (X(2,3))) .OR. (ABS (X(1,3)).LE.
     $      ABS (X(3,3)))) GO TO 100
            CC3 = (Y(1) - X(1,1) * CC1 - X(1,2) * CC2) / X(1,3)
            GO TO 130
 100     IF (ABS (X(2,3)).LE.ABS (X(3,3))) GO TO 110
            CC3 = (Y(2) - X(2,1) * CC1 - X(2,2) * CC2) / X(2,3)
            GO TO 130
 110     IF (X(3,3).EQ.0.0) GO TO 120
            CC3 = (Y(3) - X(3,1) * CC1 - X(3,2) * CC2) / X(3,3)
            GO TO 130
C                                       Solution failed, write message
 120	 CALL MSGPUT('Solution for restoring beam failed','W')
         CALL MSGPUT('Using default beam size of 1 cell','W')
C
C                                       Convert to sigmas and PA.
C                                       Make sure arg of ATAN
C                                       is determinate.
 130  RTEMP = 45.0
      IF (CC1.EQ.CC2) BPA = SIGN (RTEMP, CC3)
      IF (CC1.NE.CC2) BPA = 28.6478 * ATAN (CC3 / (CC1-CC2))
C                                       Compute sigma**2
      BMAJ = 1.0 / (2.0*CC1 + CC3 * TAN (BPA/57.29578))
      BMIN = 1.0 / (2.0*CC2 + CC3 * TAN (BPA/57.29578))
C                                       Check if soln. is real, if not
C                                       retry another.
      TRY = TRY + 1
      IF ((TRY.GT.5) .OR. ((BMAJ.GT.0.0) .AND. (BMIN.GT.0.0)))
     $   GO TO 150
      GO TO (140, 90, 100, 110, 150),  TRY
 140  CONTINUE
         IF (B4.EQ.0.0) GO TO 120
            CC2 = (D2 - B3 * CC1) / B4
            GO TO 90
C                                       Convert to sigmas.
 150  BMAJ = SQRT (BMAJ ) * XFACT
      BMIN = SQRT (BMIN ) * XFACT
C                                       Up to here BMAJ is minor
C                                       axis: rest of pgm wants it as
C                                       major - fix this here
      BPA = -BPA
      IF (BMAJ.GT.BMIN) THEN
         BPA = BPA - 90.0
      ELSE
         TEMP = BMAJ
         BMAJ = BMIN
         BMIN = TEMP
      END IF
C                                       Convert to FWHM
      BMAJ = BMAJ * 2.3548
      BMIN = BMIN * 2.3548
C                                       Add map rotation.
C      BPA = BPA - MAPROT
      IF (BPA.GT.90.0) BPA = BPA - 180.0
      IF (BPA.LT.-90.0) BPA = BPA + 180.0
C
  999 CONTINUE
      END
