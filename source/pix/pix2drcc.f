C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drcc.f	1.6    7/27/92
C
      SUBROUTINE PIX2DRCC (DRTL, XL, YL, NL, PSF, PNX, PNY,
     1   GAIN, NSUB, BSUB, GSUB, FLUX, SPEED, SLIM, CLNL, CXL, CYL, 
     2   FL, FN, ANSUB, MAXRES, TFLUX)
C
CD Clark-Clean an image. Pixel level routine. 2-D real only.
C
C	DRTL	REAL(*)	input	Name of Dirty image list
C	XL	INT(*)	input	List of X-positions
C	YL	INT(*)	input	List of Y-positions
C	NL	INT	input	Number of elements in list
C	PSF	REAL(*)	input	Name of Point Spread Function
C	PNX	INT	input	Size of X-axis of PSF
C	PNY	INT	input	Size of Y-axis of PSF
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
C	BSUB	INT	input	Start of Number of subtractions
C	GSUB	INT	input	Global Start of Number of subtractions
C	FLUX	REAL	input	Flux cutoff
C	SPEED	REAL	input	Speed up factor
C	SLIM	REAL	input	Limit of approximation
C	CLNL	REAL(*)	input	Name of Clean image list
C	CXL	INT(*)	output	List of CC x-positions
C	CYL	INT(*)	output	List of CC y-positions
C	ANSUB	INT	output	Actual number of subtractions
C	MAXRES	REAL	output	Maximum residual left
C	TFLUX	REAL	output	Total flux subtracted
C Audit trail:
C	Distinguish between FLUX and SLIM
C				T.J.Cornwell	Feb 13 1989
C	Stop at first neg if FLUX < 0.0
C				T.J. Cornwell	Nov 1 1989
C	Added speed factor
C				T.J. Cornwell	Feb 28 1990
C	Change SMN to initialize at 0.0
C				T.J. Cornwell	May 17 1991
C	Added field numbers
C				T.J. Cornwell	July 16 1992
C	Added global start of number of iterations to ensure
C	uniformity in cleaning sets of fields
C				T.J. Cornwell	July 27 1992
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NL, FL(*), FN, XL(*), YL(*), CXL(*), CYL(*),
     $			PNX, PNY
      REAL		DRTL(*), PSF(PNX,*), CLNL(*)
      REAL		GAIN, FLUX, SLIM, MAXRES, TFLUX, SPEED
      INTEGER		NSUB, BSUB, ANSUB, GSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRCC')
C
      INTEGER		ISUB, PMX, PMY, PMPX, PMPY, OFFX, OFFY
      INTEGER		PIXISAMA, IPEAK, PX, PY, I
      REAL		PNTSUB, MAXPSF, SMN
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find peak of PSF
C
      IPEAK = PIXISAMA(PNX*PNY, PSF, 1) - 1
      PMPX = 1 + MOD(IPEAK, PNX)
      PMPY = 1 + (IPEAK - PMPX + 1)/PNX
      MAXPSF = PSF(PMPX, PMPY)
      IF(ABS(MAXPSF-1.0).GT.1E-3) THEN
         WRITE (MESSAGE, 1000) MAXPSF
 1000    FORMAT ('Peak of PSF is not unity: ',F10.4)
         CALL MSGPUT (MESSAGE, 'W')
      END IF
      IF ((PMPX.GT.PNX).OR.(PMPY.GT.PNY)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'cannot find peak of PSF')
         GO TO 999
      END IF
C
C Start iterations
C
      SMN = 0.0
      GSUB = MAX(1, GSUB)
      IF(GSUB.LT.BSUB) THEN
         DO 50 ISUB = GSUB, BSUB
            SMN = SMN + 1.0/FLOAT(ISUB)**SPEED
 50      CONTINUE
      END IF
C
      DO 100 ISUB = BSUB, ABS(NSUB)
C
         SMN = SMN + 1.0/FLOAT(ISUB)**SPEED
C
         IPEAK = PIXISAMA(NL, DRTL, 1)
         PMX = XL(IPEAK)
         PMY = YL(IPEAK)
         MAXRES = DRTL(IPEAK)
C
         IF ((FLUX.LT.0.0).AND.(MAXRES.LT.0.0)) GO TO 110
         IF ((NSUB.LT.0).AND.(MAXRES.LT.0.0)) GO TO 110
         IF (ABS(MAXRES).LE.MAX(SLIM*SMN,ABS(FLUX))) GO TO 110
         IF (SYSINTRP) THEN
            IF (SYSINTAC.EQ.'QUIT') THEN
               CALL MSGPUT ('Stopping CCLEAN now', 'I')
               GO TO 110
            END IF
         END IF
C
         PNTSUB = GAIN * MAXRES
         TFLUX = TFLUX + PNTSUB
         CLNL(ISUB) = PNTSUB
         CXL(ISUB) = PMX
         CYL(ISUB) = PMY
         FL(ISUB) = FN
C
C Now actually do the CCLEANing part
C
         OFFX = PMPX + PMX
         OFFY = PMPY + PMY
         DO 20 I = 1, NL
            PX = OFFX - XL(I)
            PY = OFFY - YL(I)
            IF ((PX.GE.1).AND.(PX.LE.PNX).AND.(PY.GE.1).AND.
     1         (PY.LE.PNY)) THEN
               DRTL(I) = DRTL(I) - PNTSUB * PSF(PX,PY)
             END IF
  20     CONTINUE
C
 100  CONTINUE
C
      ANSUB = NSUB
      IPEAK = PIXISAMA(NL, DRTL, 1)
      MAXRES = DRTL(IPEAK)
      GO TO 120
C
 110  CONTINUE
      ANSUB = ISUB - 1
C
 120  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
