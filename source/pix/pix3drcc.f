C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3drcc.f	1.5    7/27/92
C
      SUBROUTINE PIX3DRCC (DRTL, XL, YL, ZL, NL, PSF, PNX, PNY,
     1   PNZ, GAIN, NSUB, BSUB, GSUB, FLUX, SPEED, SLIM, CLNL, 
     2   CXL, CYL, CZL, FL, FN, ANSUB, MAXRES, TFLUX)
C
CD Clark-Clean an image. Pixel level routine. 3-D real only.
C
C
C	DRTL	REAL(*)	input	Name of Dirty image list
C	FL	INT(*)	input	List of field number
C	FN	INT	input	number of this field
C	XL	INT(*)	input	List of X-positions
C	YL	INT(*)	input	List of Y-positions
C	NL	INT	input	Number of elements in list
C	PSF	REAL(*)	input	Name of Point Spread Function
C	PNX	INT	input	Size of X-axis of PSF
C	PNY	INT	input	Size of Y-axis of PSF
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
C	BSUB	INT	input	Start of Number of subtractions
C	FLUX	REAL	input	Flux cutoff
C	SPEED	REAL	input	Speed up factor
C	SLIM	REAL	input	Limit in approximation
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
C	Added field numbers
C				T.J. Cornwell	July 16 1992
C	Added global start of number of iterations to ensure
C	uniformity in cleaning sets of fields
C				T.J. Cornwell	July 27 1992
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NL, XL(*), YL(*), ZL(*), CXL(*), CYL(*), CZL(*)
      INTEGER		FN, FL(*), PNX, PNY, PNZ
      REAL		DRTL(*), PSF(PNX,PNY,*), CLNL(*)
      REAL		GAIN, FLUX, SLIM, MAXRES, TFLUX, SPEED
      INTEGER		NSUB, BSUB, ANSUB, GSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DRCC')
C
      INTEGER		ISUB, PMX, PMY, PMZ, PMPX, PMPY, 
     1			PMPZ, OFFX, OFFY, OFFZ
      INTEGER		PIXISAMA, IPEAK, PX, PY, PZ, I
      REAL		PNTSUB, MAXPSF, SMN
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find peak of PSF
C
      IPEAK = PIXISAMA(PNX*PNY*PNZ, PSF, 1) - 1
      PMPX = 1 + MOD(IPEAK, PNX)
      IPEAK = IPEAK - (PMPX - 1)
      PMPY = 1 + MOD(IPEAK/PNX, PNY)
      IPEAK = IPEAK - (PMPY - 1) * PNY
      PMPZ = 1 + IPEAK/(PNX*PNY)
      MAXPSF = PSF(PMPX, PMPY, PMPZ)
      IF (PMPX.GE.PNX) PMPX = PNX / 2
      IF (PMPY.GE.PNY) PMPY = PNY / 2
      IF (PMPZ.GE.PNZ) PMPZ = PNZ / 2
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
         PMZ = ZL(IPEAK)
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
         CZL(ISUB) = PMZ
         FL(ISUB) = FN
C
C Now actually do the CCLEANing part
C
         OFFX = PMPX + PMX
         OFFY = PMPY + PMY
         OFFZ = PMPZ + PMZ
         DO 20 I = 1, NL
            PX = OFFX - XL(I)
            PY = OFFY - YL(I)
            PZ = OFFZ - ZL(I)
            IF ((PX.GE.1).AND.(PX.LE.PNX).AND.(PY.GE.1).AND.
     1         (PY.LE.PNY).AND.(PZ.GE.1).AND.(PZ.LE.PNZ)) THEN
               DRTL(I) = DRTL(I) - PNTSUB * PSF(PX,PY,PZ)
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
