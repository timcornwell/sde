C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drn1.f	1.1    6/7/93
C
      SUBROUTINE PIX2DRN1 (DRT, PSF, BOX, NX, NY, PNX, PNY,
     1   GAIN, NSUB, BSUB, FLUX, CLN, RES, WRK, ANSUB, MAXRES, TFLUX,
     $   CFLUX, CX, CY)
C
CD Clean an image. Pixel level routine. 2-D real only.  Non-Negative CCs
CD  (old style backtracking)
C
C	DRT	REAL(*)	input	Name of Dirty image
C	PSF	REAL(*)	input	Name of Point Spread Function
C	BOX	REAL(*)	input	Name of Clean Box Function
C	NX	INT	input	Size of X-axis of DRT
C	NY	INT	input	Size of Y-axis of DRT
C	PNX	INT	input	Size of X-axis of PSF
C	PNY	INT	input	Size of Y-axis of PSF
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
C	BSUB	INT	input	Start of Number of subtractions
C	FLUX	REAL	input	Flux cutoff
C	CLN	REAL(*)	input	Clean image
C	RES	REAL(*)	input	Residual image
C	WRK	REAL(*)	input	Working space same size as RES
C	ANSUB	INT	output	Actual number of subtractions
C	MAXRES	REAL	output	Maximum residual left
C	TFLUX	REAL	output	Total flux subtracted
C       CFLUX   REAL    output  Clean components
C       CX, CY  INT     output  Positions of x, y 
C
C This could almost certainly be made faster with a list based back
C rather than the array based backtrack as here.  But then, the whole
C idea is beastly slow and inefficient to start with.
C
C Audit trail:
C	Cloned from PIX2DRCL
C				D.S.Briggs	Mar 2 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, PNX, PNY, CX(*), CY(*)
      REAL		DRT(NX,*), PSF(PNX,*), CLN(NX,*), RES(NX,*)
      REAL		BOX(NX,*), WRK(NX,*)
      REAL		GAIN, FLUX, MAXRES, TFLUX, CFLUX(*)
      INTEGER		NSUB, BSUB, ANSUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRN1')
C
      INTEGER		ISUB, IX, IY, PMX, PMY, PMPX, PMPY, OFFX, OFFY
      INTEGER		PIXISAMA, IPEAK
      LOGICAL		WRKNZ
      REAL		PNTSUB, MAXPSF
C=====================================================================
      IF (ERROR) GO TO 999
C
C Load Dirty image into residual, zero out WRK
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            RES(IX,IY) = DRT(IX,IY) * BOX(IX, IY)
            WRK(IX,IY) = 0.0
  1      CONTINUE
  2   CONTINUE
      WRKNZ = .FALSE.
C
C Find peak of PSF
C
      IPEAK = PIXISAMA(PNX*PNY, PSF, 1) - 1
      PMPX = 1 + MOD(IPEAK, PNX)
      PMPY = 1 + (IPEAK - PMPX + 1)/PNX
      MAXPSF = PSF(PMPX, PMPY)
      IF (PMPX.GE.PNX) PMPX = PNX / 2
      IF (PMPY.GE.PNY) PMPY = PNY / 2
C
C Start iterations
C
      DO 100 ISUB = BSUB, ABS(NSUB)
C
C Repeat until we get a valid component
C
 10      CONTINUE
         IPEAK = PIXISAMA(NX*NY, RES, 1) - 1
         PMX = 1 + MOD(IPEAK, NX)
         PMY = 1 + (IPEAK - PMX + 1)/NX
         MAXRES = RES(PMX, PMY)
         PNTSUB = GAIN * MAXRES
C
C Check if resulting total component would be negative
C
         IF (PNTSUB+CLN(PMX,PMY).LT.0.0) THEN
C
C If the component is already <= 0, reject it and try another
C
            IF (CLN(PMX,PMY).LE.0.0) THEN
               WRK(PMX,PMY) = RES(PMX,PMY)
               RES(PMX,PMY) = 0.0
               WRKNZ = .TRUE.
               GO TO 10
            ELSE
C
C If the existing comp is positive, just set it back to zero.
C An alternative strategy would be to accept a fraction of the comp
C
               PNTSUB = -CLN(PMX,PMY)
            END IF
         END IF
C
         IF (WRKNZ) THEN
            DO 15 IY = 1, NY
               DO 14 IX = 1, NX
                  RES(IX,IY) = RES(IX,IY) + WRK(IX,IY)
                  WRK(IX,IY) = 0.0
 14            CONTINUE
 15         CONTINUE
            WRKNZ = .FALSE.
         END IF
C
         IF ((NSUB.LT.0).AND.(MAXRES.LT.0.0)) GO TO 110
         IF (ABS(MAXRES).LE.FLUX) GO TO 110
         IF (SYSINTRP) THEN
            IF (SYSINTAC.EQ.'QUIT') THEN
               CALL MSGPUT ('Stopping CLEAN now', 'I')
               GO TO 110
            END IF
         END IF
C
         TFLUX = TFLUX + PNTSUB
         CLN(PMX, PMY) = CLN(PMX, PMY) + PNTSUB
         CFLUX(ISUB) = PNTSUB
         CX(ISUB) = PMX
         CY(ISUB) = PMY
C
         IF (MOD(ISUB, MAX(1,(ABS(NSUB)-BSUB+1)/10)).EQ.1) THEN
            WRITE (MESSAGE, 1100) ISUB, PMX, PMY, MAXRES, TFLUX
 1100       FORMAT ('It ',I5,' Residual(',I4,',',
     1         I4,') = ',F8.4,', Flux = ',1PE12.4)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
C Now actually do the CLEANing part
C
         OFFX = PMPX + PMX
         OFFY = PMPY + PMY
         DO 21 IY = 1, NY
            DO 20 IX = 1, NX
               RES(IX, IY) = RES(IX, IY) - 
     1            (PNTSUB * PSF(OFFX-IX,OFFY-IY) * BOX(IX, IY))
  20        CONTINUE
  21     CONTINUE
C
 100  CONTINUE
C
      ANSUB = NSUB
      IPEAK = PIXISAMA(NX*NY, RES, 1) - 1
      PMX = 1 + MOD(IPEAK, NX)
      PMY = 1 + (IPEAK - PMX + 1)/NX
      MAXRES = RES(PMX, PMY)
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
