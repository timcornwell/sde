C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix1drcl.f	1.5    6/10/93
C
      SUBROUTINE PIX1DRCL (DRT, PSF, BOX, NX, PNX,
     $   GAIN, NSUB, BSUB, FLUX, CLN, RES, ANSUB, MAXRES, TFLUX,
     $   CFLUX, CX)
C
CD Clean an image. Pixel level routine. 1-D real only.
C
C	DRT	REAL(*)	input	Name of Dirty image
C	PSF	REAL(*)	input	Name of Point Spread Function
C	BOX	REAL(*)	input	Name of Clean Box Function
C	NX	INT	input	Size of X-axis of DRT
C	PNX	INT	input	Size of X-axis of PSF
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
C	FLUX	REAL	input	Flux cutoff
C	CLN	REAL(*)	input	Name of Clean image
C	RES	REAL(*)	input	Name of Residual image
C	ANSUB	INT	output	Actual number of subtractions
C	MAXRES	REAL	output	Maximum residual left
C	TFLUX	REAL	output	Total flux subtracted
C       CFLUX   REAL    output  Clean components
C       CX      INT     output  Positions of ccs
CM Machine/OS: CONVEX
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Added Clean Boxes
C                               R.G. Marson     Dec 20 1990
C	Added CC list support
C				D.S.Briggs	Oct 21 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, PNX
      REAL		DRT(NX), PSF(PNX), CLN(NX), RES(NX), BOX(NX)
      REAL		GAIN, FLUX, MAXRES, TFLUX, CFLUX(*)
      INTEGER		NSUB, BSUB, ANSUB, CX(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX1DRCL')
C
      INTEGER		ISUB, IX, PMX, PMPX, OFFX
      INTEGER		PIXISAMA
      REAL		PNTSUB, MAXPSF
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Load Dirty image into residual
C
      DO 1 IX = 1, NX
         RES(IX) = DRT(IX) * BOX(IX)
  1   CONTINUE
C
C Find peak of PSF
C
      MAXPSF = 0.0
      DO 5 IX = 4, PNX-3
         IF (ABS(MAXPSF).LT.ABS(PSF(IX))) THEN
            PMPX = IX
            MAXPSF = PSF(IX)
         END IF
   5  CONTINUE
C
      WRITE (MESSAGE, 1000) PMPX, MAXPSF
 1000 FORMAT ('Maximum of PSF is : PSF(',I4,') = ',F7.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C Start iterations
C
      DO 100 ISUB = BSUB, ABS(NSUB)
C
         PMX = PIXISAMA(NX, RES, 1)
         MAXRES = RES(PMX)
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
         PNTSUB = GAIN * MAXRES
         TFLUX = TFLUX + PNTSUB
         CLN(PMX) = CLN(PMX) + PNTSUB
         CFLUX(ISUB) = PNTSUB
         CX(ISUB) = PMX
C
         IF (MOD(ISUB, MAX(1,(ABS(NSUB)-BSUB+1)/10)).EQ.1) THEN
            WRITE (MESSAGE, 1100) ISUB, PMX, MAXRES, TFLUX
 1100       FORMAT ('It ',I5,' Residual(',
     1         I4,') = ',F8.4,'  Flux = ',1PE12.4)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
C Now actually do the CLEANing part
C
         OFFX = PMPX + PMX
         DO 20 IX = 1, NX
            RES(IX) = RES(IX) - (PNTSUB * PSF(OFFX-IX) * BOX(IX))
  20     CONTINUE
C
 100  CONTINUE
C
      ANSUB = NSUB
      PMX = PIXISAMA(NX, RES, 1)
      MAXRES = RES(PMX)
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
