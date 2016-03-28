C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drcl.f	1.6    2/27/95
C
      SUBROUTINE PIX2DRCL (DRT, PSF, BOX, NX, NY, PNX, PNY,
     1   GAIN, NSUB, BSUB, FLUX, CLN, RES, ANSUB, MAXRES, TFLUX,
     $   CFLUX, CX, CY)
C
CD Clean an image. Pixel level routine. 2-D real only.
C
C   GAIN, NSUB, BSUB, FLUX, CLN, RES, ANSUB, MAXRES, TFLUX, CFLUX,
C   CX, CY)
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
C	CLN	REAL(*)	input	Name of Clean image
C	RES	REAL(*)	input	Name of Residual image
C	ANSUB	INT	output	Actual number of subtractions
C	MAXRES	REAL	output	Maximum residual left
C	TFLUX	REAL	output	Total flux subtracted
C      CFLUX   REAL    output  Clean components
C      CX, CY  INT     output  Positions of x, y 
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added clean component listing
C				T.J.Cornwell	June 7 1989
C	Added clean Boxes
C				R.G. Marson     Dec 20 1990
C	Changed format of print statement slightly
C				D.S.Briggs	Feb 27 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, PNX, PNY, CX(*), CY(*)
      REAL		DRT(NX,*), PSF(PNX,*), CLN(NX,*), RES(NX,*)
      REAL		GAIN, FLUX, MAXRES, TFLUX, CFLUX(*), BOX(NX, *)
      INTEGER		NSUB, BSUB, ANSUB
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRCL')
C
      INTEGER		ISUB, IX, IY, PMX, PMY, PMPX, PMPY, OFFX, OFFY
      INTEGER		PIXISAMA, IPEAK
      REAL		PNTSUB, MAXPSF
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Load Dirty image into residual
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            RES(IX,IY) = DRT(IX,IY) * BOX(IX, IY)
  1      CONTINUE
  2   CONTINUE
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
C
      DO 100 ISUB = BSUB, ABS(NSUB)
C
         IPEAK = PIXISAMA(NX*NY, RES, 1) - 1
         PMX = 1 + MOD(IPEAK, NX)
         PMY = 1 + (IPEAK - PMX + 1)/NX
         MAXRES = RES(PMX, PMY)
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
         CLN(PMX, PMY) = CLN(PMX, PMY) + PNTSUB
         CFLUX(ISUB) = PNTSUB
         CX(ISUB) = PMX
         CY(ISUB) = PMY
C
         IF (MOD(ISUB, MAX(1,(ABS(NSUB)-BSUB+1)/10)).EQ.1) THEN
            IF (MAXRES.GT.0.0010) THEN
               WRITE (MESSAGE, 1100) ISUB, PMX, PMY, MAXRES, TFLUX
 1100          FORMAT ('It ',I7,' Residual(',I4,',',
     1            I4,') = ',F8.4,', Flux = ',1PE12.4)
            ELSE
               WRITE (MESSAGE, 1101) ISUB, PMX, PMY, MAXRES, TFLUX
 1101          FORMAT ('It ',I7,' Residual(',I4,',',
     1            I4,') = ',1PE12.4,', Flux = ',1PE12.4)
            END IF
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
