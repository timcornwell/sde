C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drcl.f	1.5    5/22/92
C
      SUBROUTINE PIX2RMCL (DRT, PSF, PSFBOX, BOX, NX, NY, PNX, PNY,
     $   NPSF, PSFRX, PSFRY, POFFX, POFFY, GAIN, NSUB, BSUB, FLUX,
     $   CLN, RES, ANSUB,
     $   MAXRES, TFLUX, CFLUX, CX, CY)
C
CD Clean an image. (Multi PSF) Pixel level routine. 2-D real only.
C
C	DRT	REAL(*)	input	Dirty image
C	PSF	REAL(*)	input	Point Spread Function (Stack)
C	PSFBOX	REAL(*)	input	Point Spread Function Boxes (Stack)
C	BOX	REAL(*)	input	Name of Clean Box Function
C	NX	INT	input	Size of X-axis of DRT
C	NY	INT	input	Size of Y-axis of DRT
C	PNX	INT	input	Size of X-axis of PSF
C	PNY	INT	input	Size of Y-axis of PSF
C	NPSF	INT	input	Number of PSFs
C	PSFRX	INT	input	PSF reference pixel
C	PSFRY	INT	input	PSF reference pixel
C	POFFX	INT(NPSF) input	Working space
C	POFFY	INT(NPSF) input	Working space
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
C	BSUB	INT	input	Start of Number of subtractions
C	FLUX	REAL	input	Flux cutoff
C	CLN	REAL(*)	input	Name of Clean image
C	RES	REAL(*)	input	Name of Residual image
C	ANSUB	INT	output	Actual number of subtractions
C	MAXRES	REAL	output	Maximum residual left
C	TFLUX	REAL	output	Total flux subtracted
C       CFLUX   REAL    output  Clean components
C       CX, CY  INT     output  Positions of x, y 
C Audit trail:
C	Cloned from pixr2dcln.f
C				D.S.Briggs	Nov 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, PNX, PNY, NPSF, CX(*), CY(*),
     $   		PSFRX, PSFRY, POFFX(*), POFFY(*)
      REAL		DRT(NX,*), PSF(PNX,PNY,*), PSFBOX(NX,NY,*),
     $   		CLN(NX,*), RES(NX,*)
      REAL		GAIN, FLUX, MAXRES, TFLUX, CFLUX(*), BOX(NX, *)
      INTEGER		NSUB, BSUB, ANSUB, I
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2RMCL')
C
      INTEGER		ISUB, IX, IY, PMX, PMY, OFFX, OFFY
      INTEGER		PIXISAMA, IPEAK, KPSF
      REAL		PNTSUB
C=====================================================================
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
C Find peaks of PSFs
C
      DO 50 I = 1, NPSF
         IPEAK = PIXISAMA(PNX*PNY, PSF(1,1,I), 1) - 1
         POFFX(I) = 1 + MOD(IPEAK, PNX) - PSFRX
         POFFY(I) = 1 + (IPEAK - POFFX(I) + 1)/PNX - PSFRY
 50   CONTINUE
C
C Start iterations
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
C Find the appropriate PSF to use
C
         KPSF = NPSF
         DO 10 I = 1, NPSF-1
            IF (PSFBOX(PMX,PMY,I).GT.0.0) THEN
               KPSF = I
               GO TO 11
            END IF
 10      CONTINUE
 11      CONTINUE
C
         PNTSUB = GAIN * MAXRES
         TFLUX = TFLUX + PNTSUB
         CLN(PMX-POFFX(KPSF), PMY-POFFY(KPSF)) =
     $      CLN(PMX-POFFX(KPSF), PMY-POFFY(KPSF)) + PNTSUB
         CFLUX(ISUB) = PNTSUB
         CX(ISUB) = PMX - POFFX(KPSF)
         CY(ISUB) = PMY - POFFY(KPSF)
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
         OFFX = PMX - PSFRX - POFFX(KPSF)
         OFFY = PMY - PSFRY - POFFY(KPSF)
         DO 21 IY = 1, NY
            DO 20 IX = 1, NX
               RES(IX, IY) = RES(IX, IY) - 
     1            (PNTSUB * PSF(IX-OFFX,IY-OFFY,KPSF) * BOX(IX, IY))
  20        CONTINUE
  21     CONTINUE
C
C End of main loop
C
 100  CONTINUE
C
C Cleanup
C
      ANSUB = NSUB
      IPEAK = PIXISAMA(NX*NY, RES, 1) - 1
      PMX = 1 + MOD(IPEAK, NX)
      PMY = 1 + (IPEAK - PMX + 1)/NX
      MAXRES = RES(PMX, PMY)
      GO TO 120
C
C Jump to here on a QUIT or termination by flux criterion
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
