C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3drcl.f	1.4    5/22/92
C
      SUBROUTINE PIX3DRCL (DRT, PSF, BOX, NX, NY, NZ, PNX, PNY, PNZ,
     1   GAIN, NSUB, BSUB, FLUX, CLN, RES, ANSUB, MAXRES, TFLUX,
     $   CFLUX, CX, CY, CZ)
C
CD Clean an image. Pixel level routine. 3-D real only.
C
C	DRT	REAL(*)	input	Name of Dirty image
C	PSF	REAL(*)	input	Name of Point Spread Function
C	PSF	REAL(*)	input	Name of Clean Box Function
C	NX	INT	input	Size of X-axis of DRT
C	NY	INT	input	Size of Y-axis of DRT
C	NZ	INT	input	Size of Z-axis of DRT
C	PNX	INT	input	Size of X-axis of PSF
C	PNY	INT	input	Size of Y-axis of PSF
C	PNZ	INT	input	Size of Z-axis of PSF
C	GAIN	REAL	input	Loop gain 
C	NSUB	INT	input	Number of subtractions
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
C	Added Clean Boxes
C				R.G. Marson     Dec 20 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, PNX, PNY, PNZ, CX(*), CY(*), CZ(*)
      REAL		DRT(NX,NY,*), PSF(PNX,PNY,*), CLN(NX,NY,*), 
     1			RES(NX,NY,*), CFLUX(*), BOX(NX, NY, *)
      REAL		GAIN, FLUX, MAXRES, TFLUX
      INTEGER		NSUB, BSUB, ANSUB
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX3DRCL')
C
      INTEGER		ISUB, IX, IY, IZ, PMX, PMY, PMZ, PMPX, PMPY, 
     1			PMPZ, OFFX, OFFY, OFFZ
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
      DO 3 IZ = 1, NZ
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
               RES(IX,IY, IZ) = DRT(IX,IY, IZ) * BOX(IX, IY, IZ)
  1         CONTINUE
  2      CONTINUE
  3   CONTINUE
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
C
C Start iterations
C
      DO 100 ISUB = BSUB, ABS(NSUB)
C
         IPEAK = PIXISAMA(NX*NY*NZ, RES, 1) - 1
         PMX = 1 + MOD(IPEAK, NX)
         IPEAK = IPEAK - (PMX - 1)
         PMY = 1 + MOD(IPEAK/NX, NY)
         IPEAK = IPEAK - (PMY - 1) * NY
         PMZ = 1 + IPEAK/(NX*NY)
         MAXRES = RES(PMX, PMY, PMZ)
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
         CFLUX(ISUB) = PNTSUB
         CX(ISUB) = PMX
         CY(ISUB) = PMY
         CZ(ISUB) = PMZ
         CLN(PMX, PMY, PMZ) = CLN(PMX, PMY, PMZ) + PNTSUB
C
         IF (MOD(ISUB, MAX(1,(ABS(NSUB)-BSUB+1)/10)).EQ.1) THEN
            WRITE (MESSAGE, 1100) ISUB, PMX, PMY, PMZ, MAXRES, TFLUX
 1100       FORMAT ('It ',I5,' Residual(',I4,',',
     1         I4,',',I4,') = ',F8.4,', Flux = ',1PE12.4)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
C Now actually do the CLEANing part
C
         OFFX = PMPX + PMX
         OFFY = PMPY + PMY
         OFFZ = PMPZ + PMZ
         DO 22 IZ = 1, NZ
            DO 21 IY = 1, NY
               DO 20 IX = 1, NX
                  RES(IX, IY, IZ) = RES(IX, IY, IZ) - 
     1               (PNTSUB * PSF(OFFX-IX,OFFY-IY,OFFZ-IZ) * 
     $                                                 BOX(IX, IY, IZ))
  20           CONTINUE
  21        CONTINUE
  22     CONTINUE
C
 100  CONTINUE
C
      ANSUB = NSUB
      IPEAK = PIXISAMA(NX*NY*NZ, RES, 1) - 1
      PMX = 1 + MOD(IPEAK, NX)
      IPEAK = IPEAK - (PMX - 1)
      PMY = 1 + MOD(IPEAK/NX, NY)
      IPEAK = IPEAK - (PMY - 1) * NY
      PMZ = 1 + IPEAK/(NX*NY)
      MAXRES = RES(PMX, PMY, PMZ)
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
