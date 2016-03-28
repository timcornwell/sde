C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissdp.f	1.2    11/7/90
C
      SUBROUTINE VISSDP (VIS, NVIS, WT, U, V, IMAGE, NX, NY, 
     1   CX, CY)
C
CD Pixel-level DFT routine VIS->IMG in 2D, makes single dish image
C
C
C	VIS	CMPLX	input	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	output	Output image
C	NX, NY	INT	input	Size of image
C	CX	INT	input	Center of pb
C Audit trail:
C	Fixed normalization to apply to zero spacing points only
C				T.J. Cornwell	May 9 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS
      REAL		IMAGE(NX, *), WT(*), U(*), V(*), CX, CY
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSDP')
C
      INTEGER		ICX, ICY, IX, IY, IVIS
      REAL		RNORM
C
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IMAGE(IX, IY) = 0.0
  1      CONTINUE
  2   CONTINUE
C
      RNORM = 0.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
         IF((U(IVIS).EQ.0.0).AND.(V(IVIS).EQ.0.0)) THEN
            RNORM = RNORM + WT(IVIS)
         END IF
  50  CONTINUE
      IF (RNORM.GT.0.0) THEN
         RNORM = 1.0 / RNORM
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX, CY
 1000    FORMAT ('VISSDP - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
C Poor convolution function
C
      ICX = NINT(CX)
      ICY = NINT(CY)
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF((U(IVIS).EQ.0.0).AND.(V(IVIS).EQ.0.0)) THEN
            IMAGE(ICX, ICY) = IMAGE(ICX, ICY) + WT(IVIS) * RNORM *
     $         REAL(VIS(IVIS))
         END IF
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
