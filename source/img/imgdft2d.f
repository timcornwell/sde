C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdft2d.f	1.6    7/15/93
C
      SUBROUTINE IMGDFT2D (IMAGE, NX, NY, REFX, DELTX, REFY,
     1   DELTY, VIS, NVIS, WT, U, V)
C
CD Pixel-level DFT routine IMG->VIS in 2D
C
C
C	IMAGE	REAL	input	Input image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	VIS	CMPLX	output	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Fixed factor of 2 error in phase!!!!
C				T.J.Cornwell	Oct 25 1990
C	Fixed additional phase error!  (Equivalent to a shift
C	in Y)
C				D.S.Briggs	15 July 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFT2D')
C
      INTEGER		IX, IY, IVIS
      COMPLEX		CROTX, CROTY, CFACTY, CFACTXY
      REAL		PHASE, CF
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CF = (8 * ATAN(1.0))**2 / 360.0
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         VIS(IVIS) = 0.0
         PHASE = CF * ((U(IVIS) * DELTX * (1.0 - REFX) +
     1           V(IVIS) * DELTY * (1.0 - REFY)))
         CFACTY = CMPLX (COS(PHASE), SIN(PHASE))
         PHASE = CF * U(IVIS) * DELTX 
         CROTX = CMPLX (COS(PHASE), SIN(PHASE))
         PHASE = CF * V(IVIS) * DELTY
         CROTY = CMPLX (COS(PHASE), SIN(PHASE))
         DO 20 IY = 1, NY
            CFACTXY = CFACTY
            DO 10 IX = 1, NX
               VIS(IVIS) = VIS(IVIS) + IMAGE(IX, IY) * CFACTXY
               CFACTXY = CFACTXY * CROTX
  10        CONTINUE
            CFACTY = CFACTY * CROTY
  20     CONTINUE
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
