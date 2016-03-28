C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdft3d.f	1.3    5/31/94
C
      SUBROUTINE VISDFT3D (VIS, NVIS, WT, U, V, W, IMAGE, PSF, NX, NY, 
     1   NZ, REFX, DELTX, REFY, DELTY, REFZ, DELTZ)
C
CD Pixel-level DFT routine VIS->IMG in 3D
C
C
C	VIS	CMPLX	input	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V, W	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	output	Output image
C	PSF	REAL	output	Output PSF
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added progress reports, since this can take an awful
C	long time
C				D.S.Briggs	Feb 11 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NZ, NVIS
      REAL		IMAGE(NX, NY, *), PSF(NX, NY, *), WT(*)
      REAL		U(*), V(*), W(*)
      REAL		REFX, REFY, REFZ, DELTX, DELTY, DELTZ
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFT3D')
C
      INTEGER		IX, IY, IZ, IVIS
      COMPLEX		CROTX, CROTY, CROTZ, CFACTZ, CFACTYZ, CFACTXYZ
      REAL		PHASE, RNORM, CF
      CHARACTER*(SYSMXNAM)	CTIME
C
      INTEGER		STRLEN
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CF = 2. * (4 * ATAN(1.0))**2 / 180.0
C
      DO 3 IZ = 1, NZ
         DO 2 IY = 1, NY
            DO 1 IX = 1, NX
               PSF(IX, IY, IZ) = 0.0
               IMAGE(IX, IY, IZ) = 0.0
  1         CONTINUE
  2      CONTINUE
  3   CONTINUE
C
      RNORM = 0.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
            RNORM = RNORM + WT(IVIS)
  50  CONTINUE
      IF (RNORM.GT.0.0) THEN
         RNORM = 1.0 / RNORM
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
      DO 100 IVIS = 1, NVIS
C
         IF (MOD(IVIS, NVIS/10).EQ.1) THEN
            CALL SYSETIME (CTIME)
            WRITE (MESSAGE, 1000) IVIS, CTIME(1:STRLEN(CTIME))
 1000       FORMAT ('DFT on iteration',I7,'       ',A)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
         IF (WT(IVIS).LE.0.0) GO TO 100
         PHASE = CF * (U(IVIS) * DELTX * (1.0 - REFX) +
     1           V(IVIS) * DELTY * (1.0 - REFY) +
     2           W(IVIS) * DELTZ * (1.0 - REFZ))
         CFACTZ = RNORM * WT(IVIS) * CMPLX (COS(PHASE), - SIN(PHASE))
         PHASE = CF * U(IVIS) * DELTX 
         CROTX = CMPLX (COS(PHASE), - SIN(PHASE))
         PHASE = CF * V(IVIS) * DELTY
         CROTY = CMPLX (COS(PHASE), - SIN(PHASE))
         PHASE = CF * W(IVIS) * DELTZ
         CROTZ = CMPLX (COS(PHASE), - SIN(PHASE))
         DO 30 IZ = 1, NZ
            CFACTYZ = CFACTZ
            DO 20 IY = 1, NY
               CFACTXYZ = CFACTYZ
               DO 10 IX = 1, NX
                  IMAGE(IX, IY, IZ) = IMAGE(IX, IY, IZ) + 
     1               REAL(VIS(IVIS) * CFACTXYZ)
                  PSF(IX, IY, IZ) = PSF(IX, IY, IZ) + REAL(CFACTXYZ)
                  CFACTXYZ = CFACTXYZ * CROTX
  10           CONTINUE
               CFACTYZ = CFACTYZ * CROTY
  20        CONTINUE
            CFACTZ = CFACTZ * CROTZ
  30     CONTINUE
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
