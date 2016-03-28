C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visimgmp.f	1.2 11/18/94
C
      SUBROUTINE VISIMGMP (VIS, NVIS, WT, U, V, W, IMAGE,
     $   WINDOW, NX, NY, NZ, MATRIX, NCOLS, NROWS,
     1   REFX, DELTX, REFY, DELTY, REFZ, DELTZ)
C
CD Create Matrix for Image->VIS transform
C
C This is an A matrix such that
C
C	MATRIX * IMAGEVEC = VISVEC
C
C	VIS	CMPLX	input	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V, W	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	input	Input image
C	WINDOW	REAL	input	Window image
C	NX, NY	INT	input	Size of image
C	MATRIX	REAL	output	A Matrix
C	NCOLS	REAL	input	Number of columns in A
C	NROWS	REAL	input	Number of rows in A
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Nov 16 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NZ, NVIS, NROWS, NCOLS
      REAL		IMAGE(NX, NY, *), WINDOW(NX, NY, *), WT(*)
      REAL		U(*), V(*), W(*)
      REAL		REFX, REFY, REFZ, DELTX, DELTY, DELTZ
      COMPLEX		VIS(*)
      REAL		MATRIX (NCOLS, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISIMGMP')
C
      INTEGER		IX, IY, IZ, IVIS, ICOL, IROW
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
      DO 2 IROW = 1, NROWS
         DO 1 ICOL = 1, NCOLS
            MATRIX(ICOL, IROW) = 0.0
  1      CONTINUE
  2   CONTINUE
C
      DO 100 IVIS = 1, NVIS
         ICOL = 2 * (IVIS-1) + 1
C
         IF (MOD(IVIS, NVIS/10).EQ.1) THEN
            CALL SYSETIME (CTIME)
            WRITE (MESSAGE, 1000) IVIS, CTIME(1:STRLEN(CTIME))
 1000       FORMAT ('Matrix elements for visibility ',
     $         I7,'   ',A)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
         IF (WT(IVIS).LE.0.0) GO TO 100
         PHASE = CF * (U(IVIS) * DELTX * (1.0 - REFX) +
     1                 V(IVIS) * DELTY * (1.0 - REFY) +
     2                 W(IVIS) * DELTZ * (1.0 - REFZ))
         CFACTZ = WT(IVIS) * CMPLX (COS(PHASE), SIN(PHASE))
         PHASE = CF * U(IVIS) * DELTX 
         CROTX = CMPLX (COS(PHASE), SIN(PHASE))
         PHASE = CF * V(IVIS) * DELTY
         CROTY = CMPLX (COS(PHASE), SIN(PHASE))
         PHASE = CF * W(IVIS) * DELTZ
         CROTZ = CMPLX (COS(PHASE), SIN(PHASE))
         IROW = 0
         DO 30 IZ = 1, NZ
            CFACTYZ = CFACTZ
            DO 20 IY = 1, NY
               CFACTXYZ = CFACTYZ
               DO 10 IX = 1, NX
                  IF(WINDOW(IX,IY,IZ).GT.0.0) THEN
                     IROW = IROW + 1
                     MATRIX(ICOL, IROW)   = REAL (CFACTXYZ)
                     MATRIX(ICOL+1, IROW) = AIMAG(CFACTXYZ)
                  END IF
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
