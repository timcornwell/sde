C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img3ft2.f	1.1 9/18/92
C
      SUBROUTINE IMG3FT2 (VIS, NVIS, WT, U, V, W, IMAGE, NX, 
     1   NY, REFX, DELTX, REFY, DELTY, SHIFT)
C
CD Pixel-level DFT routine IMG->VIS in 3D
C
C	VIS	CMPLX	output	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V, W	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	input	Input image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C Audit trail:
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS
      REAL		IMAGE(NX, *), WT(*), U(*), V(*), W(*) 
      REAL		REFX, REFY, DELTX, DELTY
      DOUBLE PRECISION	SHIFT(3,*)
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG3FT2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ, RAD, RSCL, TAPER, PSCL
      REAL		PHASE, D2R, TWOPI
      INTEGER		XBEG, XEND, YBEG, YEND
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL	COST, SINT, X, Y, Z, ULOCAL, VLOCAL, WLOCAL
      LOGICAL 	DOSHIFT
C=====================================================================
      COST(X) = MEMR(CSADD+COFFSET+NCS+NINT(PSCL*MOD(X,2*PI)))
      SINT(X) = MEMR(CSADD+NCS+NINT(PSCL*MOD(X,2*PI)))
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
      TWOPI = 8.0 * ATAN(1.0)
      D2R =  4. * ATAN(1.0) / 180.0
C
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      DO 50 IVIS = 1, NVIS
         VIS(IVIS) = 0.0
  50  CONTINUE
C
      DO 20 IY = 1, NY
         IF(SYSDEBUG.AND.(MOD(IY,10).EQ.0)) THEN
            WRITE (MESSAGE, 1100) IY
 1100       FORMAT ('Row ',I4)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
         DO 10 IX = 1, NX
            IF(IMAGE(IX,IY).NE.0.0) THEN
               X = D2R * DELTX * (FLOAT(IX) - REFX)
               Y = D2R * DELTY * (FLOAT(IY) - REFY)
               Z = SQRT(1-X**2-Y**2)-1.0
               IF(SYSDEBUG) THEN
                  WRITE(MESSAGE,*) X,Y,Z
                  CALL MSGPUT (MESSAGE, 'D')
               END IF
               DO 100 IVIS = 1, NVIS
                  IF (WT(IVIS).LE.0.0) GO TO 100
                  ULOCAL = SHIFT(1,1) * U(IVIS) + 
     1               SHIFT(2,1) * V(IVIS) + SHIFT(3,1) * W(IVIS)
                  VLOCAL = SHIFT(1,2) * U(IVIS) + 
     1               SHIFT(2,2) * V(IVIS) + SHIFT(3,2) * W(IVIS)
                  WLOCAL = SHIFT(1,3) * U(IVIS) + 
     1               SHIFT(2,3) * V(IVIS) + SHIFT(3,3) * W(IVIS)
                  PHASE = TWOPI * (ULOCAL * X + VLOCAL * Y
     $               + WLOCAL * Z)
                  VIS(IVIS) = VIS(IVIS) + IMAGE(IX, IY) * 
     $               CMPLX(COS(PHASE), SIN(PHASE))
 100           CONTINUE
             END IF
  10      CONTINUE
  20  CONTINUE
C
C Now phase rotate
C
      IF (DOSHIFT) THEN
         DO 200 IVIS = 1, NVIS
            IF (WT(IVIS).LE.0.0) GO TO 200
            ULOCAL = SHIFT(1,1) * U(IVIS) + 
     1         SHIFT(2,1) * V(IVIS) + SHIFT(3,1) * W(IVIS)
            VLOCAL = SHIFT(1,2) * U(IVIS) + 
     1         SHIFT(2,2) * V(IVIS) + SHIFT(3,2) * W(IVIS)
            WLOCAL = SHIFT(1,3) * U(IVIS) + 
     1         SHIFT(2,3) * V(IVIS) + SHIFT(3,3) * W(IVIS)
            PHASE = TWOPI * (WLOCAL - W(IVIS))
            VIS(IVIS) = VIS(IVIS) * CMPLX(COS(PHASE), SIN(PHASE))
 200     CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
