C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdftp2.f	1.3    11/7/90
C
      SUBROUTINE IMGDFTP2 (VIS, NVIS, WT, U, V, IMAGE, TIMAGE, NX, NY, 
     1   REFX, DELTX, REFY, DELTY, CX, CY, BMX, BMY, NPB, PBARR,
     2   RADMAX)
C
CD Pixel-level DFT routine IMG->VIS in 2D, applies primary beam before
C transformation
C
C	VIS	CMPLX	output	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	input	Input image
C	TIMAGE	REAL	output	Tapered output image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	CX	INT	input	Center of pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	NPB	INT	input	Length of array for Primary beam
C	PBARR	REAL	input	Primary beam array
C	RADMAX	REAL	input	Maximum radius allowed for PB
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added special case for u=v=0.
C				T.J. Cornwell	Oct 26 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NPB
      REAL		IMAGE(NX, *), TIMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, CX, CY, BMX, BMY
      REAL		PBARR(*), RADMAX
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTP2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ, RAD, RSCL, TAPER, PSCL
      REAL		PHASE, CF
      INTEGER		XBEG, XEND, YBEG, YEND
C
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
C
      REAL	COST, SINT, X
C=====================================================================
      COST(X) = MEMR(CSADD+COFFSET+NCS+NINT(PSCL*MOD(X,2*PI)))
      SINT(X) = MEMR(CSADD+NCS+NINT(PSCL*MOD(X,2*PI)))
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NPB-1) / RADMAX
C
      XBEG = MAX(1, NINT(CX - RADMAX/SQRT(BMX)))
      XEND = MIN(NX, NINT(CX + RADMAX/SQRT(BMX)))
      YBEG = MAX(1, NINT(CY - RADMAX/SQRT(BMY)))
      YEND = MIN(NY, NINT(CY + RADMAX/SQRT(BMY)))
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX, CY
 1000    FORMAT ('IMGDFTP2 - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
      DO 40 IY = YBEG, YEND
         YSQ = BMY * (FLOAT(IY) - CY)**2
         DO 30 IX = XBEG, XEND
            RAD = SQRT(YSQ + BMX*(FLOAT(IX)-CX)**2)
            IF (RAD.LE.RADMAX) THEN
               TAPER = PBARR(1 + NINT(RSCL * RAD))
               TIMAGE (IX, IY) = TAPER * IMAGE(IX, IY)
            ELSE
               TIMAGE (IX, IY) = 0.0
            END IF
   30    CONTINUE
   40 CONTINUE
C
      CF =  2. * (4. * ATAN(1.0))**2 / 180.0
C
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         VIS(IVIS) = 0.0
         IF((U(IVIS).EQ.0.0).AND.(V(IVIS).EQ.0.0)) THEN
            DO 2 IY = YBEG, YEND
               DO 1 IX = XBEG, XEND
                  VIS(IVIS) = VIS(IVIS) + TIMAGE(IX, IY) 
  1            CONTINUE
  2         CONTINUE
         ELSE
            DO 15 IX = XBEG, XEND
               PHASE = CF * U(IVIS) * DELTX * (FLOAT(IX) - REFX)
               ROTX(IX) = CMPLX(COST(PHASE), SINT(PHASE))
  15        CONTINUE
            DO 17 IY = YBEG, YEND
               PHASE = CF * V(IVIS) * DELTY * (FLOAT(IY) - REFY)
               ROTY(IY) = CMPLX(COST(PHASE), SINT(PHASE))
  17        CONTINUE
            DO 20 IY = YBEG, YEND
               DO 10 IX = XBEG, XEND
                  VIS(IVIS) = VIS(IVIS) + TIMAGE(IX, IY) * ROTX(IX) *
     $               ROTY(IY)
  10           CONTINUE
  20        CONTINUE
         ENDIF
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
