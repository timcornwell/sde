C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdfts2.f	1.3	 7/20/92
C
      SUBROUTINE IMGDFTS2 (VIS, NVIS, WT, U, V, IMAGE, TIMAGE, NX, NY, 
     1   REFX, DELTX, REFY, DELTY, CX1, CY1, CX2, CY2, BMX, BMY, 
     $   GAIN1, NPB1, PBARR1, RADMAX1, GAIN2, NPB2, PBARR2, RADMAX2)
C
CD Pixel-level DFT routine IMG->VIS in 2D, with beam switching, applies 
C primary beam before transformation
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
C	CX1	INT	input	Center of pb
C	CX2	INT	input	Center of switched pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	GAIN1	REAL	input	Gain for PB1
C	NPB1	INT	input	Length of array for Primary beam
C	PBARR1	REAL	input	Primary beam array
C	RADMAX1	REAL	input	Maximum radius allowed for PB
C	GAIN2	REAL	input	Gain for PB2
C	NPB2	INT	input	Length of array for switched Primary beam
C	PBARR2	REAL	input	Switched Primary beam array
C	RADMAX2	REAL	input	Maximum radius allowed for switched PB
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Jan 9 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NPB1, NPB2
      REAL		IMAGE(NX, *), TIMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, CX1, CY1, CX2, CY2,
     $   		BMX, BMY
      REAL		PBARR1(*), RADMAX1, PBARR2(*), RADMAX2,
     $   		GAIN1, GAIN2
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTS2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ1, RAD1, RSCL1, TAPER1
      REAL		YSQ2, RAD2, RSCL2, TAPER2, PSCL
      REAL		PHASE, CF
      INTEGER		XBEG1, XEND1, YBEG1, YEND1
      INTEGER		XBEG2, XEND2, YBEG2, YEND2
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
      RSCL1 = FLOAT(NPB1-1) / RADMAX1
      RSCL2 = FLOAT(NPB2-1) / RADMAX2
C
      XBEG1 = MAX(1, NINT(CX1 - RADMAX1/SQRT(BMX)))
      XEND1 = MIN(NX, NINT(CX1 + RADMAX1/SQRT(BMX)))
      YBEG1 = MAX(1, NINT(CY1 - RADMAX1/SQRT(BMY)))
      YEND1 = MIN(NY, NINT(CY1 + RADMAX1/SQRT(BMY)))
      XBEG2 = MAX(1, NINT(CX2 - RADMAX2/SQRT(BMX)))
      XEND2 = MIN(NX, NINT(CX2 + RADMAX2/SQRT(BMX)))
      YBEG2 = MAX(1, NINT(CY2 - RADMAX2/SQRT(BMY)))
      YEND2 = MIN(NY, NINT(CY2 + RADMAX2/SQRT(BMY)))
      XBEG = MIN (XBEG1, XBEG2)
      YBEG = MIN (YBEG1, YBEG2)
      XEND = MAX (XEND1, XEND2)
      YEND = MAX (YEND1, YEND2)
C
      IF(SYSDEBUG) THEN
         WRITE(MESSAGE,1000) CX1, CY1
 1000    FORMAT ('IMGDFTS2 - pointing center at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
         WRITE(MESSAGE,1000) CX2, CY2
 1001    FORMAT ('IMGDFTS2 - switch pointing at',2(1X,F8.3))
         CALL MSGPUT (MESSAGE, 'D')
      END IF
C
      DO 40 IY = YBEG, YEND
         YSQ1 = BMY * (FLOAT(IY) - CY1)**2
         YSQ2 = BMY * (FLOAT(IY) - CY2)**2
         DO 30 IX = XBEG, XEND
            RAD1 = SQRT(YSQ1 + BMX*(FLOAT(IX)-CX1)**2)
            RAD2 = SQRT(YSQ2 + BMX*(FLOAT(IX)-CX2)**2)
            TAPER1 = 0.
            TAPER2 = 0.
            IF (RAD1.LE.RADMAX1) THEN
               TAPER1 = PBARR1(1 + NINT(RSCL1 * RAD1))
            ENDIF
            IF (RAD2.LE.RADMAX2) THEN
               TAPER2 = PBARR2(1 + NINT(RSCL2 * RAD2))
            ENDIF
            IF (RAD1.LE.RADMAX1 .OR. RAD2.LE.RADMAX2) THEN
               TIMAGE (IX, IY) = GAIN1 * TAPER1 * IMAGE (IX, IY) +
     $            GAIN2 * TAPER2 * IMAGE (IX, IY)
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
