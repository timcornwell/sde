C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdfts2.f	1.2    7/20/92
C
      SUBROUTINE VISDFTS2 (VIS, NVIS, WT, U, V, IMAGE, NX, NY, 
     1   REFX, DELTX, REFY, DELTY, CX1, CY1, CX2, CY2, BMX, BMY, 
     $   GAIN1, NPB1, PBARR1, RADMAX1, GAIN2, NPB2, PBARR2, RADMAX2, 
     $   SUMWT)
C
CD Pixel-level DFT routine VIS->IMG in 2D, applies primary beam after
C transformation
C
C	VIS	CMPLX	input	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	output	Output image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	CX1	INT	input	Center of pb1
C	CX2	INT	input	Center of pb2
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	GAIN1	REAL	input	Gain for PB1
C	NPB1	INT	input	Length of array for Primary beam
C	PBARR1	REAL	input	Primary beam array
C	RADMAX1	REAL	input	Maximum radius allowed
C	GAIN2	REAL	input	Gain for PB2
C	NPB2	INT	input	Length of array for Primary beam, PB2
C	PBARR2	REAL	input	Primary beam array, PB2
C	RADMAX2	REAL	input	Maximum radius allowed, PB2
C	SUMWT	REAL	output	Sum of weights
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Jan 9 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NPB1, NPB2
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, CX1, CY1, CX2, CY2,
     $   		BMX, BMY
      REAL		PBARR1(*), RADMAX1, PBARR2(*), RADMAX2, SUMWT,
     $   		GAIN1, GAIN2
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFTS2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ1, RAD1, TAPER1, RSCL1, PSCL,
     $   		YSQ2, RAD2, TAPER2, RSCL2
      REAL		PHASE, RNORM, CF, SUMZ
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
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      CF =  2. * (4. * ATAN(1.0))**2 / 180.0
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IMAGE(IX, IY) = 0.0
  1      CONTINUE
  2   CONTINUE
C
      RSCL1 = FLOAT(NPB1-1) / RADMAX1
      RSCL2 = FLOAT(NPB2-1) / RADMAX2
C
      SUMWT = 0.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
            SUMWT = SUMWT + WT(IVIS)
  50  CONTINUE
      IF (SUMWT.GT.0.0) THEN
         RNORM = 1.0 / SUMWT
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
      SUMZ = 0.0
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF((U(IVIS).EQ.0.0).AND.(V(IVIS).EQ.0.0)) THEN
            SUMZ = SUMZ + WT(IVIS) * RNORM * REAL(VIS(IVIS))
         ELSE
            DO 15 IX = 1, NX
               PHASE = CF * U(IVIS) * DELTX * (FLOAT(IX) - REFX)
               ROTX(IX) = WT(IVIS) * RNORM * VIS(IVIS) * 
     1            CMPLX(COST(PHASE), -SINT(PHASE))
  15        CONTINUE
            DO 17 IY = 1, NY
               PHASE = CF * V(IVIS) * DELTY * (FLOAT(IY) - REFY)
               ROTY(IY) = CMPLX(COST(PHASE), -SINT(PHASE))
  17        CONTINUE
            DO 20 IY = 1, NY
               DO 10 IX = 1, NX
                  IMAGE(IX, IY) = IMAGE(IX, IY) + REAL(ROTX(IX) *
     $               ROTY(IY))
  10           CONTINUE
  20        CONTINUE
         END IF
 100  CONTINUE
C
C Now add all the single dish data at once
C
      DO 4 IY = 1, NY
         DO 3 IX = 1, NX
            IMAGE(IX, IY) = IMAGE(IX, IY) + SUMZ
 3       CONTINUE
 4    CONTINUE
C
C Take care of primary beam
C
         DO 40 IY = 1, NY
            YSQ1 = BMY * (FLOAT(IY) - CY1)**2
            YSQ2 = BMY * (FLOAT(IY) - CY2)**2
            DO 30 IX = 1, NX
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
               IF (RAD1 .LE. RADMAX1 .OR. RAD2 .LE. RADMAX2) THEN
                  IMAGE (IX, IY) = GAIN1 * TAPER1 * IMAGE (IX, IY) +
     $                             GAIN2 * TAPER2 * IMAGE (IX, IY)
               ELSE
                  IMAGE (IX, IY) = 0.0
               END IF
   30       CONTINUE
   40    CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
