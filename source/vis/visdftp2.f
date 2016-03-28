C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdftp2.f	1.2    11/7/90
C
      SUBROUTINE VISDFTP2 (VIS, NVIS, WT, U, V, IMAGE, NX, NY, 
     1   REFX, DELTX, REFY, DELTY, CX, CY, BMX, BMY, NPB, PBARR,
     2   RADMAX, SUMWT)
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
C	CX	INT	input	Center of pb
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	NPB	INT	input	Length of array for Primary beam
C	PBARR	REAL	input	Primary beam array
C	RADMAX	REAL	input	Maximum radius allowed
C	SUMWT	REAL	output	Sum of weights
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Special case for u=v=0
C				T.J. Cornwell	Oct 26 1989
C	Defer adding u=v=0 point until all data has been processed
C				T.J. Cornwell	April 18 1990
C	Added sum of weights
C				T.J. Cornwell	May 22 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NPB
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, CX, CY, BMX, BMY
      REAL		PBARR(*), RADMAX, SUMWT
      COMPLEX		VIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFTP2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ, RAD, TAPER, RSCL, PSCL
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
      RSCL = FLOAT(NPB-1) / RADMAX
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
      DO 40 IY = 1, NY
         YSQ = BMY * (FLOAT(IY) - CY)**2
         DO 30 IX = 1, NX
            RAD = SQRT(YSQ + BMX*(FLOAT(IX)-CX)**2)
            IF (RAD.LE.RADMAX) THEN
               TAPER = PBARR(1 + NINT(RSCL * RAD))
               IMAGE (IX, IY) = TAPER * IMAGE(IX, IY)
            ELSE
               IMAGE (IX, IY) = 0.0
            END IF
   30    CONTINUE
   40 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
