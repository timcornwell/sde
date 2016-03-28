C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftr3d.f	1.4    2/18/92
C
      SUBROUTINE FFTR3D (RARRAY, XARRAY, WORK, WORK2, RWORK, NX, NY, 
     1   NZ, FNU, NV, NW, DIR)
C
CD Compute Three dimensional FFT of complex array. 
C
C	RARRAY	REAL	input	Input real array
C	XARRAY	CMPLX	input	Input complex array
C	WORK	CMPLX	i/o	Work array
C	WORK2	CMPLX	i/o	Work array
C	RWORK	REAL	i/o	Work array
C	NX	INT	input	Length of real array
C	NY	INT	input	Length of real array
C	NZ	INT	input	Length of real array
C	FNU	INT	input	Length of complex array (full axis)
C	NV	INT	input	Length of complex array
C	NW	INT	input	Length of complex array
C	DIR	INT	input	Direction of transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Support for odd sized images added.  Now takes full u axis
C	size as input.
C				D.S.Briggs	Jan 21 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NZ, FNU, NV, NW, DIR
      REAL		RARRAY(NX,NY,*), RWORK(*)
      COMPLEX		XARRAY(FNU/2+1,NV,*)
      COMPLEX		WORK(*), WORK2(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTR3D')
C
      INTEGER		IX, IY, IZ, NU, FNX, FNY, FNZ, IBEGY, IENDY,
     1			IBEGZ, IENDZ
C====================================================================
      IF (ERROR) GO TO 999
C
C Find full sizes of arrays
C
      NU = FNU/2+1
      FNX = FNU
      FNY = NV
      FNZ = NW
      IBEGY = (FNY+1)/2 - (NY+1)/2 + 1
      IENDY = (FNY+1)/2 + NY/2
      IBEGZ = (FNZ+1)/2 - (NZ+1)/2 + 1
      IENDZ = (FNZ+1)/2 + NZ/2
C
      IF (DIR.GT.0) THEN
         DO 7 IZ = 1, IBEGZ-1
            DO 8 IY = 1, NV
               DO 9 IX = 1, NU
                  XARRAY(IX,IY,IZ) = 0.0
   9           CONTINUE
   8        CONTINUE
   7     CONTINUE
C
         DO 10 IZ = 1, NZ
            CALL FFTR2D (RARRAY(1,1,IZ), XARRAY(1, 1, IZ+IBEGZ-1), 
     1         WORK, WORK2, RWORK, NX, NY, FNU, NV, DIR)
  10     CONTINUE
         DO 11 IZ = IENDZ + 1, FNZ
            DO 12 IY = 1, NV
               DO 13 IX = 1, NU
                  XARRAY(IX,IY,IZ) = 0.0
  13           CONTINUE
  12        CONTINUE
  11     CONTINUE
C
         DO 20 IY = 1, NV
            DO 30 IX = 1, NU
               DO 35 IZ = 1, FNZ
                  WORK2(IZ) = XARRAY(IX, IY, IZ)
  35           CONTINUE
               CALL FFTX1D (WORK2, WORK, RWORK, FNZ, DIR)
               DO 40 IZ = 1, FNZ
                  XARRAY(IX,IY,IZ) = WORK2(IZ)
  40           CONTINUE
  30        CONTINUE
   20     CONTINUE
      ELSE
         DO 50 IY = 1, NV
            DO 60 IX = 1, NU
               DO 70 IZ = 1, FNZ
                  WORK2(IZ) = XARRAY(IX, IY, IZ)
  70           CONTINUE
               CALL FFTX1D (WORK2, WORK, RWORK, FNZ, DIR)
               DO 80 IZ = 1, FNZ
                  XARRAY(IX,IY,IZ) = WORK2(IZ)
  80           CONTINUE
  60        CONTINUE
  50     CONTINUE
C
         DO 90 IZ = 1, NZ
            CALL FFTR2D (RARRAY(1,1,IZ), XARRAY(1, 1, IZ+IBEGZ-1), 
     1         WORK, WORK2, RWORK, NX, NY, FNU, NV, DIR)
  90     CONTINUE
C
      END IF
C
  999 CONTINUE
      END
