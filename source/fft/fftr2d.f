C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftr2d.f	1.5    2/18/92
C
      SUBROUTINE FFTR2D (RARRAY, XARRAY, WORK, WORK2, RWORK, NX, NY, 
     1   FNU, NV, DIR)
C
CD Compute Two dimensional FFT of complex array. 
C
C   NU, NV, DIR)
C
C	RARRAY	REAL	input	Input real array
C	XARRAY	CMPLX	input	Input complex array
C	WORK	CMPLX	i/o	Work array
C	WORK2	CMPLX	i/o	Work array
C	RWORK	REAL	i/o	Work array
C	NX	INT	input	Length of real array
C	NY	INT	input	Length of real array
C	FNU	INT	input	Length of complex array (full axis)
C	NV	INT	input	Length of complex array
C	DIR	INT	input	Direction of transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Modified to correctly pad odd sized images
C                               R.G Marson      Dec 5 1990
C	Support for odd sized images added.  Now takes full u axis
C	size as input.
C				D.S.Briggs	Jan 21 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, FNU, NV, DIR
      REAL		RARRAY(NX,*), RWORK(*)
      COMPLEX		XARRAY(FNU/2+1,*)
      COMPLEX		WORK(*), WORK2(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTR2D')
C
      INTEGER		IX, IY, NU, FNX, FNY, IBEGY, IENDY
C====================================================================
      IF (ERROR) GO TO 999
C
C Find full sizes of arrays
C
      NU = FNU/2 + 1
      FNX = FNU
      FNY = NV
      IBEGY = (FNY+1)/2 - (NY+1)/2 + 1
      IENDY = (FNY+1)/2 + NY/2
C
      IF (DIR.GT.0) THEN
         DO 8 IY = 1, IBEGY-1
            DO 9 IX = 1, NU
               XARRAY(IX,IY) = 0.0
   9        CONTINUE
   8     CONTINUE
         DO 10 IY = 1, NY
            CALL FFTR1D2 (RARRAY(1,IY), XARRAY(1, IY+IBEGY-1), WORK, 
     1         WORK2, RWORK, NX, FNU, DIR)
  10     CONTINUE
         DO 11 IY = IENDY + 1, FNY
            DO 12 IX = 1, NU
               XARRAY(IX,IY) = 0.0
  12        CONTINUE
  11     CONTINUE
C
         DO 20 IX = 1, NU
            DO 30 IY = 1, FNY
               WORK2(IY) = XARRAY(IX, IY)
  30        CONTINUE
            CALL FFTX1D (WORK2, WORK, RWORK, FNY, DIR)
            DO 40 IY = 1, FNY
               XARRAY(IX,IY) = WORK2(IY)
  40        CONTINUE
  20     CONTINUE
      ELSE
         DO 60 IX = 1, NU
            DO 70 IY = 1, FNY
               WORK2(IY) = XARRAY(IX, IY)
  70        CONTINUE
            CALL FFTX1D (WORK2, WORK, RWORK, FNY, DIR)
            DO 80 IY = 1, FNY
               XARRAY(IX,IY) = WORK2(IY)
  80        CONTINUE
  60     CONTINUE
C
         DO 90 IY = 1, NY
            CALL FFTR1D2 (RARRAY(1,IY), XARRAY(1, IY+IBEGY-1), WORK, 
     1         WORK2, RWORK, NX, FNU, DIR)
  90     CONTINUE
C
      END IF
C
  999 CONTINUE
      END
