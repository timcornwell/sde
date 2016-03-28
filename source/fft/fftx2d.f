C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftx2d.f	1.3	 7/14/97
C
      SUBROUTINE FFTX2D (IARRAY, VARRAY, WORK, WORK2, IWORK, NX, NY, 
     1   NU, NV, DIR)
C
CD Compute Two dimensional FFT of complex array. 
C
C Arguments: CALL FFTX2D (IARRAY, VARRAY, WORK, WORK2, IWORK, NX, NY, 
C      NU, NV, DIR)
C
C	IARRAY	CMPLX	input	Input complex image array
C	VARRAY	CMPLX	input	Input complex vis array
C	WORK	CMPLX	i/o	Work array
C	WORK2	CMPLX	i/o	Work array
C	IWORK	CMPLX	i/o	Work array
C	NX	INT	input	Length of real array
C	NY	INT	input	Length of real array
C	NU	INT	input	Length of complex array
C	NV	INT	input	Length of complex array
C	DIR	INT	input	Direction of transform
C Audit trail:
C	Changed to deal with X-to-X FFT.
C	If there are problems, check in FFTX1D2, which should, but
C	does not, contain some of the stuff in FFTR1D?
C				M.A.Holdaway	Dec 3 1990
C	IBEG & IEND calcs updated to include odd sizes
C				D.S.Briggs	Feb 15 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, NU, NV, DIR
      COMPLEX		IARRAY(NX,*), IWORK(*)
      COMPLEX		VARRAY(NU,*)
      COMPLEX		WORK(*), WORK2(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTX2D')
C
      INTEGER		IX, IY, FNX, FNY, IBEGY, IENDY
C====================================================================
      IF (ERROR) GO TO 999
C
C Find full sizes of arrays
C
      FNX = NU
      FNY = NV
      IBEGY = (FNY+1)/2 - (NY+1)/2 + 1
      IENDY = (FNY+1)/2 + NY/2
C
      IF (DIR.GT.0) THEN
         DO 8 IY = 1, IBEGY-1
            DO 9 IX = 1, NU
               VARRAY(IX,IY) = 0.0
   9        CONTINUE
   8     CONTINUE
         DO 10 IY = 1, NY
            CALL FFTX1D2 (IARRAY(1,IY), VARRAY(1, IY+IBEGY-1), WORK, 
     1         WORK2, IWORK, NX, NU, DIR)
  10     CONTINUE
         DO 11 IY = IENDY + 1, FNY
            DO 12 IX = 1, FNX
               VARRAY(IX,IY) = 0.0
  12        CONTINUE
  11     CONTINUE
C
         DO 20 IX = 1, FNX
            DO 30 IY = 1, FNY
               WORK2(IY) = VARRAY(IX, IY)
  30        CONTINUE
            CALL FFTX1D (WORK2, WORK, IWORK, FNY, DIR)
            DO 40 IY = 1, FNY
               VARRAY(IX,IY) = WORK2(IY)
  40        CONTINUE
  20     CONTINUE
      ELSE
         DO 60 IX = 1, FNX
            DO 70 IY = 1, FNY
               WORK2(IY) = VARRAY(IX, IY)
  70        CONTINUE
            CALL FFTX1D (WORK2, WORK, IWORK, FNY, DIR)
            DO 80 IY = 1, FNY
               VARRAY(IX,IY) = WORK2(IY)
  80        CONTINUE
  60     CONTINUE
C
         DO 90 IY = 1, NY
            CALL FFTX1D2 (IARRAY(1,IY), VARRAY(1, IY+IBEGY-1), WORK, 
     1         WORK2, IWORK, NX, NU, DIR)
  90     CONTINUE
C
      END IF
C
  999 CONTINUE
      END
