C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrscpy.f	1.2    6/7/93
C
      SUBROUTINE PIXRSCPY (IN, WT, OUT, N1, N2, N3, N4, N5, N6, N7,
     $   M1, M2, M3, M4, M5, M6, M7, MT, DELTA, NOUT, NCOP)
C
CD Copy an array conditionally on a weight
C
C	IN	REAL	input	Input array
C       WT      REAL    input   Weight array
C	OUT	REAL	output	Output array  (1D)
C	N1-N7	INT	input	Dimensions of IN
C	M1-M7	INT	input	Dimensions of WT
C	MT	INT	input	Total pixels of WT
C	DELTA	INT(*)	input	Offset of WT wrt INPUT
C	NOUT	INT	input	Size of output array
C	NCOP	INT	output	Number of elements copied on output
C
C	On axis I, WT(K) is the same pixel as IN(K+DELTA(I))
C	WT is dimensioned here as a single dimension for speed reasons
C	but it is actually a multidimensional array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	August 30 1991
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	N1, N2, N3, N4, N5, N6, N7
      INTEGER	M1, M2, M3, M4, M5, M6, M7, MT
      INTEGER	NOUT, NCOP, DELTA(7)
      REAL	IN(N1,N2,N3,N4,N5,N6,N7)
      REAL	WT(MT)
      REAL	OUT(NOUT)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRSCPY')
C
      INTEGER	IM, I1, I2, I3, I4, I5, I6, I7
C=========================================================================
      IF (ERROR) GO TO 999
C
      NCOP = 0
      IM = 0
C
      DO 70 I7 = DELTA(7)+1, DELTA(7)+M7
         DO 60 I6 = DELTA(6)+1, DELTA(6)+M6
            DO 50 I5 = DELTA(5)+1, DELTA(5)+M5
               DO 40 I4 = DELTA(4)+1, DELTA(4)+M4
                  DO 30 I3 = DELTA(3)+1, DELTA(3)+M3
                     DO 20 I2 = DELTA(2)+1, DELTA(2)+M2
                        DO 10 I1 = DELTA(1)+1, DELTA(1)+M1
                           IM = IM + 1
                           IF (WT(IM).GT.0.0) THEN
                              NCOP = NCOP + 1
                              IF (NCOP.GT.NOUT) THEN
                                 CALL ERRREPOR (ERRNOMEM, ROUTINE,
     $                              'Size of output array exceeded')
                                 GO TO 999
                              END IF
                              OUT(NCOP) = OUT(NCOP) +
     $                           IN(I1,I2,I3,I4,I5,I6,I7)
                           END IF
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE
C
 999  CONTINUE
      END
