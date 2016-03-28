C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrmode.f	1.2    1/22/92
C
      SUBROUTINE PIXRMODE (IN, NIN, BUFR, ACC, CNTR, NBUFF, EPS, MODE)
C
CD Find the mode of an array.  (Real, pixel level)
C
C If NBUFF is too small, this may return an incorrect result.
C
C	IN	R(*)	input	Input array
C	NIN	I	input	Size of IN
C	BUFR	R(*)	input	Buffer
C	ACC	R(*)	input	Accumulator for averaging
C	CNTR	I(*)	input	Counter array for averaging
C	NBUFF	I	input	Size of BUFR, ACC, CNTR
C	EPS	R	input	Fractional accuracy for two number to be equal
C	MODE	R	output	Estimated mode
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	Sept 29 1991
C       Found the following which complied ok on most machines!
C         IF (ABS(CNTR(I).GT.ABS(CNTR(IMATCH)))) IMATCH = I
C				T.J. Cornwell	Jan 22 1992
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	IN(*), BUFR(*), ACC(*), EPS, MODE
      INTEGER	NIN, CNTR(*), NBUFF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRMODE')
C
      REAL		EPS2, VAL
      INTEGER		I, J, IMATCH, NVALUES
      LOGICAL		DOMSG
C=========================================================================
      IF (ERROR) GO TO 999
C
      DOMSG = .FALSE.
      NVALUES = 0
      EPS2 = EPS / 2.0
      DO 300 I = 1, NIN
C
C Find a value that matches in the table of accumulated values
C
         VAL = IN(I)
         IMATCH = 0
         DO 100 J = 1, NVALUES
            IF (ABS((VAL-BUFR(J))/(VAL+BUFR(J))).LE.EPS2) THEN
               IMATCH = J
               GO TO 200
             END IF
 100     CONTINUE
 200     CONTINUE
C
C Stash it or bump count, as needed.
C
         IF (IMATCH.NE.0) THEN
            CNTR(IMATCH) = SIGN(ABS(CNTR(IMATCH)) + 1, CNTR(IMATCH))
            IF (VAL.NE.BUFR(IMATCH)) CNTR(IMATCH) = -ABS(CNTR(IMATCH))
            ACC(IMATCH) = ACC(IMATCH) + VAL
         ELSE
            IF (NVALUES.LT.NBUFF) THEN
               NVALUES = NVALUES + 1
               CNTR(NVALUES) = 1
               BUFR(NVALUES) = VAL
               ACC(NVALUES) = VAL
            ELSE
               DOMSG = .TRUE.
            END IF
         END IF
 300  CONTINUE
C
C Now scan for the most common value
C
      IMATCH = 1
      DO 400 I = 2, NVALUES
         IF (ABS(CNTR(I)).GT.ABS(CNTR(IMATCH))) IMATCH = I
 400  CONTINUE
C
C The idea is to return the exact bit pattern in the event that all the
C inputs were the same, but an average, if any were different.
C
      IF (CNTR(IMATCH).GT.0) THEN
         MODE = BUFR(IMATCH)
      ELSE
         MODE = ACC(IMATCH) / REAL(ABS(CNTR(IMATCH)))
      END IF
C
C Issue a warning if we exceeded our buffers.
C
      IF (DOMSG) THEN
         CALL MSGPUT ('Warning: Internal Buffer Exceeded in PIXRMODE.',
     $      'W')
         WRITE (MESSAGE, 501) MODE
 501     FORMAT ('Returned MODE of',1PG15.7E2,' is suspect')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C
 999  CONTINUE
      END
