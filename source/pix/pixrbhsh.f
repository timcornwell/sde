C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrbhsh.f	1.1    6/8/94
C
      INTEGER FUNCTION PIXRBHSH (A, N)
C
CD Return a hash value for a real binary array
C
C	A	REAL	input	Real array
C	N	INT	input	Number of elements
C
C The hash value will be between 0 and IM-1.  (The constants are
C from the table on page 198 of NR.)
C
C Audit trail:
C	Original version:
C				D.S.Briggs	Nov 30 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A(*)
      INTEGER		N
C
      INTEGER		I, IST, IEND, J, JEND, V, HASH
C
      INTEGER		IM, IA, IC
      PARAMETER		(IM=259200, IA=7141, IC=54773)
      INTEGER		NBITS
      PARAMETER		(NBITS=18)
C=====================================================================
      HASH = 0
      JEND = N / NBITS
      IST = 1
      IEND = IST + NBITS - 1
C
C Main loop over all the complete NBIT chunks
C
      DO 110 J = 1, JEND
         V = 0
         IF (A(IST).NE.0.0) V = 1
         DO 100 I = IST+1, IEND
            V = V * 2
            IF (A(I).NE.0.0) V = V + 1
 100     CONTINUE
         HASH = MOD(HASH*IA+IC+V,IM)
         IST = IST + NBITS
         IEND = IEND + NBITS
 110  CONTINUE
C
C Finish up any loose ends
C      
      IEND = N - JEND * NBITS + IST - 1
      IF (IEND.GE.IST) THEN
         V = 0
         IF (A(IST).NE.0.0) V = 1
         DO 120 I = IST+1, IEND
            V = V * 2
            IF (A(I).NE.0.0) V = V + 1
 120     CONTINUE
         HASH = MOD(HASH*IA+IC+V,IM)
      END IF
C
      PIXRBHSH = HASH
C
 999  CONTINUE
      END

