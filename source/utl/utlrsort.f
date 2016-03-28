C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlrsort.f	1.3    6/7/93
C
      SUBROUTINE UTLRSORT (N, ARRIN, ARROUT)
C
C A simple ascending sort in place
C
C	N	INT	inp	Number of elements in ARROUT
C	ARRIN	REAL	inp	1-D array, unsorted
C	ARROUT	REAL	in/out	1-D Array, sorted
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C	Did away with the ABS when testing in the sort
C				M.A.Holdaway	Sept 21 1992
C	Rewritten to use heapsort.
C				D.S.Briggs	May 27 1993
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		N
      REAL		ARROUT(*), ARRIN(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLRSORT')
C
      INTEGER		I
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 20 I = 1, N
         ARROUT(I) = ARRIN(I)
 20   CONTINUE
C
C Call NR heapsort
C
      IF (N.GT.1) CALL RSORTNRA (N, ARROUT)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C
C NR heapsort
C
      SUBROUTINE RSORTNRA (N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
