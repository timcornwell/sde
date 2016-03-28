C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utldhsrt.f	1.1    12/11/92
C
      SUBROUTINE UTLDHSRT (ARRIN, SIZE, DIR, INDX)
C
C Interface to NR heapsort index code.
C
C Output 
C
C	ARRIN	DBLE	input	1-D array, unsorted
C	SIZE	INT	input	Size of arrays
C	DIR	INT	input	>0 for ascending, otherwise descending
C	INDX	INT	output	1-D Array, such that ARRIN(INDX(J)) is
C				ascending or descending in J
C
C Audit trail:
C	Original version:
C				D.S.Briggs	Nov 16 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		SIZE, DIR, INDX(SIZE)
      DOUBLE PRECISION	ARRIN(SIZE)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLDHSRT')
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (DIR.GT.0) THEN
         CALL NRDHSRTA (SIZE, ARRIN, INDX)
      ELSE
         CALL NRDHSRTD (SIZE, ARRIN, INDX)
      END IF
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
      SUBROUTINE NRDHSRTA(N,ARRIN,INDX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
C
      SUBROUTINE NRDHSRTD(N,ARRIN,INDX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).GT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.GT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

