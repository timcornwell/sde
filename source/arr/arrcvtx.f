C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)arrcvtx.f	1.1    8/27/92
C
      SUBROUTINE ARRCVTX (IN, OUT)
C
CD Convert array to complex
C
C	IN	CH*(*)	input	Name of input array
C	OUT	CH*(*)	input	Name of output array
C
C IN may be the same as OUT
C
C Audit trail:
C       Original version:  Only R->X implemented.
C                               D.S.Briggs      Aug 26 1992
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ARRCVTX')
C
      CHARACTER*(*)     IN, OUT
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NDUMMY
      CHARACTER*1	ATYPE
C
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, NDUMMY)
C
      IF (ATYPE.NE.'R') THEN
         MESSAGE = 'Promotion of type ' // ATYPE //
     $      ' to X not implemented'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IF (ATYPE.EQ.'R') THEN
         CALL ARRQU2X (IN, 'NULL', 'CVT-TEMPX')
         IF (DATEXIST(OUT)) THEN
            CALL DATDELAR (OUT)
            CALL DATMAKAR (OUT, NAX, NAXIS, 'X', NDUMMY)
            CALL ARRCOPY ('CVT-TEMPX', OUT)
            CALL DATDELET ('CVT-TEMPX')
         ELSE
            CALL DATRENAM ('CVT-TEMPX', OUT)
         END IF
      END IF
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
