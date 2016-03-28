C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)arrcvtd.f	1.1    6/7/93
C
      SUBROUTINE ARRCVTD (IN, OUT)
C
CD Convert array to double precision
C
C	IN	CH*(*)	input	Name of input array
C	OUT	CH*(*)	input	Name of output array
C
C IN may be the same as OUT
C
C Audit trail:
C       Original version:  Only R->D implemented.
C                               D.S.Briggs      Nov 24 1992
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ARRCVTD')
C
      CHARACTER*(*)     IN, OUT
C
      CHARACTER*(*)	TEMPNAME
      PARAMETER		(TEMPNAME = ROUTINE//'-TMP')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD, DADD, RNAX, I, NT
      CHARACTER*1	ATYPE
C
      INTEGER		DATADD, CRDRNAX
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, IADD)
      IF (ATYPE.EQ.'D') GO TO 990
C
      IF (ATYPE.NE.'R') THEN
         MESSAGE = 'Promotion of type ' // ATYPE //
     $      ' to D not implemented'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      RNAX = CRDRNAX (NAX, NAXIS)
      NT = 1
      DO 10 I = 1, RNAX
         NAXIS(I) = MAX(1, NAXIS(I))
         NT = NT * NAXIS(I)
  10  CONTINUE
C
      CALL ARRCOPY (IN, TEMPNAME)
      CALL DATDELAR (IN)
      IADD = DATADD(TEMPNAME)
      CALL DATMAKAR (OUT, NAX, NAXIS, 'D', DADD)
C      
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRDCPY (MEMR(IADD), 1, MEMD(DADD), 1, NT)
      END IF
C
      CALL DATDELET (TEMPNAME)
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
