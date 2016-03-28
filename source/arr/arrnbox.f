C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrnbox.f	1.3    11/7/90
C
      SUBROUTINE ARRNBOX (NAME, WT, XINC, NINT)
C
CD Find number of boxs each of length XINC needed to contain the
C values in an array
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	WT	CH*(*)	input	Name of weights array
C	XINC	REAL	input	Increment
C	NINT	INT	output	Number of boxs
C
C Audit trail:
C	New subroutine
C				T.J.Cornwell	Feb 22 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, WT
      REAL		XINC
      INTEGER		NINT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRNBOX')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, NDUMMY, WADD, DATADD
      INTEGER		RNAX, CRDRNAX
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX (NAX, NAXIS)
      IF(RNAX.GT.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Only valid for 1-D arrays')
         GO TO 999
      END IF
      WADD = DATADD (WT)
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXNBOX(MEMR(ADD), MEMR(WADD), NAXIS(1), XINC, NINT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Real arrays only')
         GO TO 999
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
