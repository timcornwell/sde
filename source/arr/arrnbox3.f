C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrnbox3.f	1.1	 3/14/91
C
      SUBROUTINE ARRNBOX3 (NAME, WT, XINC, BEGIN, INTER, ENDING, NINT)
C
C Find number of boxs each of length XINC needed to contain the
C values in an array.
C This version specifies beginning and ending points and does not 
C include in the results spaces between data.
C It also breaks the span up into equal increments. 
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	WT	CH*(*)	input	Name of weights array
C	XINC	REAL	input	Increment
C       BEGIN   REAL    in/out  First value
C       INTER   REAL    output  Interval
C       ENDING  REAL    in/out  Last value 
C	NINT	INT	output	Number of boxs
C
C Audit trail:
C	New subroutine
C				R.T.Duquet	Apr 15 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, WT
      REAL		XINC, BEGIN, ENDING, INTER
      INTEGER		NINT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRNBOX3')
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
         CALL PIXNBOX3(MEMR(ADD), MEMR(WADD), NAXIS(1),
     1                 XINC, BEGIN, INTER, ENDING, NINT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Real arrays only')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

