C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgclone.f	1.3    11/7/90
C
      SUBROUTINE IMGCLONE (IN, OUT)
C
CD Clone an image to another directory entry. Do not copy array: only
C the header items.
C
C Arguments : CALL IMGCLONE (IN, OUT)
C
C	IN	CH*(*)	input	Name of input image
C	OUT	CH*(*)	input	Name of output image
C Audit trail:
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCLONE')
C
      INTEGER		ADD, IAX, NAX, NAXIS(SYSMXDIM)
      CHARACTER*1	ATYPE
      LOGICAL		DATEXIST
C======================================================================
      IF (ERROR) GO TO 999
C
      IF(.NOT.DATEXIST(IN)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Original does not exist')
         GO TO 999
      END IF
C
      IF(DATEXIST(OUT)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Target already exists')
         GO TO 999
      END IF
C
      CALL DATCREAT (OUT)
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, ADD)
      CALL DATMAKAR (OUT, NAX, NAXIS, ATYPE, ADD)
C
      CALL HEDCOPY (IN, OUT)
C
      CALL HISCOPY (IN, OUT)
C
      IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
