C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imglink.f	1.3    11/7/90
C
      SUBROUTINE IMGLINK (OLD, NEW)
C
CD LINK an image to another directory entry. Do not copy array: only
C the header items. The array data is that of the OLD image.
C
C Arguments : CALL IMGLINK (NEW, OLD)
C
C	NEW	CH*(*)	input	Name of original image
C	OLD	CH*(*)	input	Name of linked (i.e. new) image
C Audit trail:
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NEW, OLD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGLINK')
C
      LOGICAL		DATEXIST
      INTEGER		STRLEN
C======================================================================
      IF (ERROR) GO TO 999
C
      IF(.NOT.DATEXIST(OLD)) THEN
         WRITE (MESSAGE, 1000) OLD(1:STRLEN(OLD))
 1000    FORMAT ('Original, ',A,' does not exist')
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IF(DATEXIST(NEW)) THEN
         WRITE (MESSAGE, 1010) NEW(1:STRLEN(NEW))
 1010    FORMAT ('Target, ',A,' already exists')
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      CALL DATCREAT (NEW)
C
      CALL HEDCOPY (OLD, NEW)
C
      CALL HISCOPY (OLD, NEW)
C
      CALL DATLNARR (OLD, NEW)
C
      IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
