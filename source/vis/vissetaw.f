C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissetaw.f	1.1	11/14/90
C
       SUBROUTINE VISSETAW (NAME, SUB, AUTOWT)
C
CD  Change AUTOCORRELATION WEIGHT in a visibility data set
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	AUTOWT	REAL	inp	New AUTOCORRELATION WT
C Audit trail:
C	New Routine
C				M.A.Holdaway	Nov 12 1990
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB
      REAL		AUTOWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSETAW')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, 
     $   		BADD, WTADD, NVIS
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      NVIS = NAXIS(1)
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL VISSEAWP (NVIS, MEMR(BADD), MEMR(WTADD), AUTOWT)
C
      WRITE (MESSAGE, 1100) AUTOWT
 1100 FORMAT ('Autocorrelation Weight changed to ',1F5.3)
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
