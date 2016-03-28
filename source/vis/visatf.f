C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visatf.f	1.2    11/7/90
C
       SUBROUTINE VISATF (NAME)
C
CD Fix Strange AIPS defintion of time i.e. subtract 5 days * (array#-1)
C
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISATF')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, TADD, BADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      INTEGER		ARRAYNO, IVIS
      REAL		BASELINE
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM2(NAME, 'TIME'), NAX, NAXIS, ATYPE, TADD)
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
C
      DO 10 IVIS = 1, NAXIS(1)
         BASELINE = MEMR(BADD+IVIS-1)
         ARRAYNO = 1 + NINT(100.0*(BASELINE - FLOAT(INT(BASELINE))))
         MEMR(TADD+IVIS-1) = MEMR(TADD+IVIS-1) - 5.0 * (ARRAYNO - 1)
 10   CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
