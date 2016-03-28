C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcstype.f	1.4    1/28/93
C
       SUBROUTINE IPCSTYPE (HANDLE, NAME)
C
CD Type ipcs data
C
C	HANDLE	CH*(*)	input	HANDLE of output stream
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Cloned from vislist
C				T.J.Cornwell	Jan 24 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, HANDLE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSTYPE')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, PHADD,
     1			XADD, YADD, FRADD, NPHOTONS
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM) STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, PHADD)
      NPHOTONS = NAXIS(2)
      XADD = DATADD (STRM2(NAME, 'XPIXEL'))
      YADD = DATADD (STRM2(NAME, 'YPIXEL'))
      FRADD = DATADD (STRM2(NAME, 'FRAME'))
C
      CALL IPCSTYPEP (HANDLE, MEMR(PHADD), MEMR(XADD), MEMR(YADD), 
     1   MEMR(FRADD), NPHOTONS)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
