C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistaper.f	1.3    10/22/93
C
       SUBROUTINE VISTAPER (NAME, SUB, TAPER, SUBO)
C
CD Taper visibility data
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	TAPER	REAL	input	Taper in arcseconds
C	SUB0	CH*(*)	input	Name of output sub-class
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added MIN & MAXTAPER output
C				D.S.Briggs	Sept 24 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, SUBO
       REAL		TAPER(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTAPER')
C
      REAL		MINTAPER, MAXTAPER
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, 
     1			WTADD, WTOADD, UADD, VADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, 
     1   WTADD)
      CALL DATGETAR (STRM3(NAME, SUBO, 'WT'), NAX, NAXIS, ATYPE, 
     1   WTOADD)
      UADD = DATADD(STRM2(NAME, 'UU'))
      VADD = DATADD(STRM2(NAME, 'VV'))
C
      CALL VISTAPEP (MEMR(WTADD), NAXIS(1), MEMR(UADD), 
     1   MEMR(VADD), MEMR(WTOADD), TAPER, MINTAPER, MAXTAPER)
C
      WRITE (MESSAGE, 1000) MAXTAPER, MINTAPER
 1000 FORMAT ('Max taper applied = ',1PE9.3,'  Min taper = ',E9.3)
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
