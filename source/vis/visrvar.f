C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrvar.f	1.2    11/7/90
C
       SUBROUTINE VISRVAR (NAME, CLASS, RVAR)
C
CD Find Reciprocal variances from dispersion of data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	CLASS	CH*(*)	input	CLASS E.G. 'OBS/I'
C	RVAR	CH*(*)	input	Name of output for inverse variances
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, CLASS, RVAR
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRVAR')
C
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
C
      INTEGER		IAX, NAX, NANT, DATADD, NAXIS(SYSMXDIM),
     1			VSADD, WTADD, BADD, RVARADD, NVIS
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(NAME, CLASS, 'VIS'), NAX, NAXIS, ATYPE, 
     1   VSADD)
      CALL DATGETAR (STRM3(NAME, CLASS, 'WT'), NAX, NAXIS, ATYPE, 
     1   WTADD)
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
C
      ATYPE = 'R'
      CALL DATMAKAR (RVAR, NAX, NAXIS, ATYPE, RVARADD)
C
      CALL VISRVARP (MEMX(VSADD), MEMR(BADD), MEMR(WTADD), NAXIS(1), 
     1   MAXNANT, MEMR(RVARADD))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
