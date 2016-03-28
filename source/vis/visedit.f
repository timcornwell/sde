C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visedit.f	1.5    11/23/94
C
       SUBROUTINE VISEDIT (NAME, SUB, MSUB, EDT, THRES, MODE)
C
CD Edit visibility data on the basis on discrepancy from model.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	EDT	CH*(*)	input	Name of Edited data
C       THRES   REAL    input   Threshold in sigma
C	MODE	CH*(*)	input	Mode (not used)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Destroy old scratch arrays
C				T.J.Cornwell	November 22 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, MSUB, EDT, MODE
      REAL		THRES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDIT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			BADD, WTADD, TADD, VSADD, MVSADD, VSNADD, 
     2			MWTADD, WTNADD, NVIS, NANT, NUMINT, NFLAG, 
     3			ORADD, NRADD, NDUMMY
      LOGICAL		DATEXIST
      REAL		ORESID, NRESID, TINT, TINTD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (ERROR) GO TO 999
      CALL DATGETR (NAME, 'TINT', TINT, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         TINT = 1.0
      END IF
      IF (TINT.EQ.0.0) TINT = 1.0
      TINTD = TINT/86400.0
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MVSADD = DATADD (STRM3(NAME, MSUB, 'VIS'))
      MWTADD = DATADD (STRM3(NAME, MSUB, 'WT'))
      VSNADD = DATADD (STRM3(NAME, EDT, 'VIS'))
      WTNADD = DATADD (STRM3(NAME, EDT, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
C Now find out how many integrations are required
C
      CALL PIXNBOX(MEMR(TADD), MEMR(WTADD), NVIS, TINTD, NUMINT)
      IF (NUMINT.LE.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     1      'No integration intervals found')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1000) NUMINT, TINT
 1000    FORMAT ('Found ',I6,' integration intervals, each less than ',
     1     F9.2, ' seconds')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Now make arrays for residuals
C
      IF(DATEXIST(STRM3(NAME, EDT, 'ORES'))) THEN
         CALL DATDELET (STRM3(NAME, EDT, 'ORES'))
      END IF
      IF(DATEXIST(STRM3(NAME, EDT, 'NRES'))) THEN
         CALL DATDELET (STRM3(NAME, EDT, 'NRES'))
      END IF
      NANT = 28
      CALL DATMAKAR (STRM3(NAME, EDT, 'ORES'), 1, NUMINT, 'R',
     1   ORADD)
      CALL DATMAKAR (STRM3(NAME, EDT, 'NRES'), 1, NUMINT, 'R',
     1   NRADD)
C
      CALL VISEDITP (MEMX(VSADD), MEMX(MVSADD), MEMR(BADD), 
     1   MEMR(TADD), MEMR(WTADD), MEMR(MWTADD), NVIS, NANT, 
     2   TINTD, MODE, MEMX(VSNADD), MEMR(WTNADD), NUMINT, THRES,
     3   MEMR(ORADD), MEMR(NRADD), NFLAG, ORESID, NRESID)
      IF (ERROR) GO TO 990
C
      CALL DATDELET (STRM3(NAME, EDT, 'ORES'))
      CALL DATDELET (STRM3(NAME, EDT, 'NRES'))
C
      WRITE (MESSAGE, 1100) NFLAG
 1100 FORMAT ('Edited ',I8,' records')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) ORESID, NRESID
 1200 FORMAT ('Residual before = ',1PE12.4,', after = ',1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
