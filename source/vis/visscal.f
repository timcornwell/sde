C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visscal.f	1.4    17 Aug 1995
C
       SUBROUTINE VISSCAL (NAME, SUB, MSUB, CAL, MODE)
C
CD Self-Calibrate data. This creates arrays to hold the antenna
C gains. The arrays have names like 'VIS/CAL/I/ANTGAIN' (NANT*NUMINT) 
C complex, and 'VIS/CAL/I/GAINTIME' (NUMINT) real, where NUMINT
C is the number of integration times.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	CAL	CH*(*)	input	Name of calibrated data
C	MODE	CH*(*)	input	' '|'AMPPHI' | 'GLOBAL'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added global mode to allow for solution of simple overall
C	calibration errors
C				T.J.Cornwell	Nov 8 1989
C	Upped the NANT from 28 to 40 for MMA
C				M.A. Holdaway	Aug 17 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MSUB, CAL, MODE
       REAL		FLUX
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSCAL')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, GADD,
     1			BADD, WTADD, TADD, VSADD, MVSADD, VSNADD, 
     2			MWTADD, WTNADD, NVIS, NANT, NUMINT, NFLAG, 
     3			TGADD, ORADD, NRADD, NDUMMY
      REAL		ORESID, NRESID, TINT, TINTD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
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
      VSNADD = DATADD (STRM3(NAME, CAL, 'VIS'))
      WTNADD = DATADD (STRM3(NAME, CAL, 'WT'))
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
C Now make antenna gain array, and solution time array
C
      NANT = 40
      NAXIS(1) = NANT
      NAXIS(2) = NUMINT
      CALL DATMAKAR (STRM3(NAME, CAL, 'ANTGAIN'), 2, NAXIS, 'X',
     1   GADD)
      IF (.NOT.ERROR) THEN
         NAXIS(1) = NUMINT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM3(NAME, CAL, 'GAINTIME'), 1, NAXIS, 'R',
     1      TGADD)
         CALL DATMAKAR (STRM3(NAME, CAL, 'ORES'), 1, NAXIS, 'R',
     1      ORADD)
         CALL DATMAKAR (STRM3(NAME, CAL, 'NRES'), 1, NAXIS, 'R',
     1      NRADD)
      ELSE
         CALL ERRCANCE
         CALL DATGETAR (STRM3(NAME, CAL, 'ANTGAIN'), NAX, NAXIS, 
     1      ATYPE, GADD)
         NANT = NAXIS(1)
         NUMINT = NAXIS(2)
         TGADD = DATADD (STRM3(NAME, CAL, 'GAINTIME'))
         ORADD = DATADD (STRM3(NAME, CAL, 'ORES'))
         NRADD = DATADD (STRM3(NAME, CAL, 'NRES'))
      END IF
C
      CALL VISSCALP (MEMX(VSADD), MEMX(MVSADD), MEMR(BADD), 
     1   MEMR(TADD), MEMR(WTADD), MEMR(MWTADD), NVIS, NANT, 
     2   TINTD, MODE, MEMX(VSNADD), MEMR(WTNADD), NUMINT, MEMX(GADD),
     3   MEMR(TGADD), MEMR(ORADD), MEMR(NRADD), NFLAG, ORESID, NRESID)
      IF (ERROR) GO TO 990
C
      WRITE (MESSAGE, 1100) NFLAG
 1100 FORMAT ('Flagged ',I8,' records')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) ORESID, NRESID
 1200 FORMAT ('Residual before = ',1PE12.4,', after = ',1PE12.4 , 
     1   ' in gain')
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
