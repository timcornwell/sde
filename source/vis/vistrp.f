C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistrp.f	1.2    11/7/90
C
       SUBROUTINE VISTRP (NAME, SUB, MSUB, CAL, MODE)
C
CD Extract visibility data from triple product data i.e. use closure
C phase info. This produces much the same result as VISSCAL but costs
C much more computer time. It is mainly useful as a test bed for the
C triple-product solution routines.
C The model is used as a starting point for the least-squares routines.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	CAL	CH*(*)	input	Name of calibrated data
C      MODE    CH*(*)  input   Mode of solution 'AMPPHI' | ' '
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Renamed from VISBS. Removed creation of antenna gain tables
C	since these are not estimated.
C				T.J.Cornwell	March 15 1989
C      Added MODE
C				T.J.Cornwell	March 15 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MSUB, CAL, MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTRP')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD,
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
C Check type of file
C
      CALL DATCHKTP (NAME, 'VIS')
C
C Get integration time
C
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
         NANT = 28
         NAXIS(1) = NUMINT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM3(NAME, CAL, 'GAINTIME'), 1, NAXIS, 'R',
     1      TGADD)
         CALL DATMAKAR (STRM3(NAME, CAL, 'ORES'), 1, NAXIS, 'R',
     1      ORADD)
         CALL DATMAKAR (STRM3(NAME, CAL, 'NRES'), 1, NAXIS, 'R',
     1      NRADD)
C
      CALL VISTRPPI (MEMX(VSADD), MEMX(MVSADD), MEMR(BADD), 
     1   MEMR(TADD), MEMR(WTADD), MEMR(MWTADD), NVIS, NANT, 
     2   TINTD, MEMX(VSNADD), MEMR(WTNADD), NUMINT, MEMR(TGADD),
     3   MEMR(ORADD), MEMR(NRADD), NFLAG, ORESID, NRESID, MODE)
      IF (ERROR) GO TO 990
C
      WRITE (MESSAGE, 1100) NFLAG
 1100 FORMAT ('Flagged ',I8,' records')
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1200) ORESID, NRESID
 1200 FORMAT ('Residual before = ',1PE12.4,', after = ',1PE12.4 ,
     1    ' Jy')
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
