C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftstblsk.f	1.3    11/7/90
C
      SUBROUTINE FTSTBLSK (TBLNAME)
C
CD Skip binary data from FITS file.
C
C
C 	TBLNAME	CH*(*)	input	Name of table in directory 
C Audit trail:
C	Fixed to work properly. Previously would read an incorrect
C	number of cards
C				T.J.Cornwell	Jan 9 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	TBLNAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSTBLSK')
C
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER		FILEID, NIO, DATFGETI, L, STRLEN
      INTEGER		IAX, NREAD, IBLOCK, IADD, 
     1			NELEMENT, NNEXT
      INTEGER           NROWS, NCOLS, ICOL, NCHARS, NTOREAD, CPERB
C
      REAL		DATFGETR
      INTEGER 		FTSBUFF (FTSBLOCK / SYSCHINT)
C====================================================================
      IF (ERROR) GOTO 999
C
C Get definitions
C
      NCHARS = DATFGETI(TBLNAME, 'NAXIS1')
      IF(ERROR.OR.(NCHARS.EQ.0)) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'Cannot find NAXIS1')
         GO TO 999
      END IF
      NROWS = DATFGETI(TBLNAME, 'NAXIS2')
      IF(ERROR.OR.(NROWS.EQ.0)) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'Cannot find NAXIS2')
         GO TO 999
      END IF
      NCOLS = DATFGETI(TBLNAME, 'TFIELDS')
      IF(ERROR.OR.(NCOLS.EQ.0)) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'Cannot find TFIELDS')
         GO TO 999
      END IF
C
C Get logical unit number for access
C
      FILEID = DATFGETI (TBLNAME, 'FILEID')
C
C Now start reading FITS cards
C
      IBLOCK = 0
      NREAD = 0
      CPERB = FTSBLOCK/(SYSCHINT*NCHARS)
C
C Start of reading loop
C
   1  CONTINUE
C
C Read some more blocks into the appropriate place.
C
      NNEXT = MAX (MIN (CPERB, NROWS - NREAD), 0)
      NTOREAD = NNEXT * NCHARS
C
C If no more data then stop 
C
      IF (NNEXT.LE.0) GO TO 200
C
C Read into a buffer, then into the actual array
C
      CALL FILCREAD (FTSBUFF, NTOREAD, FILEID, NIO)
      IF (NIO.EQ.0) GO TO 100
      NREAD = NREAD + NNEXT
      IBLOCK = IBLOCK + 1
C
C Go back for more
C
      GO TO 1
C
C If we get here then we ran into an EOF before all the data had been
C read!
C
 100  CONTINUE
      MESSAGE = 'Premature End of File'
      CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
      GO TO 999
C
C Found correct number of rows
C
 200  CONTINUE
C
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
