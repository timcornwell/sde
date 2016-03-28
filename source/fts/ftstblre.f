C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftstblre.f	1.3    11/7/90
C
      SUBROUTINE FTSTBLRE (TBLNAME)
C
CD Read in binary data from FITS file into the tables specified.
C
C
C 	TBLNAME	CH*(*)	input	Name of table in directory 
C Audit trail:
C	Added more information to EOF message. Fixed handling of card
C	length.
C				T.J.Cornwell	Jan 8 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	TBLNAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSTBLRE')
C
      CHARACTER		ATYPE*1, STRM2*(SYSMXNAM),
     1			STRM3*(SYSMXNAM),
     2			COLNAME*(SYSMXNAM),
     4			STRINT*(6),
     6			STRTMP*(SYSMXNAM)
      INTEGER		ANAX, ANAXIS(SYSMXDIM)
      INTEGER		AADD(FTSNCOLS), SCOLS(FTSNCOLS), 
     1			ECOLS(FTSNCOLS), DATADD
      INTEGER		FILEID, NIO, DATFGETI, L, STRLEN
      INTEGER		IAX, NREAD, IBLOCK, IADD, 
     1			NELEMENT, NNEXT, CPERB, NCHARS, NTOREAD
      INTEGER           NROWS, NCOLS, ICOL, NDUMMY
      CHARACTER		CFMT(FTSNCOLS)*8
C
      REAL		DATFGETR
      INTEGER 		FTSBUFF (FTSBLOCK / SYSCHINT)
C====================================================================
      IF (ERROR) GOTO 999
C
C Make the table columns first
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
      ANAX = 1
      ATYPE = 'R'
      ANAXIS(1) = NROWS
      IF (ERROR) GO TO 990
      DO 10 ICOL = 1, NCOLS
C
C COLNAME will be something like: 'IMAGE/AIPS CC/FLUX'
C
         STRTMP = 'TTYPE'
         CALL STRAPPEN (STRTMP, STRINT(ICOL))
         CALL DATGETC (TBLNAME, STRTMP, COLNAME, 1, NDUMMY)
         COLNAME = STRM2(TBLNAME, COLNAME)
C
C Make array entry for each column, and find address.
C
         CALL DATMAKAR (COLNAME, ANAX, ANAXIS, ATYPE, AADD(ICOL))
C
C Get the beginning of each column
C
         STRTMP = 'TBCOL'
         CALL STRAPPEN (STRTMP, STRINT(ICOL))
         SCOLS(ICOL) = DATFGETI (TBLNAME, STRTMP)
C
C Get the format for each column
C
         STRTMP = 'TFORM'
         CALL STRAPPEN (STRTMP, STRINT(ICOL))
         CALL DATGETC (TBLNAME, STRTMP, CFMT(ICOL), 1, NDUMMY)
         IF(ERROR) THEN
            CALL ERRREASO (STRTMP)
            IF(STRTMP.EQ.ERRTRUNC) THEN
               CALL ERRCANCE
            ELSE
               GO TO 990
            END IF
         END IF
         L = STRLEN(CFMT(ICOL))
         STRTMP(1:L) = CFMT(ICOL)(1:L)
         CFMT(ICOL)  = '('//STRTMP(1:L)//')'
         IF (ERROR) GO TO 990
  10  CONTINUE
C
C Fill in end columns
C
      DO 5 ICOL = 1, NCOLS - 1
          ECOLS(ICOL) = SCOLS(ICOL+1) - 1
  5   CONTINUE
      ECOLS(NCOLS) = NCHARS
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
C Read some more cards into the appropriate place.
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
      CALL FTIREADT (FTSBUFF, NNEXT, NCOLS, SCOLS, ECOLS, IADD, 
     1   AADD, CFMT)
      IF (ERROR) GO TO 990
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
      WRITE(MESSAGE,1000) NREAD, NROWS
 1000 FORMAT('Premature EOF : only read ',I6,' of ',I6,
     1   ' records expected')
      CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
      GO TO 999
C
C Found correct number of rows
C
 200  CONTINUE
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
