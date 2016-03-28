C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftstblwr.f	1.3    11/7/90
C
      SUBROUTINE FTSTBLWR (TBLNAME)
C
CD Write out binary data to FITS file from the tables specified.
C
C
C 	TBLNAME	CH*(*)	input	Name of table in directory 
C Audit trail:
C	Fixed up to work. Did not work at all previously.
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
      PARAMETER		(ROUTINE = 'FTSTBLWR')
C
      CHARACTER		ATYPE*1, STRM2*(SYSMXNAM),
     2			COLNAME*(SYSMXNAM),
     4			STRINT*(6),
     6			STRTMP*(SYSMXNAM)
      INTEGER		ANAX, ANAXIS(SYSMXDIM)
      INTEGER		AADD(FTSNCOLS), SCOLS(FTSNCOLS),
     1			ECOLS(FTSNCOLS)
      INTEGER		FILEID, NIO, DATFGETI, L, STRLEN
      INTEGER		IAX, NWROTE, IBLOCK, IADD, NNEXT, NDUMMY
      INTEGER           NROWS, NCOLS, ICOL, NCHARS, CPERB, NTOWRITE
      CHARACTER		CFMT(FTSNCOLS)*8
C
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
C Get array parameters
C
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
         CALL DATGETAR (COLNAME, ANAX, ANAXIS, ATYPE, AADD(ICOL))
         IF (ICOL.EQ.1) THEN
            NROWS = ANAXIS(1)
         ELSE
            IF (ANAXIS(1).NE.NROWS) THEN
               CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     1           'Table columns do not conform')
               GO TO 999
            END IF
         END IF
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
            IF (STRTMP.EQ.ERRTRUNC) THEN
               CALL ERRCANCE
            ELSE
               GOTO 990
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
C Now start writing FITS cards
C
      IBLOCK = 0
      NWROTE = 0
      CPERB = FTSBLOCK/(SYSCHINT*NCHARS)
C
C Start of writing loop
C
   1  CONTINUE
C
C Write some more blocks.
C
      NNEXT = MAX (MIN (CPERB, NROWS - NWROTE), 0)
      NTOWRITE = NNEXT * NCHARS
C
C If no more data then stop 
C
      IF (NNEXT.LE.0) GO TO 200
C
C Write into a buffer, then into the actual file
C
      CALL FTIWRITE (FTSBUFF, NNEXT, NCOLS, SCOLS, ECOLS, IADD, 
     1   AADD, CFMT)
      IF (ERROR) GO TO 990
      CALL FILCWRIT (FTSBUFF, NTOWRITE, FILEID, NIO)
      IF(NIO.NE.NTOWRITE) THEN
         CALL ERRREPOR (ERROUTPT, ROUTINE, 'Could not write all bytes')
         GOTO 999
      END IF
      IF (NIO.EQ.0) GO TO 100
      NWROTE = NWROTE + NNEXT
      IBLOCK = IBLOCK + 1
C
C Go back for more
C
      GO TO 1
C
C If we get here then we ran into an error before all the data had been
C written
C
 100  CONTINUE
      MESSAGE = 'Error on write of table'
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
