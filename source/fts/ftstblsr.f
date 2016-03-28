C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftstblsr.f	1.3    11/7/90
C
      SUBROUTINE FTSTBLSR (NAME, TBLLIST)
C
CD Read in TABLES for FITS file for the tables specified. To be read
C in the sub-directory for the table must be listed in TBLLIST.
C Wild cards are allowed in TBLLIST e.g. '*' for all tables.
C
C
C 	NAME		CH*(*)	input	Name of entry in directory 
C	TBLLIST	CH*(*)	input	List of tables to load
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, TBLLIST
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSTBLSR')
C
      CHARACTER*(SYSMXNAM)	STRM2,	STRM3, TBLTEMP, TBLTITLE,
     2   TBLTRGT, REASON, STRTMP
      CHARACTER*6		STRINT
      INTEGER			NDUMMY, NTBLSRED, STRLEN
      LOGICAL			ISEOF, DATEXIST, STRMATCH
C======================================================================
      IF (ERROR) GO TO 999
      NTBLSRED = 0
   1  CONTINUE
C
C Create temporary slot 
C
      TBLTEMP = STRM2(NAME, 'FIITEMP')
      CALL STRAPPEN (TBLTEMP, STRINT(NTBLSRED+1))
      CALL DATCREAT (TBLTEMP)
C
C Now read header into this spot. We did not find a table if
C we found an EOF or if XTENSION does not exist in the sub-directory
C
      CALL FTSREADH (TBLTEMP, ISEOF)
      IF(.NOT.DATEXIST(STRM2(TBLTEMP, 'XTENSION')).OR.ISEOF) THEN
         CALL DATDELET (TBLTEMP)
         GO TO 990
      END IF
C
C Is this a table?
C
      CALL DATGETC (TBLTEMP, 'XTENSION', STRTMP, 1, NDUMMY)
      IF (STRTMP.NE.'TABLE') THEN
         IF (ERROR) THEN
            CALL ERRREASO(REASON)
            IF (REASON.EQ.ERRNTFND) THEN
               CALL ERRCANCE
               GO TO 1
            ELSE 
               GO TO 990
            END IF
         ELSE
            GO TO 1
         END IF
      END IF
C
C Now see if it was one which we wanted. If so then read it in. 
C Otherwise carry on reading until the next table.
C
      CALL DATGETC (TBLTEMP, 'EXTNAME', TBLTITLE, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRREASO(REASON)
         IF (REASON.EQ.ERRNTFND) THEN
            CALL ERRCANCE
            GO TO 1
         ELSE
            GO TO 990
         END IF
      ELSE
         IF (STRMATCH(TBLLIST, TBLTITLE)) THEN
            WRITE (MESSAGE, 1000) TBLTITLE(1:STRLEN(TBLTITLE))
 1000       FORMAT ('Reading FITS table ',A)
            CALL MSGPUT (MESSAGE, 'I')
            TBLTRGT = STRM2(NAME, TBLTITLE)
            CALL DATRENAM (TBLTEMP, TBLTRGT)
            CALL FTSTBLRE (TBLTRGT)
            IF (ERROR) GO TO 990
            NTBLSRED = NTBLSRED + 1
         ELSE
            CALL FTSTBLSK(TBLTEMP)
            IF (ERROR) GO TO 990
         END IF
         IF (ERROR) GO TO 990
      END IF
      GO TO 1      
C
 990  CONTINUE
C
C Check to see that we actually read at least one table
C
      IF ((.NOT.ERROR).AND.(NTBLSRED.EQ.0)) THEN
         CALL MSGPUT ('No tables were found', 'I')
         CALL DATDELET (TBLTEMP)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      END IF
C
 999  CONTINUE
      END
