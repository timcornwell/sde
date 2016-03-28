C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftstblsw.f	1.3    11/7/90
C
      SUBROUTINE FTSTBLSW (NAME, TBLLIST)
C
CD Write out TABLES for FITS file for the tables specified. To be 
C written the table must be listed in TBLLIST. No wild cards
C are allowed.
C
C
C 	NAME		CH*(*)	input	Name of entry in directory 
C	TBLLIST	CH*(*)	input	List of tables to write
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
      PARAMETER		(ROUTINE = 'FTSTBLSW')
C
      CHARACTER*(SYSMXNAM)	STRM2,	TBLTRGT, REASON, 
     1   STRTMP
      INTEGER			NDUMMY, NTBLSWRT, STRLEN
C======================================================================
      IF (ERROR) GO TO 999
      NTBLSWRT = 0
   1  CONTINUE
C
C Find table to write
C
      CALL STREXTRA (TBLLIST, NTBLSWRT+1, STRTMP)
      IF (STRTMP.EQ.' ') GO TO 990
      WRITE (MESSAGE, 1000) STRTMP(1:STRLEN(STRTMP))
 1000 FORMAT ('Writing FITS table ',A)
      CALL MSGPUT (MESSAGE, 'I')
      TBLTRGT = STRM2(NAME, STRTMP)
C
C Now write header for this table
C
      CALL FTSWRITH (TBLTRGT)
C
C Write data
C
      CALL FTSTBLWR (TBLTRGT)
      IF (ERROR) GO TO 990
      NTBLSWRT = NTBLSWRT + 1
      GO TO 1      
C
 990  CONTINUE
C
C Check to see that we actually wrote at least one table
C
      IF ((.NOT.ERROR).AND.(NTBLSWRT.EQ.0)) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 
     1      'No tables were written')
         GO TO 999
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      END IF
C
 999  CONTINUE
      END
