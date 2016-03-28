C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgetn.f	1.1	 7/21/94
C
      SUBROUTINE FILGETN (NAME, FILE)
C
CD Read arbitrary arrays from ASCII file, put them in named arrays off of NAME
C
C       the first two lines of the file should be something like:
C
C
C        3
C        'X'       'Y'        'ANOTHER'
C
C        This will create arrays NAME/X, NAME/Y, NAME/ANOTHER
C        and fill them with REALS from column 1, 2, and 3.
C        lines with # are ignored
C	 NOTE: each name should be 8 characters of less
C
C	An array or characters called 'ARRAYNAMES' will
C	also be attached to NAME
C
C	NAME	CH(*)	inp	Name of Directory
C	FILE	CH(*)	inp	File name
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 27 1994
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, FILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGETN')
C
      INTEGER		NROWS, MAXTHING, NTHINGS
      PARAMETER		(NROWS = 70000)
      PARAMETER		(MAXTHING = SYSMXDIM)

      CHARACTER*(8)		ARRNAMES(MAXTHING)
      REAL			A(NROWS, MAXTHING)
C
      INTEGER		IROW, NROW, ITHING, IADD
      INTEGER		NAX, NAXIS(MAXTHING)
C
      CHARACTER*132	LINE
      LOGICAL		EOF
      INTEGER		NCHAR
C
      INTEGER			STRLEN
      CHARACTER*(SYSMXNAM)	STRM2
      DATA		NAXIS /MAXTHING * 1/
      DATA		ARRNAMES /MAXTHING * ' '/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL TXTOPEN (ROUTINE, FILE, 'READ')
C
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      READ (LINE(:STRLEN(LINE)), *, ERR = 200, END = 100) NTHINGS
C
      IF (NTHINGS .GT. MAXTHING) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $        'Lazy Programmer didnt allow for this many arrays')
         GOTO 999
      ENDIF
C
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      READ (LINE(:STRLEN(LINE)), *, ERR = 200, END = 100)
     $     (ARRNAMES(ITHING), ITHING=1, NTHINGS)

      IROW = 1
 10   CONTINUE
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF (EOF) GO TO 100
         IF (ERROR) GOTO 990
         IF (LINE(1:1).EQ.'#') GO TO 10
         IF (IROW .GE. NROWS) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $           'Lazy Programmer didnt allow for this many elements')
            GOTO 999
         ENDIF

         READ (LINE(:STRLEN(LINE)), *, ERR = 200, END = 100) 
     $        ( A(IROW, ITHING), ITHING=1, NTHINGS )
         IROW = IROW + 1
C
      GOTO 10
 100  CONTINUE
      CALL TXTCLOSE(ROUTINE)
      NROW = IROW - 1
C
C Finished reading data, now fill the SDE arrays
C
C
      NAX = 1
      NAXIS(1) = NROW
      DO 120 ITHING = 1, NTHINGS
         CALL DATMAKAR ( STRM2(NAME, ARRNAMES(ITHING)),
     $        NAX, NAXIS, 'R', IADD)
         DO 110 IROW = 1, NROW
            MEMR(IADD + IROW - 1) = A(IROW, ITHING)
 110     CONTINUE
 120  CONTINUE
C
      CALL DATPUTC (NAME, 'ARRAYNAMES', ARRNAMES, NTHINGS)
C
      GO TO 300
  200 CONTINUE
      CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//LINE)
      GO TO 999
  300 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
