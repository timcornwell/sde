C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsopen.f	1.3    11/7/90
C
      SUBROUTINE FTSOPEN (NAME)
C
CD Open FITS file on disk and read in header
C
C
C 	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSOPEN')
C
      CHARACTER*(SYSMXNAM)	FILENAME, ACCESS
      LOGICAL		ISEOF
      INTEGER		STRLEN, NDUMMY
C====================================================================
C
      IF (ERROR) GO TO 999
C
C Get file name
C
      CALL DATGETC (NAME, 'FILNAME', FILENAME, 1, NDUMMY)
      IF (ERROR) THEN
         MESSAGE = 'Must have filename for OPEN'
         CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Get file access mode
C
      CALL DATGETC (NAME, 'FILACCESS', ACCESS, 1, NDUMMY)
      IF (ERROR) THEN
         MESSAGE = 'Must have file access mode for OPEN'
         CALL ERRREPOR (ERRLOGIC, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Now open file for access
C
      CALL FILSYSOP (NAME)
      IF (ERROR) GO TO 990
C
C Write message to user
C
      WRITE (MESSAGE, 1000) FILENAME(1:STRLEN(FILENAME)), 
     1   ACCESS(1:STRLEN(ACCESS)), NAME 
 1000 FORMAT ('Opening FITS file ',A,' for ',A,' as ',A)
      CALL MSGPUT (MESSAGE, 'I')
C
C Read the header in
C
      IF (ACCESS.EQ.'READ') THEN
         CALL FTSREADH (NAME, ISEOF)
         IF (ISEOF) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'No FITS cards')
            GO TO 999
         END IF
      END IF
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
