C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)txt.f	1.4    2/20/91
C
      SUBROUTINE TXTCLOSE (HANDLE)
C
CD Close a text file for access.
C
C
C	HANDLE	CH*(*)	input	Name of stream
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	HANDLE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TXTCLOSE')
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		IUNIT, NDUMMY
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETI (STRM2('TXTDIR', HANDLE), 'UNIT', IUNIT, 1, 
     1   NDUMMY)
      CLOSE (UNIT = IUNIT)
      CALL SYSFRELU (IUNIT)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C++
C
      SUBROUTINE TXTOPEN (HANDLE, NAME, ACCESS)
C
C Open a text file for access.
C
C
C	HANDLE	CH*(*)	input	Name of stream e.g. 'Stats'
C	NAME	CH*(*)	input	Name of file e.g. 'TEST.DAT'
C	ACCESS	CH*(*)	input	Access mode 'READ', 'WRITE', 'READWRITE'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added explicit access mode
C				T.J.Cornwell	Nov 1 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	HANDLE, NAME, ACCESS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TXTOPEN')
C
      CHARACTER*(SYSMXNAM)	STRM2, TRANSNAM
      INTEGER		IUNIT, SYSGETLU
      LOGICAL		FILEXIST
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (HANDLE.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null Handle')
         GO TO 999
      END IF
C
C Translate name
C
      CALL SYSTRANS (NAME, TRANSNAM)
      IF (TRANSNAM.EQ.' ')THEN
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file translation')
          GO TO 999
      END IF
C
      IUNIT = SYSGETLU()
      IF(ACCESS.EQ.'WRITE') THEN
         IF(FILEXIST(TRANSNAM)) THEN
            OPEN (UNIT = IUNIT, FILE = TRANSNAM, STATUS = 'OLD', 
#if COMP_CRAY
     1         POSITION='APPEND',
#else
#if COMP_IBM
#else
     1         ACCESS='APPEND',
#endif
#endif
     $           ERR = 100)
         ELSE
            OPEN (UNIT = IUNIT, FILE = TRANSNAM, STATUS = 'NEW', 
     1         ERR = 100)
         END IF
      ELSEIF(ACCESS.EQ.'READ') THEN
         IF(.NOT.FILEXIST(TRANSNAM)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'No such file')
            GO TO 999
         ELSE
            OPEN (UNIT = IUNIT, FILE = TRANSNAM, STATUS = 'OLD', 
     1         ERR = 100)
         END IF
      END IF
      GO TO 200
 100  CALL ERRREPOR (ERROPEN, ROUTINE, 'Cannot open file')
      GO TO 999
 200  CONTINUE
C
      CALL DATCREAT ('TXTDIR')
      CALL DATCREAT (STRM2 ('TXTDIR', HANDLE))
      CALL DATPUTI (STRM2 ('TXTDIR', HANDLE), 'UNIT', IUNIT, 1)
      CALL DATPUTC (STRM2 ('TXTDIR', HANDLE), 'FILE', TRANSNAM, 1)
      CALL DATPUTC (STRM2 ('TXTDIR', HANDLE), 'ACCESS', ACCESS, 1)
C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C++
C
      SUBROUTINE TXTREAD (HANDLE, TEXT, LENGTH, EOF)
C
C Read from a text file
C
C
C	HANDLE	CH*(*)	input	Name of stream
C	TEXT	CH*(*)	output	Returned text
C	LENGTH	INT	output	Length of line
C	EOF	LOG	output	True at EOF
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	HANDLE, TEXT
      INTEGER		LENGTH
      LOGICAL		EOF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TXTREAD')
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		IUNIT, NDUMMY, STRLEN
C=====================================================================
      LENGTH = 0
      TEXT = ' '
      EOF = .FALSE.
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETI (STRM2('TXTDIR', HANDLE), 'UNIT', IUNIT, 1, NDUMMY)
      EOF = .FALSE.
      READ (IUNIT, '(A)', ERR = 100, END = 200) TEXT
      GO TO 300
 100  CALL ERRREPOR (ERRINPUT, ROUTINE, 'Reading text file')
      GO TO 999
 200  EOF = .TRUE.
      GO TO 999
 300  LENGTH = STRLEN(TEXT)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C++
C
      SUBROUTINE TXTWRITE (HANDLE, TEXT)
C
C Write to a text file
C
C
C	HANDLE	CH*(*)	input	Name of stream
C	TEXT	CH*(*)	input	Text to be written
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	HANDLE, TEXT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TXTWRITE')
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		IUNIT, NDUMMY, STRLEN, ILENGTH
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      ILENGTH = STRLEN(TEXT)
C
      CALL DATGETI (STRM2('TXTDIR', HANDLE), 'UNIT', IUNIT, 1, NDUMMY)
      IF (ERROR) GO TO 990
      WRITE (IUNIT, '(A)', ERR = 100) TEXT(1:ILENGTH)
      GO TO 999
 100  CALL ERRREPOR (ERROUTPT, ROUTINE, 'Cannot write to text file')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
