C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sys.f	1.8    6/7/93
C
      SUBROUTINE SYSDATEC (DTSTRING)
C
CD Get date in string in format '23/11/87'
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DTSTRING
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSDATEC')
C
      INTEGER		MONTH, DAY, YEAR
C=====================================================================
C
      CALL SYSDATECC(YEAR, MONTH, DAY) 
      WRITE (DTSTRING, 100) DAY, MONTH, YEAR
 100  FORMAT (I2.2, '/', I2.2, '/', I2.2)
      END
C++
C
      SUBROUTINE SYSDATET (DTSTRING)
C
C Get date and TIME in string
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DTSTRING
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSDATET')
C
      CHARACTER*(SYSMXNAM)	STRTMP
      INTEGER		N
C=====================================================================
C
      CALL SYSDATETC (STRTMP, N)
      DTSTRING=STRTMP(1:N)
C
      END
C++
C
      SUBROUTINE SYSETIME (ESTRING)
C
C Get elapsed time in string
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ESTRING
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSETIME')
C
      INTEGER		N
      CHARACTER*(SYSMXNAM)	STRTMP
C=====================================================================
      CALL SYSUTIMEC (STRTMP, N)
      ESTRING = STRTMP(1:N)
C
 999  CONTINUE
      END
C++
C
      INTEGER FUNCTION SYSSYSTM (COMMAND)
C
C Execute system command: return status
C
C
C	COMMAND	CH*(*)	input	Command to be executed
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	COMMAND
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSSYSTM')
C
      INTEGER		SYSTEMC, STRLEN
C=====================================================================
      IF (ERROR) GO TO 999
C
      SYSSYSTM = SYSTEMC (COMMAND, STRLEN(COMMAND))
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSTAIL (NAME1, NAME2)
C
C Strip off any preceding directory
C
C
C	NAME1	CH*(*)	input	Input name
C	NAME2	CH*(*)	output	Output name
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSTAIL')
C
      CHARACTER*1	DELIMIT
      PARAMETER 	(DELIMIT='/')
C
      INTEGER		POSDEL, STRLEN
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF(INDEX(NAME1,DELIMIT).EQ.0) THEN
         NAME2=NAME1
         GO TO 999
      END IF
      DO 10 POSDEL = STRLEN(NAME1)-1, 1, -1
         IF (NAME1(POSDEL:POSDEL).EQ.DELIMIT) THEN
            GO TO 20
         ENDIF
  10  CONTINUE
      POSDEL = 0
  20  CONTINUE
      POSDEL = POSDEL + 1
      IF(POSDEL.GT.STRLEN(NAME1)) THEN
         NAME2=' '
      ELSE
         NAME2 = NAME1(POSDEL:STRLEN(NAME1))
      ENDIF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSTRANS (NAME1, NAME2)
C
C Translate 1system name to something usable e.g. translate
C environment variables for UNIX. Will translate as much as
C possible of the name. Will return the input name if there is
C not translation.
C
C
C	NAME1	CH*(*)	input	Input name
C	NAME2	CH*(*)	output	Output name
C Audit trail:
C	If output name is blank then set it to input name
C				T.J.Cornwell	Feb 3 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSTRANS')
C
      INTEGER		POSNS, POSSL, STRLEN
C=====================================================================
      NAME2 = NAME1
      IF (ERROR) GO TO 999
C
C Translate environment variables if present
C
      DO 10 POSNS = 1, LEN(NAME1)
         IF (NAME1(POSNS:POSNS).NE.' ') GO TO 20
  10  CONTINUE
      GO TO 999
  20  CONTINUE
C
      CALL SYSGTENV (NAME1(POSNS:), NAME2)
      IF (NAME2.EQ.' ') THEN
         POSSL = INDEX (NAME1, '/')
         IF (POSSL.NE.0) THEN
            CALL SYSGTENV (NAME1(POSNS:POSSL-1), NAME2)
            IF (NAME2.NE.' ') THEN
               NAME2 = NAME2(1:STRLEN(NAME2))//
     1            NAME1(POSSL:)
            END IF
         ELSE 
            NAME2 = NAME1(POSNS:)
         END IF
      END IF

      IF (NAME2.EQ.' ') THEN
         NAME2 = NAME1
      END IF

      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSGTENV (NAME1, NAME2)
C
C Translate system name to something usable e.g. translate
C environment variables for UNIX using GETENV. Returns blank
C if the name is not known.
C
C
C	NAME1	CH*(*)	input	Input name
C	NAME2	CH*(*)	output	Output name
C Audit trail:
C	New version
C				T.J.Cornwell	March 9 1989
C	Maximum size of environment variable expanded, and checking
C	implemented.  Interface to SYSGTENC changed.  Bug in error
C	return fixed.
C				D.S.Briggs	Feb 21 1993
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME1, NAME2
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSGTENV')
C
      INTEGER		N, STRLEN
      CHARACTER*450	STRTMP
C=====================================================================
      IF (ERROR) GO TO 999
C
      STRTMP = ' '
      N = LEN(STRTMP)
      CALL SYSGTENC (NAME1, STRLEN(NAME1), STRTMP, N)
C
      IF (LEN(NAME2).LT.N) THEN
         STRTMP = NAME1
         CALL ERRREPOR (ERRTRUNC, ROUTINE, 'Environment var '//
     $        STRTMP(1:STRLEN(STRTMP))//' too long for passed buffer')
         GO TO 999
      END IF
      IF (N.LT.0) THEN
         CALL ERRREPOR (ERRTRUNC, ROUTINE, 'Internal buffer too small')
         GO TO 999
      END IF
C
      IF (N.LE.0) THEN
         NAME2 = ' '
      ELSE
         NAME2 = STRTMP(1:N)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSHOST (NAME)
C
C Find host name
C
C
C	NAME	CH*(*)	Output	Output host name
C Audit trail:
C	New version
C				T.J.Cornwell	June 29 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSHOST')
C
      INTEGER			N
      CHARACTER*(SYSMXNAM)	STRTMP
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL SYSHOSTC (STRTMP, N)
      NAME = STRTMP(1:N)
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSCWD (NAME)
C
C Find name of current working directory
C
C
C	NAME	CH*(*)	Output	Output host name
C Audit trail:
C	New version
C				T.J.Cornwell	Nov 6 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSCWD')
C
      INTEGER			N
      CHARACTER*(SYSMXNAM)	STRTMP
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL SYSCWDC (STRTMP, N)
      NAME = STRTMP(1:N)
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSACWD (NAME)
C
C Find name of current working directory in absolute form i.e. with
C name of local machine prepended. This is the AOC style
C
C
C	NAME	CH*(*)	Output	Output host name
C Audit trail:
C	New version
C				T.J.Cornwell	Nov 6 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSACWD')
C
      INTEGER			N
      CHARACTER*(SYSMXNAM)	STRTMP, HOST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL SYSCWDC (STRTMP, N)
      NAME = STRTMP(1:N)
      CALL SYSHOST (HOST, N)
      IF(INDEX(STRTMP, HOST).EQ.0) THEN
         HOST = '/'//HOST
         CALL STRAPPEN (HOST, STRTMP)
         STRTMP = HOST
      END IF
C
 999  CONTINUE
      END
      INTEGER FUNCTION SYSGETLU (DUMMY)
C
C Get a valid logical unit number
C
C
C	DUMMY	INT	input	Dummy argument
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		DUMMY
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSGETLU')
C
      LOGICAL		LUNS(90), FIRST
      COMMON		/SYSLUNS/ LUNS
      INTEGER		I
      DATA		FIRST	/.TRUE./
      SAVE		FIRST
C====================================================================
      SYSGETLU = 0
C
      IF(FIRST) THEN
         DO 5 I = 1, 90
            LUNS(I) = .TRUE.
 5       CONTINUE
         FIRST = .FALSE.
      END IF
C
      DO 10 I = 10, 99
         IF(LUNS(I-9)) THEN
            LUNS(I-9) = .FALSE.
            SYSGETLU = I
            GO TO 999
         END IF
 10   CONTINUE
C
      CALL ERRREPOR (ERRBDARG, ROUTINE, 'No free logical units')
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE SYSFRELU (LUN)
C
C Free a valid logical unit number
C
C
C	LUN	INT	input	Logical unit number
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		LUN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSFRELU')
C
      LOGICAL		LUNS(90)
      COMMON		/SYSLUNS/ LUNS
C====================================================================
C
C
      IF((LUN.GE.10).AND.(LUN.LE.99)) THEN
         LUNS(LUN-9) = .TRUE.
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Invalid logical units')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
C++
C
      REAL FUNCTION SYSECPUT (X)
C
C Find elapsed CPU time
C
C	X	REAL	input	Dummy
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL	X
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSECPUT')
C
      REAL	FIRST, NOW
      DATA	FIRST	/0.0/
      SAVE	FIRST
C=====================================================================
C
      CALL SYSECPUTC(NOW)
      IF(FIRST.EQ.0.0) FIRST = NOW
      SYSECPUT = NOW-FIRST
C
      END
