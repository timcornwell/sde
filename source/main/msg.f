C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)msg.f	1.10    24 Feb 1995
C
      SUBROUTINE MSGPUT (MSG, TYPE)
C
CD Write message to user
C
C
C   NAME    CH*(*)   input    Message text to be put
C   TYPE    CH*(*)   input    Type of message e.g. I, E
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	The second character of TYPE specifies the disposition of the
C	message. 'T' => terminal only, 'L' => log only, ' '=> both
C				T.J. Cornwell	Oct 30 1989
C	Removed use of TXT routines
C				T.J. Cornwell	Nov 2 1989
C	Now should work on IBM and CRAY
C				T.J. Cornwell	Feb 20 1991
C       Added APPEND option in open call for COMP_ALPHA.
C                               J.D. Ellithorpe Oct 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	MSG, TYPE
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='MSGPUT')
C
      CHARACTER		LMSG*256
      INTEGER		STRLEN
      INTEGER 		OUTUNIT, N, J
      INTEGER		MSGLUN
      COMMON		/SYIMSG/ MSGLUN
      DATA OUTUNIT 	/ 6 /
C=====================================================================
C
C Check to see if message system is switched on
C
      IF (.NOT.SYSMSG) RETURN
C
C Don't check for errors since we must carry on regardless. Just write
C the message. 
C
      LMSG = SYSPROG(1:STRLEN(SYSPROG))//' '//
     1   TYPE(1:1)//': '//MSG(1:STRLEN(MSG))
      DO 10 J = 1, STRLEN(LMSG), 80
         N = MAX (STRLEN (LMSG(J:J+80-1)), 1)
         IF(STRLEN(TYPE).EQ.1) THEN
            IF ((J+N-1).GE.STRLEN(LMSG)) THEN
               WRITE (OUTUNIT, '(A)') LMSG(J:J+N-1)
            ELSE
               WRITE (OUTUNIT, '(A)') LMSG(J:J+N-1)
            END IF
C
C Log file must exist by the time we get here
C
            IF (SDELOG.NE.' ') THEN
               OPEN (UNIT=MSGLUN, FILE=SDELOG, STATUS='OLD',
#if COMP_SUN || COMP_CONVEX || COMP_HP || COMP_ALPHA
     $            ACCESS='APPEND',
#endif
     $            ERR=100)
               WRITE (MSGLUN, '(A)') LMSG(J:J+N-1)
               CLOSE(UNIT=MSGLUN,ERR=100)
 100           CONTINUE
            END IF
         ELSE IF (TYPE(2:2).EQ.'T') THEN
            IF ((J+N-1).GE.STRLEN(LMSG)) THEN               
               WRITE (OUTUNIT, '(A,$)') LMSG(J:J+N-1)
            ELSE
               WRITE (OUTUNIT, '(A)') LMSG(J:J+N-1)
            END IF
         ELSE IF(TYPE(2:2).EQ.'L') THEN
            IF (SDELOG.NE.' ') THEN
               OPEN (UNIT=MSGLUN, FILE=SDELOG, STATUS='OLD',
#if COMP_SUN || COMP_CONVEX || COMP_HP || COMP_ALPHA
     $            ACCESS='APPEND',
#endif
     $            ERR=200)
               WRITE (MSGLUN, '(A)') LMSG(J:J+N-1)
               CLOSE(UNIT=MSGLUN,ERR=200)
 200           CONTINUE
            END IF
         END IF
 10   CONTINUE
C
      END
C++
C
      SUBROUTINE MSGWELCO (MSG)
C
C Write introductory message to user
C
C
C   NAME       CH*(*)   input    Message text to be put
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	MSG
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='MSGWELCO')
C
C=====================================================================
C
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT (MSG, 'I')
      CALL MSGPUT (' ', 'I')
      CALL MSGPUT (SYSGVER, 'I')
      CALL MSGPUT ('Compiled : '//SYSVERS, 'I')
      CALL MSGPUT (' ', 'I')
C
      END
C++
C
      SUBROUTINE MSIINITI
C
C Initialise error system
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Use SDELOG itself as temporary variable, which has the
C	correct length
C				D.S.Briggs	Sept 3 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='MSIINITI')
C
      LOGICAL		TEXIST
      INTEGER		MSGLUN, SYSGETLU
      COMMON		/SYIMSG/ MSGLUN
C======================================================================
      IF (ERROR) GO TO 999
C
      SDELOG = ' '
      CALL SYSGTENV ('SDELOG', SDELOG)
      IF (SDELOG.NE.' ') THEN
         MSGLUN = SYSGETLU()
         INQUIRE (FILE = SDELOG, EXIST = TEXIST)
         IF(.NOT.TEXIST) THEN
            OPEN (UNIT=MSGLUN, FILE=SDELOG, STATUS='NEW', ERR=100)
            CLOSE(UNIT=MSGLUN,ERR=100)
         END IF
 100     CONTINUE
      END IF
C
 999  CONTINUE
      END
