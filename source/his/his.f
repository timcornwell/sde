C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)his.f	1.4    10/15/93
C
      SUBROUTINE HISCLOSE (NAME)
C
CD Close history mechanism for directory entry.
C
C
C	NAME	CH*(*)	input	Directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISCLOSE')
C
      CHARACTER*(SYSMXNAM)	STRM2
C======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATDELET (STRM2(NAME,'HII'))
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISCOPY (NAME, ONAME)
C
C Copy history cards
C
C
C	NAME	CH*(*)	input	Name of database entry
C	ONAME	CH*(*)	input	Name of output entry
C Audit trail:
C	New version
C				T.J.Cornwell	Jan 9 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, ONAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISCOPY')
C
      LOGICAL		DATEXIST
      INTEGER		IHISCARD, ICARD, MAXNCARD, NDUMMY
      CHARACTER*80	HISCARD
      CHARACTER*(SYSMXNAM)	STRM2
C====================================================================
      IF (ERROR) GO TO 999
C
C Open new history
C
      CALL HISOPEN(ONAME)
C
C If there are no history cards then stop
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'HII'))) GO TO 999
C
C Read old cards and write new ones
C
      IHISCARD = 0
      CALL DATGETI (STRM2(NAME, 'HII'), 'NCARDS', MAXNCARD, 1, 
     1   NDUMMY)
      IHISCARD = 0
      DO 10 ICARD = 1, MAXNCARD
         IHISCARD = IHISCARD + 1
         CALL HISGET(NAME, HISCARD, IHISCARD)
         IF (IHISCARD.GT.0) THEN
            CALL HISPUT (ONAME, HISCARD)
         ELSE
            CALL ERRREPOR (ERRNTFND, ROUTINE, 
     1         'Found too few history cards')
            GO TO 999
         END IF
 10   CONTINUE
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISSDCPY (NAME, ONAME)
C
C Copy history cards that look like SDE records
C
C	NAME	CH*(*)	input	Name of database entry
C	ONAME	CH*(*)	input	Name of output entry
C
C The selection mechanism is quite crude.  It simply doesn't copy
C anything until it sees a record starting with #, (as do records
C written by HISINPUT).  All subsequent records are copied.
C
C Audit trail:
C	Cloned from HISCOPY
C				D.S.Briggs	Oct 15 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, ONAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISSDCPY')
C
      LOGICAL		DATEXIST, SDEHIS
      INTEGER		IHISCARD, ICARD, MAXNCARD, NDUMMY
      CHARACTER*80	HISCARD
      CHARACTER*(SYSMXNAM)	STRM2
C====================================================================
      IF (ERROR) GO TO 999
C
C Open new history
C
      CALL HISOPEN(ONAME)
C
C If there are no history cards then stop
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'HII'))) GO TO 999
C
C Read old cards and write new ones
C
      IHISCARD = 0
      SDEHIS = .FALSE.
      CALL DATGETI (STRM2(NAME, 'HII'), 'NCARDS', MAXNCARD, 1, 
     1   NDUMMY)
      IHISCARD = 0
      DO 10 ICARD = 1, MAXNCARD
         IHISCARD = IHISCARD + 1
         CALL HISGET(NAME, HISCARD, IHISCARD)
         IF (IHISCARD.GT.0) THEN
            IF (HISCARD(1:1).EQ.'#') SDEHIS = .TRUE.
            IF (SDEHIS) CALL HISPUT (ONAME, HISCARD)
         ELSE
            CALL ERRREPOR (ERRNTFND, ROUTINE, 
     1         'Found too few history cards')
            GO TO 999
         END IF
 10   CONTINUE
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISGET (NAME, CARD, NCARD)
C
C Get a specific history card from the database. Signals end of cards 
C NCARD = -1
C
C
C	NAME	CH*(*)	input	Name of database entry
C	CARD	CH*(*)	output	History card
C	NCARD	INT	i/o	Number of desired card
C Audit trail:
C	Changed to work by selecting a specified numbered card
C				T.J.Cornwell	Jan 10 1989
C	Changed to signal end with IHISCARD = -1
C				T.J. Cornwell	Jan 10 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, CARD
      INTEGER		NCARD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISGET')
C
      INTEGER		HISLEN
      PARAMETER 	(HISLEN = 80)
      INTEGER		NDUMMY, MAXNCARD
      CHARACTER*(SYSMXNAM)	STRINT, REASON, STRTMP, STRM2
      LOGICAL		STRMATCH, DATEXIST
C====================================================================
      CARD = ' '
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'HII'))) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No HII directory')
         GO TO 999
      END IF
C
      CALL DATGETI (STRM2(NAME, 'HII'), 'NCARDS', MAXNCARD, 1, 
     1   NDUMMY)
      IF ((NCARD.LE.0).OR.(NCARD.GT.MAXNCARD)) THEN
         CARD = ' '
         GO TO 999
      END IF
      STRTMP = 'CARD'
      CALL STRAPPEN (STRTMP, STRINT(NCARD))
      CALL DATGETC (STRM2(NAME, 'HII'), STRTMP, CARD, 1, NDUMMY)
C
C Trap end of history error
C
      IF (ERROR) THEN
         CARD = ' '
         NCARD = -1
         CALL ERRREASO(REASON)
         IF (STRMATCH(REASON,ERRNTFND)) THEN
            CALL ERRCANCE
         END IF
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISINPUT (NAME)
C
C Add the inputs to the history section
C
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	First version
C				T.J.Cornwell	Jan 9 1989
C	Added SYSORIG
C				T.J.Cornwell	Feb 26 1989
C	Changed format
C				T.J.Cornwell	July 28 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'HISINPUT')
C
      CHARACTER*(SYSMXNAM)	INPUTFIL
      CHARACTER*(24)	DTIME
      INTEGER		L, STRLEN
      LOGICAL		EOF, FILEXIST
      CHARACTER*80	HISCARD
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      L = STRLEN(SYSPROG)
      INPUTFIL = SYSPROG(:L)//'.cur'
C
      IF(.NOT.FILEXIST(INPUTFIL)) GO TO 999
      CALL HISOPEN(NAME)
      CALL TXTOPEN ('Inputs', INPUTFIL, 'READ')
      CALL SYSDATET (DTIME)
      WRITE (MESSAGE, 1000) SYSPROG(:L), DTIME
 1000 FORMAT ('# ',A,' run at ',A)
      CALL HISPUT (NAME, MESSAGE)
      WRITE (MESSAGE, 2000) SYSVERS
 2000 FORMAT ('# version compiled at ',A)
      CALL HISPUT (NAME, MESSAGE)
      CALL HISPUT (NAME, SYSPROG(:L)//' << EOF')
 1    CONTINUE
      CALL TXTREAD ('Inputs', HISCARD, L, EOF)
      IF (ERROR) GO TO 990
      IF(EOF) GO TO 100
      CALL HISPUT (NAME, HISCARD)
      GO TO 1
 100  CONTINUE
C
      CALL HISPUT (NAME, ' go')
      CALL HISPUT (NAME, 'EOF')
      CALL HISPUT (NAME, '# Run on '//SYSORIG)
C
      CALL TXTCLOSE ('Inputs')
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
      SUBROUTINE HISLIST (NAME)
C
C List the history section
C
C
C	NAME	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	First version
C				T.J.Cornwell	Jan 11 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'HISLIST')
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      INTEGER		ICARD, MAXNCARD, IHISCARD, NDUMMY
      CHARACTER*80	HISCARD
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C If there are no history cards then stop
C
      IF (.NOT.DATEXIST(STRM2(NAME, 'HII'))) GO TO 999
C
C Read cards
C
      WRITE (MESSAGE, 1000) NAME
 1000 FORMAT ('History for ',A)
      CALL MSGPUT (MESSAGE, 'I')
      CALL DATGETI (STRM2(NAME, 'HII'), 'NCARDS', MAXNCARD, 1, 
     1   NDUMMY)
      IHISCARD = 0
      DO 10 ICARD = 1, MAXNCARD
         IHISCARD = IHISCARD + 1
         CALL HISGET(NAME, HISCARD, IHISCARD)
         IF (IHISCARD.GT.0) THEN
            CALL MSGPUT (HISCARD, 'I')
         ELSE
            CALL ERRREPOR (ERRNTFND, ROUTINE, 
     1         'Found too few history cards')
            GO TO 999
         END IF
 10   CONTINUE
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
      SUBROUTINE HISOPEN (NAME)
C
C Initialize history mechanism for directory entry. MUST be called
C before calling HISPUT.
C
C
C	NAME	CH*(*)	input	Directory entry
C Audit trail:
C	Works in absolute numbered system i.e. SCARD has gone
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISOPEN')
C
      CHARACTER*(SYSMXNAM)	STRM2, STRTMP
      LOGICAL		DATEXIST
C======================================================================
      IF (ERROR) GO TO 999
C
      STRTMP = STRM2(NAME, 'HII')
      IF (.NOT.DATEXIST(STRTMP)) THEN
         CALL DATCREAT (STRTMP)
         CALL DATPUTI (STRTMP, 'NCARDS', 0, 1)
      END IF
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISPUT (NAME, HISCARD)
C
C Add a history card to the database adding the program name. This is
C the usual routine to use. Fi you don't want to add the program name
C then use HISAPUT.
C
C	NAME	CH*(*)	input	Name of database entry
C	HISCARD	CH*(*)	input	History card
C Audit trail:
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, HISCARD
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISPUT')
C
      CHARACTER*(SYSMXNAM)	STRINT, STRTMP, STRM2, CARD
      INTEGER		HISNUM, STRLEN, DATFGETI, L
C====================================================================
      IF (ERROR) GO TO 999
C
C Filter out empty cards
C
      IF (HISCARD.EQ.' ') GO TO 999
C
C Now put into the database
C
      HISNUM = DATFGETI (STRM2(NAME, 'HII'), 'NCARDS') + 1
      CALL DATPUTI (STRM2(NAME, 'HII'), 'NCARDS', HISNUM, 1)
      STRTMP = 'CARD'
      CALL STRAPPEN (STRTMP, STRINT(HISNUM))
      L = MIN(STRLEN(HISCARD), 80)
      CARD = SYSPROG(1:STRLEN(SYSPROG))//' '//HISCARD(:L)
      CALL DATPUTC (STRM2(NAME, 'HII'), STRTMP, HISCARD(:L), 1)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C++
C
      SUBROUTINE HISAPUT (NAME, HISCARD)
C
C Add a history card to the database anonymously
C
C
C	NAME	CH*(*)	input	Name of database entry
C	HISCARD	CH*(*)	input	History card
C Audit trail:
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, HISCARD
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HISPUT')
C
      CHARACTER*(SYSMXNAM)	STRINT, STRTMP, STRM2
      INTEGER		HISNUM, STRLEN, DATFGETI, L
C====================================================================
      IF (ERROR) GO TO 999
C
C Filter out empty cards
C
      IF (HISCARD.EQ.' ') GO TO 999
C
C Now put into the database
C
      HISNUM = DATFGETI (STRM2(NAME, 'HII'), 'NCARDS') + 1
      CALL DATPUTI (STRM2(NAME, 'HII'), 'NCARDS', HISNUM, 1)
      STRTMP = 'CARD'
      CALL STRAPPEN (STRTMP, STRINT(HISNUM))
      L = MIN(STRLEN(HISCARD), 80)
      CALL DATPUTC (STRM2(NAME, 'HII'), STRTMP, HISCARD(:L), 1)
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
