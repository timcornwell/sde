C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fildel.f	1.2    24 Feb 1995
C
      SUBROUTINE FILDEL (NAME)
C
C Remove a file from the filesystem.  It is not considered to be an
C error if the file does not exist.
C
C	NAME	CH*(*)	input	Name of file e.g. 'TEST.DAT'
C
C There simply does not seem to be any machine independent way to do
C this.  After much searching, I simply decided to go with #defines.
C unlink seems to work on most unix platforms, but there is no
C guarantee.  In principle, this routine will have to be checked out on
C every platform on which it is needed.  It's pretty ugly.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C                               D.S.Briggs      Oct 15 1990
C       Added COMP_ALPHA to the F_UNLINK method.
C                               J.D.Ellithopre  Oct 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Methods we know about
C
#define F_UNLINK	1
#define C_UNLINK	2
C
C Methods for specific operating systems
C
#if COMP_CONVEX || COMP_SUN || COMP_ALPHA
#define METHOD  F_UNLINK
#elif COMP_IBM
#define METHOD	C_UNLINK
#endif
C
C Default to fortran callable unlink()
C
#ifndef METHOD
#define METHOD  F_UNLINK
#endif
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILDEL')
C
      CHARACTER*(SYSMXNAM)	TRANSNAM
      LOGICAL		FILEXIST
      INTEGER		ERRCODE, L
#if METHOD == F_UNLINK
      INTEGER           UNLINK
#elif METHOD == C_UNLINK
      INTEGER		UNLINK, STRLEN
#endif
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Translate name
C
      CALL SYSTRANS (NAME, TRANSNAM)
      IF (TRANSNAM.EQ.' ') THEN
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file translation')
          GO TO 999
      END IF
C
C Check for existence
C
      IF(FILEXIST(TRANSNAM)) THEN
#if METHOD == C_UNLINK
         L = STRLEN(TRANSNAM) + 1
         TRANSNAM(L:L) = CHAR(0)
         ERRCODE = UNLINK(%REF(TRANSNAM))
         TRANSNAM(L:L) = ' '
#elif METHOD == F_UNLINK
         ERRCODE = UNLINK(TRANSNAM)
#endif
         IF (ERRCODE.NE.0) THEN
            WRITE (MESSAGE, 1000) ERRCODE
 1000       FORMAT ('Error',I3,' during UNLINK -- check file perms.')
            CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
            GO TO 999
         END IF
C
C Test that the file is really gone.
C
         IF(FILEXIST(TRANSNAM)) THEN           
            CALL ERRREPOR ('Cannot remove file', ROUTINE,
     1           'Check file permissions')
         END IF
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
