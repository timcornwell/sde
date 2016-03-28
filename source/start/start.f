C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
      PROGRAM TOPLEVEL
C
C This is the top level routine which normally is never
C seen by programmers. 
C Audit trail:
C	Added retrieval of system parameters from SDEPARMS file
C				T.J.Cornwell	Feb 4 1989
C	Removed retrieval of system parameters from SDEPARMS file
C				T.J.Cornwell	June 2 1989
C      Added SIGQUIT catching for CONVEX
C				T.J.Cornwell	June 5 1989
C	Added proper hostname
C				T.J.Cornwell	June 29 1989
C	Added IEEE exception handler (partly deferred)
C				T.J.Cornwell	Sept 20 1989
C	New SIGCTL for SYSV
C				T.J.Cornwell	Nov 17 1989
C	Added setting of exit value via SYSEXIT
C				T.J.Cornwell	Nov 26 1989
C	Removed useless SUN IEEE handler
C				T.J.Cornwell	August 16 1990
C	Removed SYSV handler
C				T.J.Cornwell	Jan 16 1991
C	Initialize SYSECPUT timer
C				T.J.Cornwell	Jan 18 1991
C	Fixed OS_SYSV
C				T.J.Cornwell	Aug 14 1992
C	Changed to separate interrupt handlers for each interrupt.
C				T.J.Cornwell	Aug 17 1992
C	Added support of parallel processing
C				T.J.Cornwell	Dec 23 1992
C       Added OS_OSF1
C                               J.D.Ellithorpe  Oct 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
#include	"sysver.h"
C
      CHARACTER*24	STARTTIM, ENDTIME
      REAL		XDUMMY, SYSECPUT
C
#if OS_BSD || OS_OSF1
      EXTERNAL		TSKINT2, TSKINT3, TSKINT10
      INTEGER		SIGNAL, OLDHNDLR
#else 
#if OS_SYSV
      EXTERNAL		TSKINT2, TSKINT3, TSKINT10
#endif
#endif
      INTEGER		STRLEN
      CHARACTER*(SYSMXNAM)    HOST
      LOGICAL		ERRORSV
C=====================================================================
C
C Start error handlers if required
C
#if OS_BSD || OS_OSF1
      OLDHNDLR = SIGNAL (2, TSKINT2, -1)
      OLDHNDLR = SIGNAL (3, TSKINT3, -1)
      OLDHNDLR = SIGNAL (11, TSKINT10, -1)
#else
#if OS_SYSV
      CALL SIGNAL (2, TSKINT2)
      CALL SIGNAL (3, TSKINT3)
      CALL SIGNAL (11, TSKINT10)
#endif
#endif
C
C Define some useful defaults
C
      SYSMSG = .FALSE.
      SYSINTRP = .FALSE.
      SYSDEBUG = .FALSE.
C
C This is the string containing the compile time which is set by the
C task sysver which writes the file sysver.h included above
C
      SYSVERS = SYSVRINI
      SYSGVER = SYSGVR
C
C Get program name after stripping directory
C
#if COMP_HP
      N = IGETARG(0, STRBUF, 256)
      CALL SYSTAIL(STRBUF,SYSPROG)
#else
      CALL GETARG (0, STRBUF)
      CALL SYSTAIL(STRBUF, SYSPROG)
#endif
C
C Initialize the world
C
      CALL DAIINIT
      CALL ERIINITI
      CALL FILCINIT
      CALL MSIINITI
      CALL USIINIT
C
C Find host name
C
      CALL SYSHOST (HOST)
      SYSORIG = 'NRAO '//HOST(1:STRLEN(HOST))//' SDE'
C
C Initialize timers
C
      XDUMMY=SYSECPUT(XDUMMY)
      CALL SYSETIME (MESSAGE)
      CALL SYSDATET (STARTTIM)
C
C By default, Parallel processing is on
C
      SYSPAR=.TRUE.
C
C Now switch message system on
C
      SYSMSG = .TRUE.
C
C Call main routine. The main routine is always named SDEMAIN
C
      CALL SDEMAIN
C
C Flush out any remaining errors from the error system
C
      ERRORSV = ERROR
      CALL ERRFLUSH
C
C Find out how long we took
C
      CALL MSGPUT (' ', 'I')      
      CALL SYSDATET (ENDTIME)
      CALL MSGPUT ('Started  : '//STARTTIM, 'I')

      CALL MSGPUT ('Finished : '//ENDTIME, 'I')
      CALL SYSETIME (MESSAGE)
      CALL MSGPUT (MESSAGE, 'I')
      CALL MSGPUT ('Run on '//HOST, 'I')
C
C Clear up the data base system: this is required mainly for cases where
C disk is mapped into memory
C
      CALL DAIEXIT
C
C The absolute, irrevocable end
C
      IF (ERRORSV) THEN
         CALL SYSEXIT(1)
      ELSE
         CALL SYSEXIT(0)
      END IF
C
      END
