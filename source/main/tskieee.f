C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tskieee.f	1.3	 7/20/92
C
      INTEGER FUNCTION TSKIEEE (SIG, CODE, CONTEXT)
C
C IEEE condition handler
C
C
C	SIG	INT	input	Signal (always SIGFPE)
C	CODE	INT	input	Code
C	CONTEXT	INT	input	Context array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		SIG, CODE, CONTEXT(5)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'TSKIEEE')
C
#if COMP_SUN
      INTEGER		LOC
      INTEGER		EXITSTAT
      DATA		EXITSTAT	/1/
#endif
C=======================================================================
C
      TSKIEEE = 0
C
#if COMP_SUN
C
C Pass exceptions inexact and underflow, abort on all others
C
      IF ((LOC(CODE).EQ.196).OR.(LOC(CODE).EQ.204)) THEN
      ELSE
         WRITE (MESSAGE, *) 'IEEE exception code ', LOC(CODE),
     $     ' occured at pc ', CONTEXT(4)
         CALL MSGPUT (MESSAGE, 'E')
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'IEEE exception')
      END IF
#endif
C
      END
