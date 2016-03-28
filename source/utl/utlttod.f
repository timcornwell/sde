C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlttod.f	1.2	 7/20/92
C
      SUBROUTINE UTLTTOD (TIME, DTIME)
C
C Convert time (d,h,m,s) to days.
C
C
C	TIME	INT(4)	input	Time d,h,m,s
C	DTIME	REAL	output	Time in days
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		TIME(4)
      REAL		DTIME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'UTLTTOD')
C
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DTIME =  FLOAT(TIME(1)) + FLOAT(TIME(2))/24.
     1   + FLOAT(TIME(3))/1440. + FLOAT(TIME(4))/86400.
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
