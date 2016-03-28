C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)world.f	1.3    5/21/92
C
      SUBROUTINE SDEMAIN
C
CD Program to say 'Hello, World'
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'WORLD')
C
C==================================================================
      CALL MSGWELCO ('Hello, world')
C
      END
