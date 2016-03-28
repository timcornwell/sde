C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gobble.f	1.1 12/4/95
C
      SUBROUTINE SDEMAIN
C
CD Program to gobble memory
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 16 1995
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GOBBLE')
C
      INTEGER		NDUMMY, I, M
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
C
      CALL MSGWELCO ('I gobble memory')
C
      M=0
      DO 10 I = 1, 10000
         CALL DATMAKAR (STRM2('Memory', STRINT(I)), 1, 256*1024*10,
     $        'R', NDUMMY)
         IF(ERROR) GO TO 999
         M=M+10
         WRITE(MESSAGE, 1000) M
 1000    FORMAT('Gobbled ',I5,' Mbytes')
         CALL MSGPUT (MESSAGE, 'I')
 10   CONTINUE
C
 999  CONTINUE
      END
