C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trplist.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to list triple product data
C
C Audit trail:
C	New task
C				T.J.Cornwell	March 20 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPLIST')
C
      CHARACTER*(SYSMXNAM)	TRPFILE, OUTFILE
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I list triple product data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Triple', TRPFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL TRPGET ('Triple', TRPFILE, 'I', '*', ' ')
C
      CALL TXTOPEN ('Listing', OUTFILE, 'WRITE')
      CALL TRPLIST ('Listing', 'Triple', 'OBS/I')
      CALL TXTCLOSE ('Listing')
C
 999  CONTINUE
      END
