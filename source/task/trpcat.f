C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trpcat.f	1.1    12/19/90
C
      SUBROUTINE SDEMAIN
C
CD Program to concatenate triple product data
C
C Audit trail:
C	New task
C				T.J.Cornwell    Dec 19 1990
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPCAT')
C
      CHARACTER*(SYSMXNAM)	TRPFILE(SYSMXIMG), OUTFILE, STOKES
      INTEGER		NDUMMY, ITRP
C==================================================================
      CALL MSGWELCO ('I concatenate triple product data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Triple', TRPFILE, SYSMXIMG, NDUMMY)
      CALL USRGETC('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
C
C Get the original visibility files
C
      ITRP=1
      CALL TRPGET ('Triple', TRPFILE(1), STOKES, '*', ' ')
 10   CONTINUE
      ITRP=ITRP+1
      IF(TRPFILE(ITRP).EQ.' ') GO TO 20
         CALL TRPGET ('TripleAdd', TRPFILE(ITRP), STOKES, '*', ' ')
         CALL TRPCAT ('Triple', 'TripleAdd', 'Triple', 'OBS', STOKES)
         CALL DATDELET ('TripleAdd')
         GO TO 10
 20   CONTINUE
C
C Put the visibility file
C
      CALL TRPPUT ('Triple', OUTFILE, 'OBS', STOKES, '*', ' ')
C
 999  CONTINUE
      END
