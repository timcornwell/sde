C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)testvis.f	1.3    11/9/90
C
      SUBROUTINE SDEMAIN
C
CD Program to test visput and visget (especially in FITS mode)
C
C Audit trail:
C      Cloned from uvsim.f
C                              R.G. Marson    Sep 16 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)      ROUTINE
      PARAMETER		(ROUTINE = 'TESTVIS')
C
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE
      INTEGER		        NDUMMY
C
C==================================================================
      CALL MSGWELCO ('I test visget and visput')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
C
C Read the visibilty file
C  
      CALL VISGET ('Vis', VISFILE, '*', '*', '*')
C
C Save the visibility file 
C
      CALL VISPUT ('Vis', OUTFILE, 'OBS', '*', '*', ' ')
C
      END


