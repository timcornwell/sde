C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tcorr.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
CD     
CD    Program to sum IPCS data in a variety of ways (see .inf file)
C
CS    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcssum
C                                         R.G. Marson     Aug 29 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TCORR')
C     
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE
      INTEGER           NDUMMY
C     
C==================================================================
C     
      CALL MSGWELCO ('I simulate triple products')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get the Image file
C
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL FILIMGGE('Image', INFILE, ' ')
C
C Triple-correlate it
C
      CALL ARR3CORR('Image', 'Image', 'Triple')
C
C And write out the file
C
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Triple', OUTFILE, ' ')
C
 999  CONTINUE
      END
