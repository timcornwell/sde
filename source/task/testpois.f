C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)testpois.f	1.1    11/7/90
C
      SUBROUTINE SDEMAIN
CD     
C    Program to check the poission stats routines.
C
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from tcorr
C                                         R.G. Marson     Sep 20 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TESTPOIS')
C  
      CHARACTER*(SYSMXNAM) FILENAME
      CHARACTER*8 CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL MEAN
      REAL RPIX(SYSMXDIM), ROTA(SYSMXDIM), DELT(SYSMXDIM)
      INTEGER SEED, NDUMMY, TRIALS, SIZE, ADD
      INTEGER NAX, NAXIS(SYSMXDIM)
C     
C==================================================================
C     
      CALL MSGWELCO ('I check for poission statistics')
      CALL USRCTL
      CALL USRGETR ('Mean', MEAN, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETI ('Trials', TRIALS, 1, NDUMMY)
      CALL USRGETI ('Hsize', SIZE, 1, NDUMMY)
      CALL USRGETC ('Outfile', FILENAME, 1, NDUMMY)
C
C Clean up the inputs
C
      MEAN = MAX(0.0, MEAN)
      TRIALS = MAX(1,TRIALS)
      IF (SIZE.LE.0) SIZE = NINT(5 * MEAN)
C
C Create the output array
C
      NAX = 1
      NAXIS(1) = SIZE
      CALL DATMAKAR('HIST', NAX, NAXIS, 'R', ADD)
      CTYPE(1) = 'BINS'
      RVAL(1) = 0.0
      RPIX(1) = 1.0
      DELT(1) = 1.0
      ROTA(1) = 0.0
      CALL CRDPUT('HIST', NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Do the work
C
      CALL ARRSETCO('HIST', 0.0, 0.0)
      CALL PIXRHISTP(MEMR(ADD), SIZE, MEAN, TRIALS, SEED)
C
C Fix up the header
C
      CALL HISINPUT('HIST')
C
C And write out the file
C
      CALL FILIMGPU ('HIST', FILENAME, ' ')
C
 999  CONTINUE
      END
