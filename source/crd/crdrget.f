C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrget.f	1.1    1/31/93
C
      SUBROUTINE CRDRGET 
     $     (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
CD Get coordinate information for a directory entry. An error is
C generated if the NAXIS* information cannot be found. Any other
C information may be missing. Also Strip out axis that have only one
C element in them (much like CRDRNAX). Return the effective number of axis
C in nax
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	NAX	INT	output	Number of real axes
C	TYPE	CH*(*)	output	Types of axes
C	NAXIS	INT(*)	output	Number of pixels for each axis
C	RVAL	DBLE(*)	output	Reference values
C	RPIX	NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)REAL(*)	output	Reference pixels
C	DELT	REAL(*)	output	Coordinate increments
C	ROTA	REAL(*)	output	Rotations of coordinates
C Audit trail:
C	Basically Cloned from CRDGET
C				R.G Marson 17 Apr 1992
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)     NAME
      INTEGER           NAX, NAXIS(*)
      DOUBLE PRECISION  RVAL(*)
      REAL              RPIX(*), DELT(*), ROTA(*)
      CHARACTER*8       TYPE(*)
C
      CHARACTER*(*)     ROUTINE
      PARAMETER		(ROUTINE = 'CRDGET')
C
      INTEGER           I, RNAX
C=====================================================================
      IF(ERROR) GO TO 999
C
C Get all the axis in the standard way
C
      CALL CRDGET(NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Strip out the ones with Naxis = 1
C
      RNAX = 0
      DO I = 1, NAX
         IF (NAXIS(I).GT.1) THEN
            RNAX = RNAX + 1
            TYPE(RNAX) = TYPE(I)
            NAXIS(RNAX) = NAXIS(I)
            RVAL(RNAX) = RVAL(I)
            RPIX(RNAX) = RPIX(I)
            DELT(RNAX) = DELT(I)
            ROTA(RNAX) = ROTA(I)
         END IF
      END DO
      NAX = RNAX
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
