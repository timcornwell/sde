C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grd2d.f	1.4	 7/20/92
C
      SUBROUTINE GRD2D (VIS)

C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRD2D')
      COMPLEX 	VIS(*)
C
C==========================================================================
      IF (ERROR) GO TO 999
C
      CALL MSGPUT('Create Me, PLEASE!', 'W')
      CALL MSGPUT('I dont yet exist', 'W')
      CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $   'Dummy subroutine written so things will run')
      CALL ERRTRACE
C
 999  CONTINUE
      END
