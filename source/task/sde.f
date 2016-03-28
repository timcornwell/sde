C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sde.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Control Environment of SDE
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SDE')
C
C Names of input variables
C
      INTEGER		NDUMMY
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I change the SDE environment file')
C
C Call user interface routine
C
      CALL USRCTL
C
      END
