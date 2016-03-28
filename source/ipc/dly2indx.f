C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)dly2indx.f	1.1    2/10/93
C
      INTEGER FUNCTION DLY2INDX (DELAY, NANT, NDELAYS)
C
CD Given an Delay set convert this to an INDEX
C
C In IPCSSIM and IPCSHMM the set of model fringes is
C  indexed using an INDEX. To generate this using the delays this routine
C  is used
C
C	DELAY	INTEGER(NANT)   Input Delay set
C	NANT	INTEGER         Number of holes in the apeture
C	NDELAYS	INTEGER         Number of delays per hole
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Apr 14 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           NANT, NDELAYS, DELAY(NANT)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DLY2INDX')
C
C Local Variables
C
      INTEGER I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Begin gode here
C
      DLY2INDX = DELAY(1) - 1
      DO I = 2, NANT
         DLY2INDX = NDELAYS * DLY2INDX + DELAY(I) - 1
      END DO
C
  999 RETURN
      END
