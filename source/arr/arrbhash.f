C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrbhash.f	1.1    6/8/94
C
      INTEGER FUNCTION ARRBHASH (A)
C
CD Return a hash value for a binary array
C
C	A	CH*(*)	input	Name of array
C
C The only information used in the calculation of the hash value is
C whether or not the array value is non-zero at a given pixel
C
C Audit trail:
C	Original version:
C				D.S.Briggs	Nov 30 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRBHASH')
C
      CHARACTER*1	ATYPE
      INTEGER		I, NAX, NAXIS(SYSMXDIM), ADD, NT
C
      INTEGER		PIXRBHSH
C=====================================================================
      IF (ERROR) GO TO 999
C
      ARRBHASH = 0
      CALL DATGETAR (A, NAX, NAXIS, ATYPE, ADD)
      NT = 1
      DO 100 I = 1, NAX
         NT = NT * NAXIS(I)
 100  CONTINUE
C
C Call appropriate routine
C
      IF (ATYPE.EQ.'R') THEN
         ARRBHASH = PIXRBHSH (MEMR(ADD), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//ATYPE//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END



