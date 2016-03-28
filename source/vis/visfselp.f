C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visfselp.f	1.2    11/7/90
C
      SUBROUTINE VISFSELP (WT, NVIS, TIME, BASL, U, V, W, FSEL,
     $   NEWWT, NSEL)
C
CD Select visibility data using a logical function FSEL. FSEL is 
C declared as:
C
C LOGICAL FUNCTION FSEL (U, V, W, TIME, BASL)
C
C where all arguments are real.
C
C     $   NEWWT, NSEL)
C
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	TIME	REAL	input	Time of each visibility
C	U,V,W	REAL	input	U,V,W in wavelengths
C      FSEL    EXT     input   LOGICAL function name
C	NEWWT	REAL	output	Output weights
C	NSEL	INT	output	Number selected
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NSEL
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS), NEWWT(NVIS)
      REAL		TIME(NVIS), BASL(*)
      EXTERNAL          FSEL
      LOGICAL           FSEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISFSELP')
C
      INTEGER		IVIS
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NSEL = 0
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF(FSEL(U(IVIS), V(IVIS), W(IVIS), TIME(IVIS), BASL(IVIS)))
     $      THEN
            NSEL = NSEL + 1
         ELSE
            WT(IVIS) = - WT(IVIS)
         END IF
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
