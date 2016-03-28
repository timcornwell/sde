C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visselpi.f	1.4    7/8/94
C
      SUBROUTINE VISSELPI (WT, NVIS, TIME, U, V, W, TIMERANG,
     1   UVLIMITS, NEWWT, NSEL)
C
CD Select visibility data.
C
C
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	TIME	REAL	input	Time of each visibility
C	U,V,W	REAL	input	U,V,W in wavelengths
C	TIMERANG	INT	input	TIMERANG of allowed data
C	UVLIMITS	REAL	input	Allowed range of u,v radius
C	NEWWT	REAL	output	Output weights
C	NSEL	INT	output	Number selected
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Include w in uv distance
C				T.J.Cornwell	May 18 1991
C	Remove w from uv distance
C				M.A. Holdaway	July 8 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NSEL
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS), NEWWT(NVIS)
      REAL		TIME(NVIS), UVLIMITS(*), TIMERANG(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSELPI')
C
      INTEGER		IVIS
      REAL		RSQ, RSQMIN, RSQMAX, TMIN, TMAX
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NSEL = 0
      IF ((TIMERANG(1).EQ.0.0).AND.(TIMERANG(2).EQ.0.0)) THEN
         TMIN = -1E20
         TMAX = 1E20
      ELSE
         TMIN = TIMERANG(1)
         TMAX = TIMERANG(2)
      END IF
C
      RSQMIN = UVLIMITS(1)**2
      RSQMAX = UVLIMITS(2)**2
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF ((TIME(IVIS).LT.TMIN).OR.(TIME(IVIS).GT.TMAX)) THEN
            WT(IVIS) = SIGN(WT(IVIS), -1.0)
            GO TO 100
         END IF
         RSQ = U(IVIS)**2+V(IVIS)**2
         IF ((RSQ.LT.RSQMIN).OR.(RSQ.GT.RSQMAX)) THEN
            WT(IVIS) = SIGN(WT(IVIS), -1.0)
            GO TO 100
         END IF
         NSEL = NSEL + 1
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
