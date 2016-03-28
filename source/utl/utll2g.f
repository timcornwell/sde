C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utll2g.f	1.1    2/5/91
C
      SUBROUTINE UTLL2G (LOCAL, LONG, LAT, GEO)
C
CD Convert local coordinate system to geocentric system. IGNORE LONG
C
C Audit trail:
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION	LOCAL(*), GEO(*)
      DOUBLE PRECISION  LONG, LAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLL2G')
C
      DOUBLE PRECISION	COSLAT, SINLAT
C===================================================================
C
      IF (ERROR) GO TO 999
C
      COSLAT = COS(LAT)
      SINLAT = SIN(LAT)
      GEO(1) = -SINLAT * LOCAL(2) + COSLAT * LOCAL(3)
      GEO(2) =  LOCAL(1)
      GEO(3) =  COSLAT * LOCAL(2) + SINLAT * LOCAL(3)
C
 999  CONTINUE
      END




