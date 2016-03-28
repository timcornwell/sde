C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlg2l.f	1.1    2/5/91
C
      SUBROUTINE UTLG2L (GEO, LONG, LAT, LOCAL)
C
CD Convert local coordinate system from geocentric system. IGNORE LONG
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
      PARAMETER		(ROUTINE = 'UTLG2L')
C
      DOUBLE PRECISION	COSLAT, SINLAT
C===================================================================
C
      IF (ERROR) GO TO 999
C
      COSLAT = COS(LAT)
      SINLAT = SIN(LAT)
      LOCAL(1) =  GEO(2)
      LOCAL(2) =  -(SINLAT * GEO(1) - COSLAT * GEO(3))
      LOCAL(3) =  COSLAT * GEO(1) + SINLAT * GEO(3)
C
 999  CONTINUE
      END




