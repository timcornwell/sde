C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlv2g.f	1.1	 1
C
      SUBROUTINE UTLV2G (VLB, LONG, LAT, GEO)
C
CD Convert Mk II VLBI coordinate system to geocentric system.
C
C Note that this coordinate system is left handed, as opposed to most of
C the rest of the world.  Hence the VLB(2) must be negated before applying
C the usual coordinate rotations.  We make a crude attempt to remove the
C center of the array, mostly for symmetry with UTLG2V since it won't
C make any difference in uv calculations.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	14 Jul 92
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      DOUBLE PRECISION	VLB(3), GEO(3)
      DOUBLE PRECISION  LONG, LAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UTLV2G')
C
C This is the mean earth radius, (R_equator^2 * R_polar) ^ 1/3
C
      DOUBLE PRECISION	RE
      PARAMETER		(RE = 6371000.D0)
C
      DOUBLE PRECISION	CLONG, SLONG
C===================================================================
      IF (ERROR) GO TO 999
C
      CLONG = COS(LONG)
      SLONG = SIN(LONG)
C
      GEO(1) =  VLB(1) * CLONG - VLB(2) * SLONG
      GEO(2) = -VLB(1) * SLONG - VLB(2) * CLONG
      GEO(3) =  VLB(3)
C
      GEO(1) = GEO(1) - RE * COS(LAT)
      GEO(3) = GEO(3) - RE * SIN(LAT)
 999  CONTINUE
      END




