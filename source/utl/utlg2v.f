C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)utlg2v.f	1.2	 1
C
      SUBROUTINE UTLG2V (GEO, LONG, LAT, VLB)
C
CD Convert geocentric system to VLB Mk II system
C
C Note that this coordinate system is left handed, as opposed to most of
C the rest of the world.  Hence the VLB(2) must be negated before applying
C the usual coordinate rotations.  We make a crude correction to account
C for the differences in origins of the two systems, so that the VLB
C coords can later be used for lat & long calculations.  (This has no effect
C on relative spacings.)  GEO must be in meters.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	14 Jul 92
C	Two sign errors in the conversion from GEO to VLB.
C	These signs had the effect of negating the calculated
C	longitude relative to the site longitude, which produced
C	errors in the elevation flags for VLB baselines when the
C	array was specified in GEOCENTRIC COORDINATES.
C	* Any small arrays (500 km or less) are virtually unaffected
C	* Any VLB arrays using the MkII format are unaffected
C	* Only VLB arrays using GEO (' ') or LOCAL ('@') are affected
C				M.A. Holdaway	30 Nov 1994
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
      GEO(1) = GEO(1) + RE * COS(LAT)
      GEO(3) = GEO(3) + RE * SIN(LAT)
C
      VLB(1) = +GEO(1) * CLONG  - GEO(2) * SLONG
      VLB(2) = -GEO(1) * SLONG  - GEO(2) * CLONG
      VLB(3) = GEO(3)
C
 999  CONTINUE
      END




