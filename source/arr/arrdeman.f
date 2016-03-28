C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrdeman.f	1.1    3/26/93
C
      SUBROUTINE ARRDEMAN (IMAGE, ANTFILE)
C
CD Get the correct elevations for the antennas from the Digital Elevation Mod
C
C	IMAGE	CH*(*)	inp	DEM image
C	ANTFILE	CH*(*)	inp	Antenna File
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdawau	March 15 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, ANTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRDEMAN')
C
      INTEGER		NANT, NDUMMY
      CHARACTER*1	T
      DOUBLE PRECISION	LAT, LONG
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		MNAX, MNAXIS(SYSMXDIM), DEMADD
      INTEGER		RXADD, RYADD, RZADD, LXADD, LYADD, LZADD, DADD
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL		MDELTX, MDELTY
      CHARACTER*8       TYPE(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER			DATADD
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialize antenna positions to agree with MASK
C

      CALL DATGETD (ANTFILE, 'SITELAT', LAT, 1, NDUMMY)
      CALL DATGETD (ANTFILE, 'SITELONG', LONG, 1, NDUMMY)
      CALL DATGETAR (STRM2(ANTFILE, 'RX'), NAX, NAXIS, T, RXADD)
      CALL DATGETAR (STRM2(ANTFILE, 'RY'), NAX, NAXIS, T, RYADD)
      CALL DATGETAR (STRM2(ANTFILE, 'RZ'), NAX, NAXIS, T, RZADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LX'), NAX, NAXIS, T, LXADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LY'), NAX, NAXIS, T, LYADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LZ'), NAX, NAXIS, T, LZADD)
      CALL DATGETAR (STRM2(ANTFILE, 'DIAM'), NAX, NAXIS, T, DADD)
      NANT = NAXIS(1)
      DEMADD = DATADD (IMAGE)
      CALL CRDGET   (IMAGE,  MNAX, TYPE, MNAXIS, RVAL, 
     $   RPIX, DELT, ROTA)
C         MDELTX = ABS(DELT(1)) * 4.0E+7/360.0
C         MDELTY = ABS(DELT(2)) * 4.0E+7/360.0
      MDELTX = ABS(DELT(1))
      MDELTY = ABS(DELT(2))
      CALL PIXDEMAN (DEMADD, MNAXIS(1), MNAXIS(2), RPIX(1),
     $   RPIX(2), MDELTX, MDELTY, LONG, LAT, 
     $   MEMR(RXADD), MEMR(RYADD), MEMR(RZADD), 
     $   MEMD(LXADD), MEMD(LYADD), MEMD(LZADD),
     $   MEMD(DADD), NANT)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
