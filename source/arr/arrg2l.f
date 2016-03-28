C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrg2l.f	1.1    3/26/93
C
      SUBROUTINE ARRG2L (ANTFILE)
C
CD Converts the global (LX,LY,LZ) ant coordinates into local (RX, RY, RZ)
C
C	ANTFILE		CH*(*)	inp	Antenna File Directory
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 15 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ANTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRG2L')
C
      DOUBLE PRECISION	LAT, LONG
      INTEGER		IANT, NANT, NAX, NAXIS(SYSMXDIM), NDUMMY
      INTEGER		RXADD, RYADD, RZADD
      INTEGER		LXADD, LYADD, LZADD
      CHARACTER*1	T
      DOUBLE PRECISION	LOCAL(3), EARTH(3)
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETD (ANTFILE, 'SITELAT', LAT, 1, NDUMMY)
      CALL DATGETD (ANTFILE, 'SITELONG', LONG, 1, NDUMMY)
      CALL DATGETAR (STRM2(ANTFILE, 'RX'), NAX, NAXIS, T, RXADD)
      CALL DATGETAR (STRM2(ANTFILE, 'RY'), NAX, NAXIS, T, RYADD)
      CALL DATGETAR (STRM2(ANTFILE, 'RZ'), NAX, NAXIS, T, RZADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LX'), NAX, NAXIS, T, LXADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LY'), NAX, NAXIS, T, LYADD)
      CALL DATGETAR (STRM2(ANTFILE, 'LZ'), NAX, NAXIS, T, LZADD)
      NANT = NAXIS(1)
      DO 100 IANT = 0, NANT-1
         EARTH(1) = MEMD(LXADD+IANT) 
         EARTH(2) = MEMD(LYADD+IANT) 
         EARTH(3) = MEMD(LZADD+IANT) 
         CALL UTLL2G(EARTH, LONG, LAT, LOCAL)
         MEMR(RXADD+IANT) = LOCAL(1) 
         MEMR(RYADD+IANT) = LOCAL(2) 
         MEMR(RZADD+IANT) = LOCAL(3) 
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
