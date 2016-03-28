C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdeman.f	1.1    3/26/93
C
      SUBROUTINE PIXDEMAN (DEMADD, NX, NY, REFX, REFY, DX, DY,
     $   SITELONG, SITELAT, RX, RY, RZ, LX, LY, LZ, DIAM, NANT)
C
CD Initializes the antenna locations
C
C	DEMADD	INT	inp	Address of DEM Image
C	NX	INT	inp	X Size of Mask
C	NY	INT	inp	Y Size of Mask
C	REFX	R	inp	X Ref pixel
C	REFY	R	inp	Y Ref pixel
C	DX	R	inp	Cell size X
C	DY	R	inp	Cell size Y
C	SITELONG D	inp	Site Longitude
C	SITELAT	 D	inp	Site Lattitude
C	RX	R(*)	inp	Local coordinate of antennas
C	RY	R(*)	inp	
C	RZ	R(*)	inp	
C	LX	D(*)	inp	Earth coordinate of antennas
C	LY	D(*)	inp	
C	LZ	D(*)	inp	
C	DIAM	D(*)	inp	Diameter of antennas
C	NANT	I	inp	Number of antennas
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 15 1993
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY, NANT
      INTEGER		DEMADD
      DOUBLE PRECISION	SITELONG, SITELAT
      REAL	REFX, REFY, DX, DY, RX(*), RY(*), RZ(*)
      DOUBLE PRECISION	LX(*), LY(*), LZ(*), DIAM(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDEMAN')
C
      INTEGER		IANT
      REAL		VALUE1, VALUE2
      DOUBLE PRECISION	EARTH(3), LOCAL(3)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 IANT = 1, NANT
         IF (DEMADD .GT. 0) THEN
            CALL PIXBLINT (MEMR(DEMADD), NX, NY, REFX, REFY, 
     $         ABS(DX), DY, RX(IANT), RY(IANT), VALUE1, VALUE2)
            RZ(IANT) = VALUE1
         ELSE
            RZ(IANT) = 0.0
         ENDIF
C
         LOCAL(1) = RX(IANT)
         LOCAL(2) = RY(IANT)
         LOCAL(3) = RZ(IANT)
         CALL UTLL2G(LOCAL, SITELONG, SITELAT, EARTH)
         LX(IANT) = EARTH(1)
         LY(IANT) = EARTH(2)
         LZ(IANT) = EARTH(3)
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
