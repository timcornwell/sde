C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistapep.f	1.3    10/22/93
C
      SUBROUTINE VISTAPEP (WT, NVIS, U, V, NEWWT, TAPER, MINTAPER,
     $   MAXTAPER)
C
CD Taper visibility data.
C
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	U,V	REAL	input	U,V in wavelengths
C	TAPER	REAL	input	Taper in arcseconds (BMAJ, BMIN, BPA)
C	NEWWT	REAL	output	Output weights
C	MINTAPER REAL	output	Minimum taper applied
C	MAXTAPER REAL	output	Maximum taper applied
C
C Better be careful applying this to rotated data.
C
C If BMAJ or MBIN < 0, then there is an inverse taper applied in that
C axis
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added inverse taper, min/maxtaper, and fixed value of PI
C				D.S.Briggs	Sept 24 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      REAL		WT(NVIS), NEWWT(NVIS), U(NVIS), V(NVIS)
      REAL		TAPER(3), MINTAPER, MAXTAPER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTAPEP')
C
      INTEGER		IVIS
      REAL		TWOPI, STOR, FACT, COSPA, SINPA, T,
     $   		RBMAJ, RBMIN, RU, RV, R
C
      REAL	PI
      PARAMETER	(PI=3.14159265359)
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      MAXTAPER = -1.0
      MINTAPER = 1.E20
      TWOPI = 2 * PI
      STOR = TWOPI / (360.0 * 3600.0)
C
      FACT = 4.0*LOG(2.0)
      COSPA = COS(PI*(90.0-TAPER(3))/180.0)
      SINPA = SIN(PI*(90.0-TAPER(3))/180.0)
      RBMAJ = SIGN(FACT*(STOR*TAPER(1))**2, TAPER(1))
      RBMIN = SIGN(FACT*(STOR*TAPER(2))**2, TAPER(2))
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         RU =   COSPA * U(IVIS) + SINPA * V(IVIS)
         RV = - SINPA * U(IVIS) + COSPA * V(IVIS)
         R = RBMAJ*RU**2 + RBMIN*RV**2
         T = EXP(-R)
         MAXTAPER = MAX(MAXTAPER, T)
         MINTAPER = MIN(MINTAPER, T)
         NEWWT(IVIS) = WT(IVIS) * EXP(-R)
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
