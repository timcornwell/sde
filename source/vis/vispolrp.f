C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispolrp.f	1.2	 24 Feb 1995
C
      SUBROUTINE VISPOLRP (
     $   IV, IW, QV, QW, UV, UW, 
     $   VTIME, NVIS,
     $   CTIME, CHI, NCHI)
C
CD Subtract V_{p}/e^{i2chi} from V_{I}
C
C	IV	X(*)		inp	Input I visibilities
C	IW	R(*)		inp	Input I weights
C					same for Q, U
C	VTIME	REAL(*)		input	Times, in days
C	NVIS	INT		input	Number of visibilities
C	CTIME	REAL(*)		input	Time at which Chi has a value
C	CHI	REAL(*)		input	Value of Chi
C	NCHI	INT		input	Number of Chi values
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Jul 21 1994
C       Changed J = CMPLX(0,1.0) to J = CMPLX(0.0,1.0) for DEC Alpha
C       F77.
C                               J.D. Ellithorpe Oct 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NCHI
      COMPLEX		IV(*), UV(*), QV(*)
      REAL		IW(*), UW(*), QW(*)
      REAL		CTIME(*), CHI(*)
      REAL		VTIME(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPOLRP')
C
      COMPLEX		PVIS, J
      INTEGER		IVIS
      REAL		PI, THISCHI, D2R
      PARAMETER		(PI=3.14159274101257)
C
      INTEGER		PIXRNEAR
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      D2R = PI/180.0
      J = CMPLX(0.0,1.0)
C
C  Right now: just do a simple NEAREST interpolation
C
      DO 100 IVIS = 1, NVIS
         PVIS = QV(IVIS) + J * UV(IVIS)
         THISCHI = CHI ( PIXRNEAR( CTIME, NCHI, VTIME(IVIS)))
         IV(IVIS) = IV(IVIS) - PVIS / 
     $        CMPLX(COS(2*D2R*THISCHI), SIN(2*D2R*THISCHI) )
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
