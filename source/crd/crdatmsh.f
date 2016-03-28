C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdatmsh.f	1.4    9/15/94
C
      SUBROUTINE CRDATMSH (VIS, ATMOS)
C
CD Shift the reference values of ATMOS to be centered at AZ, EL
C  You should only do this ONCE, at the beginning of the "observation"
C
C
C	VIS	CH*(*)	input	Vis directory (with AZ, EL values)
C	ATMOS	CH*(*)	input	Atmosphere directory
C
C
C
C  These three subroutines are used in conjunction with each other and
C  are somewhat confusing until you understand what is going on:
C
C  CRDATMSH will initialize the atmosphere with respect to the visibility
C  set and should be run once. 
C 
C  CRDATMPO will use the times of the Visibility
C  measurements and the Atmospheric coordinates, wind velocity, and pixel
C  values to create an array of Water Vapor column density along each
C  AZ-EL line of site.  This needs to be run only once.
C
C  VISATMOS is passed the Vis data and the Atmosphere with this Water array
C  tacked on and will read the water vapor off, convert it to a phase, and
C  apply it to each visibility, looping through integration times.
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 10 1991
C	Replaced reference to 'Atmos' to ATMOS.  It worked for a surprisingly
C	long time
C				M.A.Holdaway	Oct 23 1991
C	Added subroutine documentation
C				M.A. Holdaway	Sept 15 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, ATMOS
C
      INTEGER		ELADD, AZADD
      REAL		EL, AZ
C
      DOUBLE PRECISION  RVAL(SYSMXDIM)
C
      CHARACTER*(*)     ROUTINE
      PARAMETER		(ROUTINE = 'CRDATMSH')
C
      REAL		D2R, HEIGHT
      INTEGER           NDUMMY, NRVAL
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER		DATADD
C
      DATA		RVAL /SYSMXDIM * 0.0/
C=====================================================================
      IF(ERROR) GO TO 999
C
      D2R = 4. * ATAN2(1.0, 1.0)/ 180.0
      ELADD = DATADD (STRM2 (VIS, 'EL'))
      AZADD = DATADD (STRM2 (VIS, 'AZ'))
      CALL DATGETR (ATMOS, 'HEIGHT', HEIGHT, 1, NDUMMY)
      IF (HEIGHT .LT. 0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Atmosphere''s height is < ZERO!')
         GOTO 990
      ENDIF
      EL = MEMR (ELADD)
      AZ = MEMR (AZADD)
      IF (ERROR) GOTO 990
      CALL DATGETD (ATMOS, 'CRVAL', RVAL, SYSMXDIM, NRVAL)
      IF (NRVAL .LT. 3) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Too few axes on ATMOS')
         GOTO 990
      ENDIF
C
C Get REFX
C
      IF (EL .EQ. 90.0) THEN
         RVAL(1) = 0.0
      ELSE IF (EL .LE. 0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Elevation is <= ZERO!')
         GOTO 990
      ELSE
         RVAL(1) = HEIGHT * SIN (D2R * AZ) / TAN (D2R * EL)
      ENDIF
C
C Get REFY
C
      IF (EL .EQ. 90.0) THEN
         RVAL(2) = 0.0
      ELSE IF (EL .LE. 0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Elevation is <= ZERO!')
         GOTO 990
      ELSE
         RVAL(2) = HEIGHT * COS (D2R * AZ) / TAN (D2R * EL)
      ENDIF
C
C Get REFZ
C
      RVAL(3) = HEIGHT
C
C Put Reference values into ATMOS coordinate header
C
      CALL DATPUTD (ATMOS, 'CRVAL', RVAL, NRVAL)
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
