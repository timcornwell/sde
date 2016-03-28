C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpbrad.f	1.2	 8/12/91
C
      SUBROUTINE PIXPBRAD (PBARR, NPB, RADMAX, BMX, DELTX, PBLEV,
     $   RADIUS)
C
CD Convert PBLEV to RADIUS for the given PB parameters
C
C	PBARR	REAL	input	PB array
C	NPB	INT	input	Length of array for primary beam
C	RADMAX	REAL	input	Maximum radius allowed
C	BMX	REAL	input	Scaling for X
C	DELTX	REAL	input	Size of pixels (divides out with BMX)
C	PBLEV	REAL	input	Primary Beam level
C	RADIUS	REAL	out	Rad, in arcs, corresponding to PBLEV
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 8 1991
C	Made RADIUS = ABS( ) in the fallthrough case
C				M.A.Holdaway	Aug 12 1991
C--------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NPB
      REAL		PBARR(*), RADMAX, BMX, DELTX, PBLEV, RADIUS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPBRAD')
C
      REAL		RSCL
      INTEGER		I
C=========================================================================
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NPB-1) / RADMAX
C
      IF (PBLEV .EQ. 0.0) PBLEV = .005
      IF (PBLEV .GE. 1.0) THEN
         CALL MSGPUT ('Warning: PBLEV >= 1.0','W')
         RADIUS = 0.0
      ELSE IF (PBLEV .LT. 0.0) THEN
         CALL MSGPUT ('Warning: PBLEV < 0.0','W')
         RADIUS = 1E+20
      ELSE
C
         DO 100 I = 1, NPB
            IF ( PBARR(I) .LE. PBLEV) THEN
               RADIUS = ABS (FLOAT(I-1) * DELTX / RSCL / 
     $            SQRT(BMX))
               GOTO 200
            ENDIF
 100     CONTINUE
         RADIUS = ABS (FLOAT(NPB-1) * DELTX / RSCL/BMX )
         CALL MSGPUT ('Warning: RADIUS taken to be RADMAX', 'W')
 200     CONTINUE
      ENDIF
C
C
 999  CONTINUE
      END
