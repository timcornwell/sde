C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modvis2d.f	1.2    11/21/94
C
      SUBROUTINE MODVIS2D (NCOMP, FLUX, RA, DEC, BMAJ, BMIN, BPA,
     $   TYPE, DOTIMEV, TIMEREF, VEL, VPA, RADELT, VIS, WT, U, V, W,
     $   TIME, NVIS, SHIFT)
C
CD Fill a visibility database with the analytic transform of a model
C
C	NCOMP	INT	input	Number of components
C	FLUX	REAL	input	Flux of component in Jy
C	RA	REAL	input	Position of component in Ra offset (asec)
C	DEC	REAL	input	Position of component in Dec offset (asec)
C	BMAJ	REAL	input	Major axis in asec
C	BMIN	REAL	input	Minor axis in asec
C	BPA	REAL	input	Position angle in degrees
C	TYPE	CHAR	input	Type: 'POINT', 'GAUSS'
C	DOTIMEV LOG	input	Do time variable calcs?
C	TIMEREF	REAL	input	Reference time in days.
C	VEL	REAL	input	Velocity magnitude of component in mas/day
C	VPA	REAL	input	Velocity position angle of comp in degrees
C	RADELT	REAL	input	RA axis increment
C	VIS	CMPLX(*)	Output	Un-gridded data
C	WT	REAL(*)		Output	Weights
C	U	REAL(*)		input	Coordinates of data
C	V	REAL(*)		input	Coordinates of data
C	W	REAL(*)		input	Coordinates of data
C	TIME	REAL(*)		input	Time in days
C	NVIS	INT		input	Number to be gridded
C	SHIFT	REAL		input	Shift matrix 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 29 1994
C	Added time variable models
C				D.S.Briggs	Nov 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NCOMP, NVIS
      LOGICAL		DOTIMEV
      REAL		FLUX(NCOMP), RA(NCOMP), DEC(NCOMP), BMAJ(NCOMP),
     $   		BMIN(NCOMP), BPA(NCOMP), VEL(NCOMP), VPA(NCOMP),
     $   		WT(NVIS), U(NVIS), V(NVIS), W(NVIS), TIME(NCOMP),
     $			TIMEREF, RADELT
      DOUBLE PRECISION	SHIFT(3,3)
      CHARACTER*(SYSMXNAM)	TYPE(*)
      COMPLEX		VIS(NVIS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODVIS2D')
C
      INTEGER		ICOMP, IVIS
      REAL		TWOPI, D2R, AS2R, PHASE, V1, RA1, DEC1,
     $   		A, B, C, A0, C0, THETA, G,
     $   		ULOCAL, VLOCAL, WLOCAL
      COMPLEX		ROT
C=====================================================================
      IF (ERROR) GO TO 999
C
      TWOPI = ATAN(1.0) * 8.0
      D2R = ATAN(1.0) * 4.0 / 180.0
      AS2R = D2R / 3600.0
      DO 500 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 500
         VIS(IVIS) = 0.0
         ULOCAL = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     1      SHIFT(3,1) * W(IVIS)
         VLOCAL = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     1      SHIFT(3,2) * W(IVIS)
         WLOCAL = SHIFT(1,3) * U(IVIS) + SHIFT(2,3) * V(IVIS) +
     1      SHIFT(3,3) * W(IVIS)
         PHASE = TWOPI * (WLOCAL - W(IVIS))
         ROT = CMPLX(COS(PHASE), SIN(PHASE))
C
         DO 100 ICOMP = 1, NCOMP
            IF (DOTIMEV) THEN
               V1 = (TIME(IVIS)-TIMEREF)*VEL(ICOMP)/1000.0
               RA1 = RA(ICOMP) - V1 * COS((VPA(ICOMP)+90.0)*D2R)
               DEC1 = DEC(ICOMP) + V1 * SIN((VPA(ICOMP)+90.0)*D2R)
               PHASE = TWOPI * (RA1*ULOCAL + DEC1*VLOCAL) * AS2R
            ELSE
               PHASE = TWOPI *
     $            (RA(ICOMP)*ULOCAL + DEC(ICOMP)*VLOCAL) * AS2R
            END IF
            IF (TYPE(ICOMP)(1:4).EQ.'POIN') THEN
               VIS(IVIS) = VIS(IVIS) +
     $            FLUX(ICOMP) * CMPLX(COS(PHASE),SIN(PHASE))
            ELSE IF (TYPE(ICOMP)(1:4).EQ.'GAUS') THEN
               IF (BMAJ(ICOMP).EQ.0.0) THEN
                  CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
                  GO TO 999
               END IF
               IF (BMIN(ICOMP).EQ.0.0) THEN
                  CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
                  GO TO 999
               END IF
               A0 = 1/(BMAJ(ICOMP)*AS2R/2)**2
               C0 = 1/(BMIN(ICOMP)*AS2R/2)**2
               THETA = (SIGN(BPA(ICOMP),RADELT)+90.0)*D2R
               A = (A0*COS(THETA)**2 + C0*SIN(THETA)**2) * LOG(2.0)
               B = ((A0 - C0)*SIN(2.0*THETA)) * LOG(2.0)
               C = (A0*SIN(THETA)**2 + C0*COS(THETA)**2) * LOG(2.0)
               G = EXP(TWOPI**2/(4.*A*C - B**2)*(-C*ULOCAL**2
     $            + B*ULOCAL*VLOCAL - A*VLOCAL**2))
               VIS(IVIS) = VIS(IVIS) +
     $            FLUX(ICOMP) * G * CMPLX(COS(PHASE),SIN(PHASE))
            ELSE
               CALL ERRREPOR (ERRFATAL, ROUTINE,
     $            'So sorry.  I can only deal with POINT or GAUS')
               GO TO 999
            END IF
 100     CONTINUE
C
C Now finally phase rotate
C
         VIS(IVIS) = VIS(IVIS) * ROT
C
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
