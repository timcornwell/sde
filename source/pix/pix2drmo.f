C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drmo.f	1.1    7/15/93
C
      SUBROUTINE PIX2DRMO (IMG, MASK, N1, N2, RPIX1, RPIX2, DELT1,
     $   DELT2, NCOMP, FLUX, RA, DEC)
C
CD Convert a 2D real image to an SDE model, pixel level
C
C	IMG	REAL(*)	input	Image
C	MASK	REAL(*)	input	Mask
C	N1	INT	input	Size of Image and Mask
C	N2	INT	input	 "   "    "    "   "
C	RPIX1	REAL	input	Reference pixel on first axis
C	RPIX2	REAL	input	    "       "   "  second "
C	DELT1	REAL	input	Delta coord on first axis
C	DELT2	REAL	input	  "     "   "  second "
C	NCOMP	INT	input	Expected number of components
C	FLUX	REAL(*)	output	Model fluxes
C	RA	REAL(*) output	Model RA
C	DEC	REAL(*)	output	Model Dec
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	13 July 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N1, N2, NCOMP
      REAL		IMG(N1,N2), MASK(N1,N2), RPIX1, RPIX2,
     $   		DELT1, DELT2,
     $   		FLUX(NCOMP), RA(NCOMP), DEC(NCOMP)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRMO')
C
      INTEGER		I, J, ICOMP
C=====================================================================
      IF (ERROR) GO TO 999
C
      ICOMP = 0
      DO 110 J = 1, N2
         DO 100 I = 1, N1
            IF ((MASK(I,J).NE.0.0).AND.(IMG(I,J).NE.0.0)) THEN
               ICOMP = ICOMP + 1
               IF (ICOMP.GT.NCOMP) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $               'Too many components!')
                  GO TO 999
               END IF
C
               FLUX(ICOMP) = IMG(I,J)
               RA(ICOMP) = (I - RPIX1) * DELT1
               DEC(ICOMP) = (J - RPIX2) * DELT2
            END IF
 100     CONTINUE
 110  CONTINUE
      IF (ICOMP.LT.NCOMP) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Not enough components !')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

