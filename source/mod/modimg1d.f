C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modimg1d.f	1.1    12/11/92
C
      SUBROUTINE MODIMG1D (NCOMP, FLUX, RA, BMAJ, TYPE, IMAGE, NX,
     $   REFX, DELTX)
C
CD Make an image from a description of a model.
C
C	NCOMP	INT	input	Number of components
C	FLUX	REAL	input	Flux of component in Jy
C	RA	REAL	input	Position of component in Ra offset (asec)
C	DEC	REAL	input	Position of component in Dec offset (asec)
C	BMAJ	REAL	input	Major axis in asec
C	BMIN	REAL	input	Minor axis in asec
C	BPA	REAL	input	Position angle in degrees
C	TYPE	CHAR	input	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	IMAGE	REAL	output	Output image
C	NX, NY	INT	input	Number of pixels on x, y axes
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in X
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed the convention
C				T.J.C?
C	Added 1-D support
C				D.S.Briggs	Oct 21 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NCOMP, NX
      REAL		IMAGE(NX), FLUX(*), RA(*), BMAJ(*)
      REAL		REFX, DELTX
      CHARACTER*(SYSMXNAM)	TYPE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODIMG1D')
C
      INTEGER		ICOMP, IX
      REAL		DX, R, RNORM, FACT
C
      REAL	PI
      PARAMETER	(PI=3.14159265358979323846)
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (DELTX.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in X')
         GO TO 999
      END IF
C
      DO 100 ICOMP = 1, NCOMP
         IF (TYPE(ICOMP)(1:4).EQ.'POIN') THEN
            IX = NINT(REFX + RA(ICOMP)/DELTX)
            IF ((IX.GE.1).AND.(IX.LE.NX)) THEN
               IMAGE(IX) = IMAGE(IX) + FLUX(ICOMP)
            END IF
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'GAUS') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            RNORM = 1.06447 * BMAJ(ICOMP) / DELTX
            RNORM = 1.0 / ABS(RNORM)
            FACT = 4.0*LOG(2.0)
            DO 10 IX = 1, NX
               DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
               R = (DX/BMAJ(ICOMP))**2
               IMAGE(IX) = IMAGE(IX) + FLUX(ICOMP) * 
     1            RNORM * EXP(-FACT * R)
 10         CONTINUE
         ELSE IF ((TYPE(ICOMP)(1:4).EQ.'DISK').OR.
     $            (TYPE(ICOMP)(1:4).EQ.'RECT')) THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            RNORM = BMAJ(ICOMP) / DELTX
            RNORM = 1.0 / ABS(RNORM)
            DO 30 IX = 1, NX
               DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
               IF (ABS(DX).LT.0.5*BMAJ(ICOMP)) THEN
                  IMAGE(IX) = IMAGE(IX) + FLUX(ICOMP) * RNORM
               END IF
 30         CONTINUE
         ELSE
            WRITE (MESSAGE, 1000) TYPE(ICOMP)(1:4), ICOMP
 1000       FORMAT ('Unknown model type ',A4,' for component ',I4)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
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
