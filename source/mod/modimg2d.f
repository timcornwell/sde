C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modimg2d.f	1.8    11/21/94
C
      SUBROUTINE MODIMG2D (NCOMP, FLUX, RA, DEC, BMAJ, BMIN, BPA,
     1   TYPE, IMAGE, NX, NY, REFX, DELTX, REFY, DELTY)
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
C				      'SPHERE', 'SHELL'
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
C				T.J.C ?
C	Added types SPHE (filled optically thin sphere) and SHEL
C	(optically thin spherical shell)
C
C	SPHERE, as is GAUSS, is simply the appropriate function sampled
C	discretely.  SHELL blows up at the edge, so it is radially averaged
C	over a pixel width before sampling.
C				D.S.Briggs	Mar 2 1993
C	Changed constant in GAUS code from 1.1331 to ATAN(1.0)/LOG(2.0)
C				D.S.Briggs	March 1 1994
C	Added type PLORENTZ.  That's a Pseudo-Lorentzian with an exponent
C	of -3/2 instead of -2.  (This is the Hankel transform of an
C	exponential, and resembles the VLA natural beam.)
C				D.S.Briggs	Aug 3 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NCOMP, NX, NY
      REAL		FLUX(*), RA(*), DEC(*), BMAJ(*), BMIN(*)
      REAL		REFX, REFY, DELTX, DELTY, IMAGE(NX, *), BPA(*)
      CHARACTER*(SYSMXNAM)	TYPE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODIMG2D')
C
      INTEGER		ICOMP, IX, IY
      REAL	PI
      PARAMETER	(PI=3.14159265358979323846)
      REAL		DX, DY, COSPA, SINPA, DMAJ, DMIN, R, R2, RNORM,
     1			FACT, DR, RMIN, RMAX, RMAX1
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (DELTX.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in X')
         GO TO 999
      END IF
      IF (DELTY.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in Y')
         GO TO 999
      END IF
C
      DO 100 ICOMP = 1, NCOMP
         IF (TYPE(ICOMP)(1:4).EQ.'POIN') THEN
            IX = NINT(REFX + RA(ICOMP)/DELTX)
            IY = NINT(REFY + DEC(ICOMP)/DELTY)
            IF ((IX.GE.1).AND.(IX.LE.NX).AND.
     1          (IY.GE.1).AND.(IY.LE.NY)) THEN
               IMAGE(IX, IY) = IMAGE(IX, IY) + FLUX(ICOMP)
            END IF
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'GAUS') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            RNORM = (ATAN(1.0) / LOG(2.0)) *
     $         BMAJ(ICOMP) * BMIN(ICOMP) / (DELTX * DELTY)
            RNORM = 1.0 / ABS(RNORM)
            FACT = 4.0*LOG(2.0)
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DO 11 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 10 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  R = (DMAJ/BMAJ(ICOMP))**2 + (DMIN/BMIN(ICOMP))**2
                  IMAGE(IX, IY) = IMAGE(IX, IY) + FLUX(ICOMP) * 
     1               RNORM * EXP(- FACT * R)
  10          CONTINUE
  11       CONTINUE
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'DISK') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            RNORM = ATAN(1.0) * BMAJ(ICOMP) * BMIN(ICOMP) 
     1          / (DELTX * DELTY)
            RNORM = FLUX(ICOMP) / ABS(RNORM)
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DO 21 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 20 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  R = (DMAJ/BMAJ(ICOMP))**2 + (DMIN/BMIN(ICOMP))**2
                  IF (R.LT.0.25) THEN
                     IMAGE(IX, IY) = IMAGE(IX, IY) + RNORM 
                  END IF
  20          CONTINUE
  21       CONTINUE
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'RECT') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            RNORM = BMAJ(ICOMP) * BMIN(ICOMP) / (DELTX * DELTY)
            RNORM = 1.0 / ABS(RNORM)
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DO 31 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 30 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  IF ((ABS(DMAJ).LT.0.5*BMAJ(ICOMP)).AND.
     1                (ABS(DMIN).LT.0.5*BMIN(ICOMP))) THEN
                     IMAGE(IX, IY) = IMAGE(IX, IY) + FLUX(ICOMP) * 
     1                  RNORM
                  END IF
  30          CONTINUE
  31       CONTINUE
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'SPHE') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            RNORM = 4.0 * 1.5 * DELTX * DELTY /
     $           (PI * BMAJ(ICOMP) * BMIN(ICOMP))
            RNORM = FLUX(ICOMP) * ABS(RNORM)
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DO 41 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 40 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  R = (DMAJ/BMAJ(ICOMP))**2 + (DMIN/BMIN(ICOMP))**2
                  IF (R.LT.0.25) THEN
                     IMAGE(IX, IY) = IMAGE(IX, IY) +
     $                    RNORM * SQRT (1.0 - 4.0*R)
                  END IF
  40          CONTINUE
  41       CONTINUE
C
C The shell formula is quite vulnerable to vagaries of the sampling.
C Thus we do the normalization with a direct loop, as opposed to analytically
C like all the others.
C
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'SHEL') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            DR = 2.0 * SQRT( ABS(DELTX*DELTY) /
     $                       (BMAJ(ICOMP)*BMIN(ICOMP)) )
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            RNORM = 0.0
            DO 51 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 50 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  R2 = 4.0 * ((DMAJ/BMAJ(ICOMP))**2 +
     $                        (DMIN/BMIN(ICOMP))**2)
                  R = SQRT(R2)
                  RMIN = R - DR/2.0
                  IF (R-DR/2.0 .LT. 1.0) THEN
                     RMIN = MAX (RMIN, 0.0)
                     RMAX = R + DR/2.0
                     RMAX1 = MIN(RMAX, 1.0)
                     IF (RMAX.GT.RMIN) THEN
                        RNORM = RNORM +
     $                    (SQRT(1.0-RMIN*RMIN)-SQRT(1.0-RMAX1*RMAX1)) /
     $                       (2*PI*PI*(RMAX*RMAX - RMIN*RMIN))
                     END IF
                  END IF
  50          CONTINUE
  51       CONTINUE
           RNORM = FLUX(ICOMP) / RNORM
           DO 61 IY = 1, NY
              DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
              DO 60 IX = 1, NX
                 DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                 DMAJ =   DX * SINPA + DY * COSPA
                 DMIN = - DX * COSPA + DY * SINPA
                 R2 = 4.0 * ((DMAJ/BMAJ(ICOMP))**2 +
     $                (DMIN/BMIN(ICOMP))**2)
                 R = SQRT(R2)
                 RMIN = R - DR/2.0
                 IF (R-DR/2.0 .LT. 1.0) THEN
                    RMIN = MAX (RMIN, 0.0)
                    RMAX = R + DR/2.0
                    RMAX1 = MIN(RMAX, 1.0)
                    IF (RMAX.GT.RMIN) THEN
                       IMAGE(IX, IY) = IMAGE(IX, IY) + RNORM *
     $                   (SQRT(1.0-RMIN*RMIN)-SQRT(1.0-RMAX1*RMAX1))/
     $                   (2*PI*PI*(RMAX*RMAX - RMIN*RMIN))
                    END IF
                 END IF
 60           CONTINUE
 61        CONTINUE
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'PLOR') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            FACT = 4.**(1/3.) - 1
            RNORM = ABS(DELTX * DELTY) * FACT /
     $         (ATAN(1.0) * 2.0 * BMAJ(ICOMP) * BMIN(ICOMP))
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DO 71 IY = 1, NY
               DY = (FLOAT(IY) - REFY) * DELTY - DEC(ICOMP)
               DO 70 IX = 1, NX
                  DX = (FLOAT(IX) - REFX) * DELTX - RA(ICOMP)
                  DMAJ =   DX * SINPA + DY * COSPA
                  DMIN = - DX * COSPA + DY * SINPA
                  R = ((DMAJ/BMAJ(ICOMP))**2 +
     $               (DMIN/BMIN(ICOMP))**2) * 4.0
                  IMAGE(IX, IY) = IMAGE(IX, IY) + FLUX(ICOMP) * 
     1               RNORM * (FACT*R + 1)**(-1.5)
  70          CONTINUE
  71       CONTINUE
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
