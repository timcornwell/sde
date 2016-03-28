C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modimg1p.f	1.1    6/2/93
C
      SUBROUTINE MODIMG1P (NCOMP, FLUX, RA, BMAJ, BMIN, BPA,
     1   TYPE, IMAGE, NX, REFX, DELTX)
C
CD Make an 1D projected image from a description of a model.
C  The model is always projected onto the X axis. Use MODROTAT to rotate
C  the model first if a different projection angle is required
C
C	NCOMP	INT	input	Number of components
C	FLUX	REAL	input	Flux of component in Jy
C	RA	REAL	input	Position of component in Ra offset (asec)
C	BMAJ	REAL	input	Major axis in asec
C	BMIN	REAL	input	Minor axis in asec
C	BPA	REAL	input	Position angle in degrees
C	TYPE	CHAR	input	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	IMAGE	REAL	output	Output image
C	NX	INT	input	Number of pixels on x axes
C	REFX	REAL	input	Reference pixel on x axes
C	DELTX	REAL	input	Increment in X
C
C Audit trail:
C	Cloned from modimg2d
C				R. G. Marson 19 Jan 1993
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      INTEGER		NCOMP, NX
      CHARACTER*(SYSMXNAM)	TYPE(NCOMP)
      REAL		FLUX(NCOMP), RA(NCOMP)
      REAL              BMAJ(NCOMP), BMIN(NCOMP), BPA(NCOMP)
      REAL		REFX, DELTX, IMAGE(NX)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODIMG1P')
      REAL	        PI
      PARAMETER	        (PI=3.14159274101257)
C
C Local Variables
C
      INTEGER		ICOMP, I, IX(4)
      REAL		DX, COSPA, SINPA, R
      REAL              X(4), WS, HC, PFLUX, A, B, C, D, DIS, OFFSET
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
C
C Loop for each component
C
      DO 100 ICOMP = 1, NCOMP
C
C looks like we got a point (great! this one is easy)
C
         IF (TYPE(ICOMP)(1:4).EQ.'POIN') THEN
            OFFSET = REFX + RA(ICOMP)/DELTX
            I = NINT(OFFSET)
            IF ((I.GE.1).AND.(I.LE.NX)) THEN
               IMAGE(I) = IMAGE(I) + FLUX(ICOMP)
            END IF
C
C looks like we got a gausian
C
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'GAUS') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
C
C I forgot what all these variables represent (should have commented it 
C          when I wrote it!)
C
            A = 4. * LOG(2.)/(BMAJ(ICOMP)**2)
            B = 4. * LOG(2.)/(BMIN(ICOMP)**2)
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            DIS = A*COSPA**2 + B*SINPA**2
            C = SQRT(PI/DIS)*FLUX(ICOMP)/ (PI/ SQRT(A * B))
            D = -A*B/DIS * DELTX**2
            OFFSET =  REFX + RA(ICOMP)/DELTX
            DO I = 1, NX
               R = FLOAT(I) - OFFSET
               IMAGE(I) = IMAGE(I) + C * EXP(R**2 * D) 
            END DO
C
C looks like we got a disk
C
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'DISK') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
C
C A,B,C are the terms in 1 = A x^2 + B xy + C y^2
C They are then normalised because I assumed BMAJ,BMIN are radii, and by DELTX
C
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
            A = (COSPA/BMAJ(ICOMP))**2 + (SINPA/BMIN(ICOMP))**2
            A = A * 4 * DELTX**2
            C = (SINPA/BMAJ(ICOMP))**2 + (COSPA/BMIN(ICOMP))**2
            C = C * 4 * DELTX**2
            B = COSPA*SINPA*((1/BMIN(ICOMP))**2-(1/BMAJ(ICOMP))**2)
            B = B * 4 * DELTX**2
C
C X(1,2) are the points where Y = 0 = (-Bx +/- SQRT(4C + (B^2-4AC) x^2))/(2C)
C
            X(2) = ABS(1./SQRT(A))
            X(1) = - X(2)
            OFFSET =  REFX + RA(ICOMP)/DELTX
            X(1) = X(1) + OFFSET
            X(2) = X(2) + OFFSET
C
C No need to calculate these values every time in the loop
C
            DIS = (B**2 - 4*A*C)/ (C**2)
            D = 4 / C
            PFLUX = (SQRT(A*C) / PI) * FLUX(ICOMP)
C            PFLUX = 4.*DELTX*DELTX /(PI * BMAJ(ICOMP)*BMIN(ICOMP))
C
C Loop for all non-zero values
C
            DO I = MIN(MAX(INT(X(1))+1,1),NX), MIN(MAX(INT(X(2)),1),NX)
               R = D + DIS * (FLOAT(I)-OFFSET)**2
               IF (R.GE.0.) THEN
                  IMAGE(I) = IMAGE(I) + SQRT(R) * PFLUX 
               END IF
            END DO
C
C Doing Rectangles
C
         ELSE IF (TYPE(ICOMP)(1:4).EQ.'RECT') THEN
            IF (BMAJ(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Major axis zero')
               GO TO 999
            END IF
            IF (BMIN(ICOMP).EQ.0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Minor axis zero')
               GO TO 999
            END IF
            COSPA = COS(PI*BPA(ICOMP)/180.0)
            SINPA = SIN(PI*BPA(ICOMP)/180.0)
C
C Calculate where on the X axis the 4 corners of this rectangle are
C
            WS = BMAJ(ICOMP) * SINPA / 2./ DELTX
            HC = BMIN(ICOMP) * COSPA / 2./DELTX
            OFFSET =  REFX + RA(ICOMP)/DELTX
            X(1) = WS-HC + OFFSET
            X(2) = WS+HC + OFFSET
            X(3) = -WS-HC + OFFSET
            X(4) = -WS+HC + OFFSET
C
C A projected rectangle is always a trapeziod. so just do each 
C linear segment seperatly
C
            CALL SORTR(X, 4, IX)
            PFLUX = FLUX(ICOMP)/ (X(IX(3)) - X(IX(1)))
            DX = 1
            IF (X(IX(1)).NE.X(IX(2))) DX = PFLUX/(X(IX(2)) - X(IX(1)))
            DO I = INT(X(IX(1))) + 1, INT(X(IX(2)))
               IMAGE(I) = IMAGE(I) + DX*(FLOAT(I) - X(IX(1)))
            END DO
            DO I = INT(X(IX(2))) + 1, INT(X(IX(3)))
               IMAGE(I) = IMAGE(I) + PFLUX
            END DO
            IF (X(IX(3)).NE.X(IX(4))) DX = PFLUX/(X(IX(3)) - X(IX(4)))
            DO I = INT(X(IX(3))) + 1, INT(X(IX(4)))
               IMAGE(I) = IMAGE(I) + DX*(FLOAT(I) - X(IX(4)))
            END DO
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
