C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dxmr.f	1.2	 1/2/92
C
      SUBROUTINE PIX2DXMR (AR1, AR2, NX, NY, CENX, CENY, 
     $   DX, DY, R1, R2, AVETYPE, AR3)
C
CD Radial merging of AR1, AR2: complex 2-D images
C
C	AR1	X	input	array 1 (outer u,v plane)
C	AR2	X	input	array 2 (inner u,v plane)
C	NX	INT	input	size of arrays
C	NY	INT	input	size of arrays
C	CENX	REAL	input	center of images
C	CENY	REAL	input	center of images
C	DX	REAL	input	increment of images
C	DY	REAL	input	increment of images
C	R1	REAL	input	begin AR1 at this radius
C	R2	REAL	input	end AR2 at this radius
C	AVETYPE	REAL	input	ave type: AVE or LIN or GAU
C	AR3	X	output	array 3
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 10 1991
C	Added AVETYPE = 'GAU' for Gaussian weighted average
C				M.A.Holdaway	Dec 27 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY
      COMPLEX		AR1(NX, *), AR2(NX, *), AR3(NX, *)
      REAL		CENX, CENY, DX, DY, R1, R2
      CHARACTER*(*)	AVETYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DXMR')
C
      INTEGER		IX, IY
      REAL		R1SQ, R2SQ, RYSQ, RSQ, SLOPE, WT, GSCALE, R
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      R1SQ = R1 * R1
      R2SQ = R2 * R2
      SLOPE = 0.0
      GSCALE = 0
      IF (R2 .NE. R1) THEN
         SLOPE = 1./(R2 - R1)
         GSCALE = ABS ( 2./(R2 - R1) )
      ENDIF
C
      DO 100 IY = 1, NY
         RYSQ = ((IY - CENY) * DY ) ** 2
         DO 50 IX = 1, NX
            RSQ = RYSQ + ((IX - CENX) * DX ) ** 2
            AR3(IX, IY) = 0.0
            IF (AVETYPE(1:1) .EQ. 'G') THEN
               IF (RSQ .GE. R2SQ) THEN
                  AR3(IX, IY) = AR1(IX, IY)
               ELSE
                  IF (R .GT. 0) THEN
                     R  = SQRT (RSQ)
                  ELSE
                     R = 0.0
                  ENDIF
                  WT = EXP(-(R2 - R)*(R2 - R) * GSCALE )
                  AR3(IX, IY) = WT * AR1(IX, IY) + 
     $               (1.0 - WT) * AR2(IX, IY)
               ENDIF
            ELSE
               IF (RSQ .GE. R2SQ) THEN
                  AR3(IX, IY) = AR1(IX, IY)
               ELSE IF (RSQ .LT. R1SQ) THEN
                  AR3(IX, IY) = AR2(IX, IY)

               ELSE
                  IF (AVETYPE(1:1) .EQ. 'A') THEN
                     AR3(IX, IY) = (AR1(IX, IY) + AR2(IX, IY))/2.0
                  ELSE IF (AVETYPE(1:1) .EQ. 'L') THEN
                     WT = (SQRT(RSQ) - R1) * SLOPE
                     AR3(IX, IY) = WT * AR1(IX, IY) + 
     $                  (1.0 - WT) * AR2(IX, IY)
                  ENDIF
               ENDIF
            ENDIF
 50      CONTINUE
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
