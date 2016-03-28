C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixpbcor.f	1.3    11/7/90
C
      SUBROUTINE PIXPBCOR (NAIRY, AIRYARR, RADMAX, BFRACT)
C
CD Pixel Primary Beam Corrupt.  Approximate by Gaussian after BFRACT
C
C
C	NAIRY	INT	inp	Number of elements
C	AIRYARR	REAL	in/out	Array
C	RADMAX	REAL	input	Maximum value of argument
C	BFRACT REAL	input	FRACT of peak at which we make a Gaussian
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 11 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAIRY
      REAL		RADMAX, BFRACT, AIRYARR(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXPBCOR')
C
      REAL		RSCL, A, B, R0, DA, CONST, I1, I2
      REAL		FUDGE, INTTOL
      INTEGER		N0, IT, IAIRY
      CHARACTER*4	STRINT
C
      INTEGER		BLEVEL
      REAL		RADINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      INTTOL = .005
      FUDGE = .2
      RSCL = RADMAX / FLOAT(NAIRY-1) 
C
      N0 = BLEVEL (NAIRY, AIRYARR, BFRACT)
      R0 = RSCL * N0
      IF (ERROR) GOTO 990
C
      I1 = RADINT (NAIRY, N0, AIRYARR, RSCL)
C
      A = SQRT ( 2* I1 / BFRACT )
      DO 100 IT = 1, 50
         B = BFRACT / EXP (-(R0/A)**2)
C
C OK, we've got the fit, now lets replace AIRYARR with the Gaussian
C
         CONST = RSCL/A
         DO 300 IAIRY = N0, NAIRY
            AIRYARR (IAIRY) = B * EXP (-(IAIRY*CONST)**2)
 300     CONTINUE
C
C      To be safe, lets calculate the new area
C
         I2 = RADINT (NAIRY, N0, AIRYARR, RSCL)
         IF (SYSDEBUG) THEN
            WRITE (MESSAGE, 400) I1, I2
 400        FORMAT ('I1 = ', E14.7, '  I2 = ', E14.7)
            CALL MSGPUT (MESSAGE, 'I')
            WRITE (MESSAGE, 401) A, B
 401        FORMAT ('A = ', E14.7, '  B = ', E14.7)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
         IF ( ABS(I1-I2)/I1 .LT. INTTOL) GOTO 500
         A = A * ( 1 + FUDGE*(I1 - I2)/I1 )
 100  CONTINUE
      CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Beam integrals do not match')
      GOTO 990
 500  CONTINUE
      IF (SYSDEBUG) CALL MSGPUT ('Iteration number: '//STRINT(IT), 'I')
      WRITE (MESSAGE, 600) BFRACT
 600  FORMAT ('Corrupting Primary Beam below ', F5.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C
      INTEGER FUNCTION  BLEVEL (N, ARR, FRACT)
#include		"stdinc.h"
      REAL	ARR(*), FRACT
      INTEGER	N, I
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BLEVEL')
      IF (ERROR) GOTO 999
C
      IF (FRACT .GT. .99) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Too large a value for BFRACT')
         GOTO 999
      ELSE IF (FRACT .LT. .001) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Too small a value for BFRACT')
         GOTO 999
      ENDIF
C
      DO 100 I = 1, N
         IF (ARR(I) .LT. FRACT) GOTO 200
 100  CONTINUE
      CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $   'Could not find R0')
      GOTO 999
 200  CONTINUE
      BLEVEL = I
C
 999  CONTINUE
      END
C
      REAL FUNCTION  RADINT (N1, N0, ARR, RSCALE)
#include		"stdinc.h"
      REAL	ARR(*), RSCALE
      INTEGER	N1, N0, I
      REAL	SUM
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'RADINT')
      IF (ERROR) GOTO 999
C
      SUM = 0.0
      DO 100 I = N0+1, N1
         SUM = SUM + ( ARR(I) + ARR(I-1))/2. * RSCALE * 
     $      (RSCALE * (I - .5))
 100  CONTINUE
      RADINT = SUM
C
 999  CONTINUE
      END
C


