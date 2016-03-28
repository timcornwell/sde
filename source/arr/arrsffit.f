C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsffit.f	1.2	 11/27/91
C
      SUBROUTINE ARRSFFIT (IN, MASK, AMIN)
C
CD Fits and removes a linear (pointing) and a quadratic (focus) term from arr
C  Designed for surface error simulations
C  Fits a function F = A*X + B*Y + C*(X^2 + Y^2) to IN and subtracts it
C  from OUT
C
C	IN	CH*(*)	in/out	Input Image
C	MASK	CH*(*)	input	Maks used for IN
C	AMIN	REAL	input	Masking level
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C	Added MASK
C				M.A.Holdaway	Nov 27 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, MASK
      REAL		AMIN
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRSFFIT')
C
      INTEGER           NAX1, NAXIS1(SYSMXDIM)
      INTEGER           NAX2, NAXIS2(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		ADD1, ADD2, I, NT
      CHARACTER*1	ATYPE1, ATYPE2
      REAL		A, B, C, AVE, RMS, SCALE
C
      INTEGER		CRDRNAX
C
      DATA		NAXIS1 /SYSMXDIM * 1/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IN, NAX1, TYPE, NAXIS1, RVAL, RPIX, DELT, ROTA)
      CALL DATGETAR (IN, NAX1, NAXIS1, ATYPE1, ADD1)
      CALL DATGETAR (MASK, NAX2, NAXIS2, ATYPE2, ADD2)
      IF ( CRDRNAX (NAX1, NAXIS1) .NE. 2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Must be 2-D array')
         GOTO 990
      ENDIF
      IF ( CRDRNAX (NAX2, NAXIS2) .NE. 2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Must be 2-D array')
         GOTO 990
      ENDIF
      DO 100 I = 1, 2
         IF (NAXIS1(I) .NE. NAXIS2(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Mismatched naxis')
            GOTO 990
         ENDIF
 100  CONTINUE
      IF (ATYPE1 .NE. ATYPE2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Mismatched array types')
         GOTO 990
      ENDIF
C
      IF (ATYPE1 .EQ. 'R') THEN
C
C Fit a slope (pointing term)
C
         CALL PIX2SFIT (MEMR(ADD1), MEMR(ADD2), AMIN, NAXIS1(1), 
     $      NAXIS1(2), RPIX(1), RPIX(2), A, B)
C
C Remove the Slope
C
         CALL PIX2SSUB (MEMR(ADD1), MEMR(ADD2), AMIN, NAXIS1(1), 
     $      NAXIS1(2), RPIX(1), RPIX(2), A, B)
C
C Fit a parabola centered at RPIX (orthogonal to linear term)
C
         CALL PIX2FFIT (MEMR(ADD1), MEMR(ADD2), AMIN, NAXIS1(1), 
     $      NAXIS2(2), RPIX(1), RPIX(2), C)
C
C Remove parabola (focus term)
C
         CALL PIX2FSUB (MEMR(ADD1), MEMR(ADD2), AMIN, NAXIS1(1), 
     $      NAXIS1(2), RPIX(1), RPIX(2), C)
C
         WRITE (MESSAGE, 1111) A, B, C
 1111    FORMAT ('Fit Pars: A, B, C:  ',4E14.7)
         CALL MSGPUT (MESSAGE, 'D')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Coded only for REAL')
         GOTO 990
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
