C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrimag.f	1.1	 3/26/91
C
      SUBROUTINE ARRIMAG (X, IMAG)
C
CD Take IMAG part of complex image
C
C	X	CH*(*)	input	Name of XPLEX Array
C	I	CH*(*)	input	Name of IMAG Array
C Audit trail:
C	Original version
C				M.A.Holdaway	Feb 27 1991
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	X, IMAG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRIMAG')
C
      CHARACTER		ATYPE
      INTEGER		XADD, IADD
      INTEGER		N, I, NAX, NAXIS(SYSMXDIM),
     $   		NAX2, NAXIS2(SYSMXDIM)
C
      LOGICAL		DATEXIST
C==================================================================
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (X, NAX, NAXIS, ATYPE, XADD)
      IF (ATYPE .NE. 'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'This is not a Complex image!')
         GOTO 990
      ENDIF
C
      IF (.NOT. DATEXIST(IMAG)) THEN
         CALL DATCREAT (IMAG)
         CALL DATMAKAR (IMAG, NAX, NAXIS, 'R', IADD)
      ENDIF
      CALL DATGETAR (IMAG, NAX2, NAXIS2, ATYPE, IADD)
      IF (ATYPE .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'This is not a real image !')
         GOTO 990
      ENDIF
C
      N = 1
      DO 10 I = 1, MAX (NAX, NAX2)
         IF ( NAXIS(I) .NE. NAXIS2(I) ) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes disagree')
            GOTO 990
         ENDIF
         N = N * NAXIS(I)
 10   CONTINUE
C
C convert the Xplex array into real array
C
      CALL PIXIMAG (N, MEMX(XADD), MEMR(IADD) )
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
