C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrx2qu.f	1.2	 2/27/91
C
      SUBROUTINE ARRX2QU (X, Q, U )
C
CD Input a Complex image X and put out its REAL (Q) and IMAGINARY (U) parts
C
C Arguments: CALL ARRX2QU (X, Q, U)
C
C	X	CH*(*)	input	Name of XPLEX Image
C	Q	CH*(*)	input	Name of Q image
C	U	CH*(*)	input	Name of U image
C Audit trail:
C	Original version
C				M.A.Holdaway	June 10 1990
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	X, Q, U
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRX2QU')
C
      CHARACTER		ATYPE
      INTEGER		XADD, QADD, UADD
      INTEGER		N, I, NAX, NAXIS(SYSMXDIM)
      INTEGER		NAX2, NAXIS2(SYSMXDIM)
      INTEGER		NAX3, NAXIS3(SYSMXDIM)
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
      IF (.NOT. DATEXIST(Q)) THEN
         CALL DATCREAT (Q)
         CALL DATMAKAR (Q, NAX, NAXIS, 'R', QADD)
      ENDIF
      CALL DATGETAR (Q, NAX2, NAXIS2, ATYPE, QADD)
      IF (ATYPE .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'This is not a Real image!')
         GOTO 990
      ENDIF
C
      IF (.NOT. DATEXIST(U)) THEN
         CALL DATCREAT (U)
         CALL DATMAKAR (U, NAX, NAXIS, 'R', UADD)
      ENDIF
      CALL DATGETAR (U, NAX3, NAXIS3, ATYPE, UADD)
      IF (ATYPE .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'This is not a Real image!')
         GOTO 990
      ENDIF
C
      N = 1
      NAX = MAX(NAX, NAX2)
      NAX = MAX(NAX, NAX3)
      DO 10 I = 1, NAX
         IF (NAXIS(I) .NE. NAXIS2(I) .OR. NAXIS(I) .NE. NAXIS3(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes disagree')
            GOTO 990
         ENDIF
         N = N * NAXIS(I)
 10   CONTINUE
C
C convert the Xplex image to Q and U
C
      CALL PIXX2QU (N, MEMX(XADD), MEMR(QADD), MEMR(UADD))
C      CALL TBLXTAP (X, TBLLIST, Q, U)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
