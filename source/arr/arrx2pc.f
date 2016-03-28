C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrx2pc.f	1.2	 8/31/91
C
      SUBROUTINE ARRX2PC (COM, AMP, CHI)
C
CD Read in a complex image COM and put out its AMP and CHI
C where COM(i) = AMP(i) * EXP{ i*2*CHI(i)}
C
C Arguments: CALL ARRX2PC (COM, AMP, CHI)
C
C	COM	CH*(*)	input	Name of COMPLEX Image
C	AMP	CH*(*)	input	Name of AMplitude image
C	CHI	CH*(*)	input	Name of position angle image
C Audit trail:
C	Original version
C				M.A.Holdaway	Feb 18 1990
C       Fixed bug in error reporting logic.
C                               D.S.Briggs      Aug 26 1991
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	COM, AMP, CHI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRX2PC')
C
      CHARACTER		ATYPE
      INTEGER		NDUMMY, COMADD, AMPADD, CHIADD
      INTEGER		N, I, NAX, NAXIS(SYSMXDIM)
C
      LOGICAL		DATEXIST
C==================================================================
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (COM, NAX, NAXIS, ATYPE, COMADD)
      IF (ATYPE .NE. 'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'This is not a complex image!')
         GOTO 990
      ENDIF
C
      IF (.NOT. DATEXIST(AMP)) CALL DATCREAT (AMP)
      IF (.NOT. DATEXIST(CHI)) CALL DATCREAT (CHI)
      CALL DATMAKAR (AMP, NAX, NAXIS, 'R', AMPADD)
      CALL DATMAKAR (CHI, NAX, NAXIS, 'R', CHIADD)
      IF (ERROR) GOTO 990
C
      N = 1
      DO 10 I = 1, NAX
         N = N * NAXIS(I)
 10   CONTINUE
C
C convert the complex image to AMP and CHI
C
      CALL DATPUTC (CHI, 'BUNIT', 'DEGREES', 1)
      CALL PIXX2PC (N, MEMX(COMADD), MEMR(AMPADD), MEMR(CHIADD))
C      CALL TBLXTAP (COM, TBLLIST, AMP, CHI)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
