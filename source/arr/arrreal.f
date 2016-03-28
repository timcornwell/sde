C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrreal.f	1.4	 7/20/92
C
      SUBROUTINE ARRREAL (IN, R)
C
CD Take real part of complex image
C
C	IN	CH*(*)	input	Name of XPLEX or IM Array
C	R	CH*(*)	input	Name of Real Array
C Audit trail:
C	Original version
C				M.A.Holdaway	Feb 27 1991
C	Added I --> R conversion
C				M.A.Holdaway	Aug 7 1991
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	IN, R
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRREAL')
C
      CHARACTER		ATYPE, ATYPE2
      INTEGER		INADD, RADD
      INTEGER		N, I, NAX, NAXIS(SYSMXDIM),
     $   		NAX2, NAXIS2(SYSMXDIM)
C
      LOGICAL		DATEXIST
C==================================================================
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, INADD)
C
      IF (.NOT. DATEXIST(R)) THEN
         CALL DATCREAT (R)
         CALL DATMAKAR (R, NAX, NAXIS, 'R', RADD)
      ENDIF
      CALL DATGETAR (R, NAX2, NAXIS2, ATYPE2, RADD)
      IF (ATYPE2 .NE. 'R') THEN
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
C convert the Xplex or Integer array into real array
C
      IF (ATYPE .EQ. 'I') THEN
         CALL PIXIREAL (N, MEMI(INADD), MEMR(RADD) )
      ELSEIF (ATYPE .EQ. 'X') THEN
         CALL PIXXREAL (N, MEMX(INADD), MEMR(RADD) )
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Routine not for this array type: '//ATYPE)
         GOTO 990
      ENDIF
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
