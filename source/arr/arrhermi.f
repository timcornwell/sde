C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrhermi.f	1.2	 24 Jul 1995
C
      SUBROUTINE ARRHERMI (HALF, FULL)
C
CD Make FULL the hermitian of HALF
C
C	HALF	CH*(*)	in	Half plane array (must exist)
C	FULL	CH*(*)	in	Full plane array (must exist)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 30, 1994
C	
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HALF, FULL
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRHERMI')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		HADD, FADD
C
      DATA		NAXIS1	/SYSMXDIM * 1/
      DATA		NAXIS2	/SYSMXDIM * 1/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (HALF, N1, NAXIS1, T1, HADD)
      CALL DATGETAR (FULL, N2, NAXIS2, T2, FADD)
C
      IF (T1.NE.T2) THEN
         WRITE (MESSAGE, 1000) T1, T2
 1000    FORMAT ('Array types for images 1 and 2 disagree : ',A1,1X,A1)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IF (NAXIS2(1) .NE. 2*(NAXIS1(1)-1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes logic error')
         GO TO 999
      ENDIF
      DO 30 I = 2, MAX(N1, N2)
         IF (NAXIS2(I).NE.NAXIS1(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes disagree')
            GO TO 999
         END IF
 30   CONTINUE
C
      IF (NAXIS1(3) .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Cant deal with more than 2 dimentions')
         GOTO 990
      ENDIF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRHERM (MEMR(HADD), NAXIS1(1), NAXIS1(2), 
     $        MEMR(FADD), NAXIS2(1), NAXIS2(2))
      ELSE IF (T1.EQ.'X') THEN
         CALL PIXXHERM (MEMX(HADD), NAXIS1(1), NAXIS1(2), 
     $        MEMX(FADD), NAXIS2(1), NAXIS2(2))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
