C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrwchis.f	1.3    11/7/90
C
      SUBROUTINE ARRWCHIS (IN, WT, CHISQ, SUMWT)
C
CD Form weighted chisq
C
C
C	IN	CH*(*)	input	Name of array
C	WT	CH*(*)	input	Name of array
C	CHISQ	REAL	output	Weighted chisq
C	SUMWT	REAL	output	Sum of weights
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Removed calculation of grad(CHISQ)
C				T.J.Cornwell	May 31 1990
C	Added support of real arrays
C				T.J.Cornwell	July 7 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		CHISQ, SUMWT
      CHARACTER*(*)	IN, WT
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRWCHIS')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (WT, N2, NAXIS2, T2, ADD2)
      IF (N1.NE.N2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different number of axes')
         GO TO 999
      END IF
      NT = 1
      DO 10 I = 1, N1
         IF (NAXIS1(I).NE.NAXIS2(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
            GO TO 999
         ELSE
            NT = NT * NAXIS1(I)
         END IF
  10  CONTINUE 
C
      IF (T1.EQ.'X') THEN
         CALL PIXXWCHI (MEMX(ADD1), MEMR(ADD2), NT, CHISQ, SUMWT)
      ELSEIF (T1.EQ.'R') THEN
         CALL PIXRWCHI (MEMR(ADD1), MEMR(ADD2), NT, CHISQ, SUMWT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
C
      IF (CHISQ.LT.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Chisquared < 0.0')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
