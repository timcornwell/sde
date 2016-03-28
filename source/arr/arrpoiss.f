C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrpoiss.f	1.4    11/7/90
C
      SUBROUTINE ARRPOISS (AIN, AOUT, SEED)
C
CD Put poisson noise into an array 
C
C
C	AIN	CH*(*)	input	Name of array
C	AOUT	CH*(*)	input	Name of array
C	SEED	INT	input	Seed (must be 4*k+1 initially)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Removed photons argument
C				T.J.Cornwell	Nov 21 1989
C	Made seed an explicit parameter
C				T.J.Cornwell	Nov 25 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	AIN, AOUT
      INTEGER		SEED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPOISS')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2, NT
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (AIN, N1, NAXIS1, T1, ADD1)
      IF (DATEXIST (AOUT)) THEN
         CALL DATGETAR (AOUT, N2, NAXIS2, T2, ADD2)
         IF (N1.NE.N2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
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
  10     CONTINUE
C
C Check types of arrays
C
         IF (T1.NE.T2) THEN
            WRITE (MESSAGE, 1000) T1, T2
 1000       FORMAT ('Array types for images 1 and 2 disagree : ',
     1         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
      ELSE
C
C Make output array if it does not exist
C
         T2 = T1
	 N2 = N1
         NT = 1
         DO 20 I = 1, N1
            NAXIS2(I) = NAXIS1(I)
            NT = NT * NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (AOUT, N2, NAXIS2, T2, ADD2)
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXRPOIS (MEMR(ADD1), MEMR(ADD2), NT, SEED)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
