C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrshado.f	1.1	 3/31/94
C
      SUBROUTINE ARRSHADO (IMAGE, VIEW, HEIGHT, SHADOW)
C
CD Program to make an image of what is shadow from some illumination point
C
C
C	IMAGE	CH*(*)	input	Name of array
C     	VIEW	R(3)	input	Coordinates of view, in pixel coordinates
C	HEIGHT	R	input	Is an object of HEIGHT still in shadow?
C	SHADOW	CH*(*)	input	Name of output array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C
C	Assumes that SHADOW exists
C
C				M.A.Holdaway	April 1 1994
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE, SHADOW
      REAL		HEIGHT, VIEW(3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSHADO')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM)
      INTEGER		ADD1, ADD2
C
      DATA		NAXIS1	/SYSMXDIM * 1/
      DATA		NAXIS2	/SYSMXDIM * 1/
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMAGE, N1, NAXIS1, T1, ADD1)
      CALL DATGETAR (SHADOW, N2, NAXIS2, T2, ADD2)
      DO 10 I = 1, MAX(N1, N2)
         IF (NAXIS1(I).NE.NAXIS2(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
            GO TO 999
         END IF
  10  CONTINUE
C
C Check types of arrays
C
      IF (T1.NE.T2) THEN
         WRITE (MESSAGE, 1000) T1, T2
 1000    FORMAT ('Array types for images 1 and 2 disagree : ',A1,1X,A1)
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C
C Call appropriate routine
C
      IF (.NOT.(NAXIS1(2) .GT. 1 .AND. NAXIS1(3) .EQ. 1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Works on 2-D arrays')
      ENDIF

      IF (T1.EQ.'R') THEN
         CALL PIXRSHAD (MEMR(ADD1), NAXIS1(1), NAXIS1(2), VIEW, HEIGHT, 
     $        MEMR(ADD2) )
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
