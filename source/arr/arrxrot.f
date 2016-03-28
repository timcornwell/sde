C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrxrot.f	1.1	 4/28/93
C
      SUBROUTINE ARRXROT (A1, THETA, A2)
C
CD Rotate the phase of a complex vector: A2 = A1 e^{i THETA}
C
C	A1	CH*(*)	input	Name of array
C	THETA	REAL	input	Rot Angle [degrees]
C	A2	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 28, 1993
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2
      REAL		THETA
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRXROT')
C
      CHARACTER*1	T1
      INTEGER		N1, NAXIS1(SYSMXDIM)
      INTEGER		ADD1
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      IF (T1 .NE. 'X') THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Trying to rotate the phase of non-X: '//T1)
         GOTO 999
      ENDIF
C
C Just DO-IT
C
      CALL ARRX2AP (A1, 'ARRXROT-AMP', 'ARRXROT-PHA')
      CALL ARRSCALE ('ARRXROT-PHA', 1.0, THETA, 'ARRXROT-PHA')
      CALL ARRAP2X ('ARRXROT-AMP', 'ARRXROT-PHA', A2 )
      CALL DATDELET ('ARRXROT-AMP')
      CALL DATDELET ('ARRXROT-PHA')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
