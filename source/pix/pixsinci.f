C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsinci.f	1.1    4/3/91
C
      SUBROUTINE PIXSINCI (NA, A1, A2)
C
CD Perform mid-point sinc interpolation
C
C	NA	INT		inp	length of A1
C	A1	REAL(NA)	inp	input array
C	A2	REAL(2*NA)	out	interpolated array
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 11 1991
C	!! The way I get this to work is by making
C	A1(NA/2+1) be the central pixel
C				M.A.Holdaway	April 3 1991
C	We have made NS very large for our application (PB)
C				M.A.Holdaway	April 3 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	NA
      REAL	A1(*), A2(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSINCI')
C
      INTEGER	NS
      PARAMETER	(NS = 1000)
C
      REAL	SINC(-NS+1:NS), WORK
      REAL	PI
      INTEGER	I, J, N, IMIN, IMAX
C
      SAVE	SINC
C==================================================================
C
      IF (ERROR) GOTO 999
      PI = ATAN2 (1.0, 1.0) * 4.0
C
C Assign to SINC the half-point interpolation values
C
      WORK = SIN(PI * (.5))/ (PI * (.5))
      IF (SINC(1) .NE. WORK) THEN
         DO 100 I = -NS+1, NS
            SINC(I) = SIN(PI * (I - .5))/ (PI * (I - .5))
 100     CONTINUE
      ENDIF
C
C Do interpolation
C
      N = NA + 1
      DO 300 I = 1, N
            WORK = 0.0
            IMIN = MAX (1, I - NS + 1)
            IMAX = MIN (N, I + NS)
            DO 250 J = IMIN, IMAX
               WORK = WORK + A1(J) * SINC(I-J+1)
 250        CONTINUE
            A2(2*I - 1) = A1(I)
            A2(2*I) = WORK
 300  CONTINUE
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      ENDIF
 999  CONTINUE
C
      END
