C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visconv2.f	1.3    9/14/92
C
      SUBROUTINE VISCONV2 (SDATA, N, SCODE)
C
CD Convert an array of stokes parameters in-place: only for I and V
C
C
C	SDATA	REAL	input	Stokes data
C	N	INT	input	Number of sets of data
C	SCODE	CH*(*)	input	What we want: either STD or R&L
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Strict conversion
C				T.J.Cornwell	Sept 6 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       INTEGER		N
       REAL		SDATA(6, *)
       CHARACTER*(*)	SCODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCONV2')
C
      INTEGER		I
      COMPLEX		SI, SV, SRR, SLL
      REAL		WRR, WLL, WIV
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (SCODE.EQ.'STD') THEN
         DO 10 I = 1, N
            SRR = CMPLX (SDATA(1,I), SDATA(2,I))
            WRR = MAX(SDATA(3,I),0.0)
            SLL = CMPLX (SDATA(4,I), SDATA(5,I))
            WLL = MAX(SDATA(6,I),0.0)
            IF ((WRR.GT.0.0).AND.(WLL.GT.0.0)) THEN
               WIV = (WRR+WLL)/2.0
               SI = (SRR + SLL)/2.0
               SV = (SRR - SLL)/2.0
            ELSE
               SI = 0.0
               SV = 0.0
               WIV = -1.0
            END IF
            SDATA(1,I) = REAL (SI)
            SDATA(2,I) = AIMAG (SI)
            SDATA(3,I) = WIV
            SDATA(4,I) = REAL (SV)
            SDATA(5,I) = AIMAG (SV)
            SDATA(6,I) = WIV
  10     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot do conversion')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
