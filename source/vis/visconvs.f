C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visconvs.f	1.3    9/14/92
C
      SUBROUTINE VISCONVS (SDATA, N, SCODE)
C
CD Convert an array of stokes parameters in-place
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
       REAL		SDATA(12, *)
       CHARACTER*(*)	SCODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCONVS')
C
      INTEGER		I
      COMPLEX           J
      COMPLEX		SI, SQ, SU, SV, SRR, SLL, SRL, SLR
      REAL		WRR, WLL, WRL, WLR, WIV, WQU
C=======================================================================
      IF (ERROR) GO TO 999
C
      J  =  CMPLX (0.0, 1.0)
C
      IF (SCODE.EQ.'STD') THEN
         DO 10 I = 1, N
            SRR = CMPLX (SDATA(1,I), SDATA(2,I))
            WRR = MAX(SDATA(3,I),0.0)
            SLL = CMPLX (SDATA(4,I), SDATA(5,I))
            WLL = MAX(SDATA(6,I),0.0)
            SRL = CMPLX (SDATA(7,I), SDATA(8,I))
            WRL = MAX(SDATA(9,I),0.0)
            SLR = CMPLX (SDATA(10,I), SDATA(11,I))
            WLR = MAX(SDATA(12,I),0.0)
            WQU = (WRL+WLR)
            IF((WRR.GT.0.0).AND.(WLL.GT.0.0)) THEN
               WIV = (WRR+WLL)/2.0
               SI = 0.5 * (SRR + SLL)
               SV = 0.5 * (SRR - SLL)
            ELSE
               SI = 0.0
               SV = 0.0
               WIV = -1.0
            END IF
            IF((WRl.GT.0.0).AND.(WLR.GT.0.0)) THEN
               WQU = (WRL+WLR)/2.0
               SQ = 0.5 * (SRL + SLR)
               SU = -0.5 * J * (SRL - SLR)
            ELSE
               SQ = 0.0
               SU = 0.0
               WQU = -1.0
            END IF
            SDATA(1,I) = REAL (SI)
            SDATA(2,I) = AIMAG (SI)
            SDATA(3,I) = WIV
            SDATA(4,I) = REAL (SV)
            SDATA(5,I) = AIMAG (SV)
            SDATA(6,I) = WIV
            SDATA(7,I) = REAL (SQ)
            SDATA(8,I) = AIMAG (SQ)
            SDATA(9,I) = WQU
            SDATA(10,I) = REAL (SU)
            SDATA(11,I) = AIMAG (SU)
            SDATA(12,I) = WQU
  10     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot do conversion')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
