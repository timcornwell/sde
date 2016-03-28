C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visconvp.f	1.3    9/14/92
C
      SUBROUTINE VISCONVP (SDATA, WDATA, N, SCODE)
C
CD Convert an array of stokes parameters in-place
C
C
C	SDATA	REAL	input	Stokes data
C	WDATA	REAL	input	Weights
C	N	INT	input	Number of sets of data
C	SCODE	CH*(*)	input	What we want: either STD or R&L
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Order now consistent with VISCONVS and VISSTD
C				T.J.Cornwell	Sept 14 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       INTEGER		N
       REAL		SDATA(8, *), WDATA(*)
       CHARACTER*(*)	SCODE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCONVP')
C
      INTEGER		I
      COMPLEX		J
      COMPLEX		SI, SQ, SU, SV, SRR, SLL, SRL, SLR
      REAL		WRR, WLL, WRL, WLR, WIV, WQU
C=======================================================================
      IF (ERROR) GO TO 999
C
      J  =  CMPLX(0.0, 1.0)
C
      IF (SCODE.EQ.'STD') THEN
         DO 10 I = 1, N
            SRR = CMPLX (SDATA(1,I), SDATA(2,I))
            WRR = MAX(WDATA(I),0.0)
            SLL = CMPLX (SDATA(3,I), SDATA(4,I))
            WLL = WRR
            SRL = CMPLX (SDATA(5,I), SDATA(6,I))
            WRL = WRR
            SLR = CMPLX (SDATA(7,I), SDATA(8,I))
            WLR = WRR
            WQU = (WRL+WLR)
            IF (WRR.GT.0.0) THEN
               WIV = WRR
               SI = (SRR + SLL)/2.0
               SV = (SRR - SLL)/2.0
               SQ = 0.5 * (SRL + SLR)
               SU = -0.5 * J * (SRL - SLR)
            ELSE
               SI = 0.0
               SV = 0.0
               SQ = 0.0
               SU = 0.0
               WIV = -1.0
               WQU = -1.0
            END IF
            SDATA(1,I) = REAL (SI)
            SDATA(2,I) = AIMAG (SI)
            SDATA(3,I) = REAL (SV)
            SDATA(4,I) = AIMAG (SV)
            SDATA(5,I) = REAL (SQ)
            SDATA(6,I) = AIMAG (SQ)
            SDATA(7,I) = REAL (SU)
            SDATA(8,I) = AIMAG (SU)
  10     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot do conversion')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
