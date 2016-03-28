C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vischsqp.f	1.2	 9/14/92
C
      SUBROUTINE VISCHSQP (VIS, MVIS, WT, NVIS, CHISQ, SUMWT,
     $   SUMWTS, NU)
C
CD Calculate Chi^2 wrt model, pixel level
C
C	VIS	CMPLX	input	Input visibilities
C	MVIS	CMPLX	input	Input model visibilities
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	CHISQ	REAL	output	Reduced Chi^2
C	NU	INT	output	degrees of freedom
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	6 July 1992
C	Changed to return more useful info
C				T.J.Cornwell	Sept 14 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      REAL		WT(NVIS)
      COMPLEX		VIS(NVIS), MVIS(NVIS)
      REAL		CHISQ, SUMWT, SUMWTS
      INTEGER		NU
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCHSQP')
C
      INTEGER		I
C=====================================================================
      IF (ERROR) GO TO 999
C
      NU = 0
      SUMWT = 0.0
      SUMWTS = 0.0
      CHISQ = 0.0
C
      DO 100 I = 1, NVIS
         IF (WT(I).GT.0.0) THEN
            CHISQ = CHISQ + ABS(VIS(I) - MVIS(I))**2 * WT(I)
            SUMWT = SUMWT + WT(I)
            SUMWTS = SUMWTS + ABS(VIS(I) - MVIS(I))**2 * WT(I)**2
            NU = NU + 1
         END IF
 100  CONTINUE
C
 999  CONTINUE
      END


