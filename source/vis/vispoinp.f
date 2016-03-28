C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispoinp.f	1.2    11/7/90
C
      SUBROUTINE VISPOINP (VIS, WT, NVIS, FLUX, NEWWT)
C
CD Set data to point source
C
C
C	VIS	CMPLX	input	Input visibilities
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	FLUX	REAL	input	Estimated flux
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS
      COMPLEX		VIS(NVIS)
      REAL		WT(NVIS), NEWWT(NVIS)
      REAL		FLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPOINP')
C
      INTEGER		IVIS
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 100 IVIS = 1, NVIS
         NEWWT(IVIS) = WT(IVIS)
         VIS(IVIS) = FLUX
 100  CONTINUE
C
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
