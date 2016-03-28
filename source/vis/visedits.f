C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visedits.f	1.2    11/7/90
C
      SUBROUTINE VISEDITS (VIS, VISWT, NANT, THRES, MODE, ORESID,
     $   NRESID, SCANWT)
C
CD Fla data on the basis of visibility discrepancies. 
C
C
C	VIS	CMPLX	input	Visibilities
C	VISWT	REAL	input	Visibility weights
C	NANT	INT	input	Number of antennas (not more than 28)
C      THRES   REAL    input   Threshold in sigma
C	MODE	CH*(*)	input	Mode
C	ORESID	REAL	output	Error before fit
C	NRESID	REAL	output	Error after fit
C	SCANWT	REAL	output	Total weight for this scan
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NANT
      COMPLEX		VIS(NANT,*)
      REAL		VISWT(NANT, *), ORESID, NRESID, SCANWT, THRES
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEDITS')
C
      INTEGER		IANT, JANT
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialise
C
      ORESID = 0.0
      NRESID = 0.0
C
C Find initial rms
C
      SCANWT = 0.0
      DO 14 IANT = 1, NANT
         DO 13 JANT = 1, NANT
            IF(VISWT(IANT,JANT).GT.0.0) THEN
               ORESID = ORESID + ABS(VIS(IANT, JANT))**2 *
     1         VISWT(IANT, JANT)
               SCANWT = SCANWT + VISWT(IANT,JANT)
            END IF
  13     CONTINUE
  14  CONTINUE
C
      IF (SCANWT.EQ.0.0) GO TO 999
      ORESID = SQRT(ORESID/SCANWT)
C
      DO 21 JANT = 1, NANT
         DO 20 IANT = 1, NANT
            IF(ABS(VIS(IANT,JANT)).GT.THRES * ORESID) THEN
               VISWT(IANT,JANT) = 0.0
            ENDIF
  20     CONTINUE
  21  CONTINUE
C
C Find residual
C
      NRESID = 0.0
      SCANWT = 0.0
      DO 41 JANT = 1, NANT
         DO 40 IANT = 1, NANT
            NRESID = NRESID + VISWT(IANT, JANT)*ABS(VIS(IANT,JANT))**2
            SCANWT = SCANWT + VISWT(IANT, JANT)
  40     CONTINUE
  41  CONTINUE
      IF (SCANWT.EQ.0.0) GO TO 999
      NRESID = SQRT(NRESID/SCANWT)
C
C Can jump to here if an error found
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
