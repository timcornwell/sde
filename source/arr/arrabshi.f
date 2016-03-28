C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrabshi.f	1.4    7/23/92
C
      SUBROUTINE ARRABSHI (A, HIST)
C
CD Make a histogram of the absolute value of an array
C
C
C	A	CH*(*)	input	Name of array
C	HIST	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, HIST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRABSHI')
C
      CHARACTER*1	TA, TH
C
      REAL		BASE, INCR
      INTEGER		I, NA, NB, NH, ANAXIS(SYSMXDIM),
     1			HNAXIS(SYSMXDIM)
      INTEGER		ADDA, ADDH, NT, NDUMMY
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NA, ANAXIS, TA, ADDA)
      IF (DATEXIST (HIST)) THEN
         CALL DATGETAR (HIST, NB, HNAXIS, TH, ADDH)
         NB = HNAXIS(1)
      ELSE
C
C Make output array if it does not exist
C
         TH = TA
         IF (DATEXIST (STRM2(HIST, 'NBOX'))) THEN
            CALL DATGETI (HIST, 'NBOX', NB, 1, NDUMMY)
         ELSE
            NB = 1024
         END IF
         NH = 1
         HNAXIS(1) = NB
         TH = 'I'
         CALL DATMAKAR (HIST, NH, HNAXIS, TH, ADDH)
      END IF
      NT = 1
      DO 10 I = 1, NA
         NT = NT * ANAXIS(I)
  10  CONTINUE
C
C Call appropriate routine
C
      IF (TA.EQ.'R') THEN
         CALL PIXRABSH (MEMR(ADDA), NB, MEMI(ADDH), NT, BASE,
     1      INCR)
         CALL DATPUTR (HIST, 'BASE', BASE, 1)
         CALL DATPUTR (HIST, 'INCREMENT', INCR, 1)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type not supported')
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
