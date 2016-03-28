C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrdtape.f	1.4	 7/20/92
C
      SUBROUTINE ARRDTAPE (A, B, MIND, C)
C
C Taper an array with the inverse amplitudes of another
C
C
C	A	CH*(*)	input	Directory name of array
C	B	CH*(*)	input	Directory name of dividing amplitudes
C	MIND	REAL	input	Minimum denominator
C	C	CH*(*)	input	Directory name of tapered array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
C
#include 		"stdinc.h"
C
      CHARACTER*(*) 	A, B, C
      REAL		MIND
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRDTAPE')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, BTYPE, CTYPE
      INTEGER 		ANAX, ANAXIS(SYSMXDIM), 
     1			BNAX, BNAXIS(SYSMXDIM), 
     2			CNAX, CNAXIS(SYSMXDIM)
      INTEGER 		AADD, BADD, CADD, IAX, NREAL
      LOGICAL		DATEXIAR
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
      DATA 		CNAXIS	/SYSMXDIM*1/
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETAR (A, ANAX, ANAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 990
C
      CALL DATGETAR (B, BNAX, BNAXIS, BTYPE, BADD)
      IF (ERROR) GO TO 990
C
      NREAL = 0
      DO 5 IAX = 1, SYSMXDIM
         IF (ANAXIS(IAX).GT.1) NREAL = NREAL + 1
  5   CONTINUE
      IF (DATEXIAR (C)) THEN
         CALL DATGETAR (C, CNAX, CNAXIS, CTYPE, CADD)
         IF (ATYPE.NE.CTYPE) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Array types must be the same')
         END IF
      ELSE
         DO 10 IAX = 1, ANAX
            CNAXIS(IAX) = ANAXIS(IAX)
  10     CONTINUE
         CALL DATMAKAR (C, ANAX, CNAXIS, ATYPE, CADD)
      END IF
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF ((ATYPE.EQ.'X').AND.(BTYPE.EQ.'R').AND.(NREAL.EQ.2)) THEN
         CALL PIX2DXDT (MEMX(AADD), MEMR(BADD),
     1      ANAXIS(1), ANAXIS(2), MIND, MEMX(CADD))
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'Cannot sensibly taper array ')
         GO TO 999
      END IF
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
