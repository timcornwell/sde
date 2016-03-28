C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrgtape.f	1.5    6/7/93
C
      SUBROUTINE ARRGTAPE (A, TAPER, BCEN, B)
C
CD Taper an array with a Gaussian
C
C	A	CH*(*)	input	Directory name of array
C	TAPER	REAL(*)	input	BMAJ, BMIN, BPA etc.
C	BCEN	INT(*)	input	Center of taper
C	B	CH*(*)	input	Directory name of tapered array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	1-D taper added.  Only uses BMAJ
C				D.S.Briggs	Oct 21 1992
C	Added 2-D real case.
C				D.S.Briggs	Mar 1993
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B
      REAL		TAPER (*)
      INTEGER		BCEN (*)
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRGTAPE')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, BTYPE
      INTEGER 		ANAX, ANAXIS(SYSMXDIM), 
     1			BNAX, BNAXIS(SYSMXDIM)
      INTEGER 		AADD, BADD, IAX, NREAL
      LOGICAL		DATEXIAR
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETAR (A, ANAX, ANAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 990
C
      NREAL = 0
      DO 5 IAX = 1, SYSMXDIM
         IF (ANAXIS(IAX).GT.1) NREAL = NREAL + 1
         BCEN (IAX) = MAX (1, MIN (BCEN(IAX), ANAXIS(IAX)))
  5   CONTINUE
C
      IF (DATEXIAR(B)) THEN
         CALL DATGETAR (B, BNAX, BNAXIS, BTYPE, BADD)
         IF (ATYPE.NE.BTYPE) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Array types must be the same')
         END IF
      ELSE
         DO 10 IAX = 1, ANAX
            BNAXIS(IAX) = ANAXIS(IAX)
  10     CONTINUE
         CALL DATMAKAR (B, ANAX, BNAXIS, ATYPE, BADD)
      END IF
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF ((ATYPE.EQ.'X').AND.(NREAL.EQ.1)) THEN
         CALL PIX1DGXT (MEMX(AADD), ANAXIS(1), TAPER(1), BCEN(1),
     $      MEMX(BADD))
      ELSE IF ((ATYPE.EQ.'X').AND.(NREAL.EQ.2)) THEN
         CALL PIX2DGXT (MEMX(AADD), ANAXIS(1), ANAXIS(2),
     1      TAPER(1), TAPER(2), TAPER(3), BCEN(1), BCEN(2),
     1      MEMX(BADD))
      ELSE IF ((ATYPE.EQ.'X').AND.(NREAL.EQ.3)) THEN
         CALL PIX3DXGT (MEMX(AADD), ANAXIS(1), ANAXIS(2),
     1      ANAXIS(3), TAPER(1), TAPER(2), TAPER(3), TAPER(4),
     2      BCEN(1), BCEN(2), BCEN(3), MEMX(BADD))
      ELSE IF ((ATYPE.EQ.'R').AND.(NREAL.EQ.2)) THEN
         CALL PIX2DGRT (MEMR(AADD), ANAXIS(1), ANAXIS(2),
     1      TAPER(1), TAPER(2), TAPER(3), BCEN(1), BCEN(2),
     1      MEMR(BADD))
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
