C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrinser.f	1.5    8/14/92
C
      SUBROUTINE ARRINSER (A, B, WINDOW)
C
CD Add array A into array B at window
C
C
C	A	CH*(*)	input	Directory name of array
C	B	CH*(*)	input	Directory name of array
C	WINDOW	CH*(*)	input	Directory name of Window
C Audit trail:
C	TRC was previously being compared to ANAXIS rather than BNAXIS
C				T.J.Cornwell	Feb 24 1989
C	Added D, I, X types
C				T.J.Cornwell	Aug 14 1992
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B, WINDOW
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRINSER')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, BTYPE
      INTEGER		BLC (SYSMXDIM), TRC (SYSMXDIM), 
     1			STEP (SYSMXDIM)
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM),
     1			BNAX, BNAXIS(SYSMXDIM)
      INTEGER 		AADD, BADD, IAX, NDUMMY
      LOGICAL		DATEXIST, DATEXIAR
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
      DATA		BLC	/SYSMXDIM*1/
      DATA		TRC	/SYSMXDIM*1/
      DATA		STEP	/SYSMXDIM*1/
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
C Get window parameters
C
      IF (DATEXIST(STRM2(WINDOW, 'BLC'))) THEN
         CALL DATGETI (WINDOW, 'BLC', BLC, SYSMXDIM, NDUMMY)
      ELSE
         DO 5 IAX = 1, ANAX
           BLC(IAX) = 1
  5      CONTINUE
      END IF
      IF (DATEXIST(STRM2(WINDOW, 'TRC'))) THEN
         CALL DATGETI (WINDOW, 'TRC', TRC, SYSMXDIM, NDUMMY)
      ELSE
         DO 10 IAX = 1, ANAX
            TRC (IAX) = ANAXIS(IAX)
  10     CONTINUE
      END IF
      IF (DATEXIST(STRM2(WINDOW, 'STEP'))) THEN
         CALL DATGETI (WINDOW, 'STEP', STEP, SYSMXDIM, NDUMMY)
      ELSE
         DO 15 IAX = 1, ANAX
            STEP (IAX) = 1
  15     CONTINUE
      END IF
      IF (DATEXIAR (B)) THEN
         CALL DATGETAR (B, BNAX, BNAXIS, BTYPE, BADD)
         IF (ATYPE.NE.BTYPE) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Array types must be the same')
         END IF
      ELSE
         DO 25 IAX = 1, ANAX
            BNAXIS(IAX) = (TRC(IAX) - BLC(IAX) + 1)/STEP(IAX)
  25     CONTINUE
         CALL DATMAKAR (B, ANAX, BNAXIS, ATYPE, BADD)
      END IF
      DO 20 IAX = 1, ANAX
         BLC(IAX) = MIN(MAX(1, BLC(IAX)), BNAXIS(IAX))
         TRC(IAX) = MAX(MIN(TRC(IAX), BNAXIS(IAX)), BLC(IAX))
         IF (STEP(IAX).EQ.0) STEP(IAX) = 1
  20  CONTINUE
      DO 21 IAX = ANAX + 1, SYSMXDIM
         BLC(IAX) = 1
         TRC(IAX) = 1
         STEP(IAX) = 1
 21   CONTINUE
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRINSE (MEMR(AADD), MEMR(BADD), ANAXIS(1), ANAXIS(2), 
     1     ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     2     BNAXIS(1), BNAXIS(2), BNAXIS(3), BNAXIS(4), BNAXIS(5),
     3     BNAXIS(6), BNAXIS(7), BLC, TRC, STEP)
      ELSEIF (ATYPE.EQ.'I') THEN
         CALL PIXIINSE (MEMI(AADD), MEMI(BADD), ANAXIS(1), ANAXIS(2), 
     1     ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     2     BNAXIS(1), BNAXIS(2), BNAXIS(3), BNAXIS(4), BNAXIS(5),
     3     BNAXIS(6), BNAXIS(7), BLC, TRC, STEP)
      ELSEIF (ATYPE.EQ.'D') THEN
         CALL PIXDINSE (MEMD(AADD), MEMD(BADD), ANAXIS(1), ANAXIS(2), 
     1     ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     2     BNAXIS(1), BNAXIS(2), BNAXIS(3), BNAXIS(4), BNAXIS(5),
     3     BNAXIS(6), BNAXIS(7), BLC, TRC, STEP)
      ELSEIF (ATYPE.EQ.'X') THEN
         CALL PIXXINSE (MEMX(AADD), MEMX(BADD), ANAXIS(1), ANAXIS(2), 
     1     ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7),
     2     BNAXIS(1), BNAXIS(2), BNAXIS(3), BNAXIS(4), BNAXIS(5),
     3     BNAXIS(6), BNAXIS(7), BLC, TRC, STEP)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'Cannot sensibly insert array into array')
         GO TO 999
      END IF
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
