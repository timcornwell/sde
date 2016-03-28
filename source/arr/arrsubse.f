C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsubse.f	1.8    6/7/93
C
      SUBROUTINE ARRSUBSE (A, B, WINDOW)
C
CD Subsection an array
C
C
C	A	CH*(*)	input	Directory name of array
C	B	CH*(*)	input	Directory name of array
C	WINDOW	CH*(*)	input	Directory name of window
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added COMPLEX capability
C				M.A.Holdaway	Dec 3 1990
C       Summing added
C                               R.G Marson      Mar 4 1991
C	Added DOUBLE PRECISION capability
C				D.S.Briggs	Mar 25 1993
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B, WINDOW
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSUBSE')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, BTYPE
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER		BLC (SYSMXDIM), TRC (SYSMXDIM), 
     1			STEP (SYSMXDIM), SUM (SYSMXDIM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM),
     1			BNAX, BNAXIS(SYSMXDIM)
      INTEGER 		AADD, BADD, IAX, NDUMMY, BELEM
      LOGICAL		DATEXIST, DATEXIAR
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
      DATA 		BLC	/SYSMXDIM*1/
      DATA 		TRC	/SYSMXDIM*1/
      DATA 		STEP	/SYSMXDIM*1/
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
      DO 1 IAX = 1, ANAX
         ANAXIS (IAX) = MAX(1, ANAXIS(IAX))
  1   CONTINUE
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
      IF (DATEXIST(STRM2(WINDOW, 'SUM'))) THEN
         CALL DATGETI (WINDOW, 'SUM', SUM, SYSMXDIM, NDUMMY)
      ELSE
         DO 17 IAX = 1, ANAX
            SUM (IAX) = 1
 17      CONTINUE
      END IF
C
      DO 20 IAX = 1, ANAX
         BLC(IAX) = MAX(1, MIN(BLC(IAX), ANAXIS(IAX)))
         TRC(IAX) = MIN(MAX(BLC(IAX), TRC(IAX)), ANAXIS(IAX))
         STEP(IAX) = MAX(1,MIN(TRC(IAX)-BLC(IAX)+1,STEP(IAX)))
         SUM(IAX) = MAX(1,MIN(TRC(IAX)-BLC(IAX)+1,SUM(IAX)))
  20  CONTINUE
      DO 21 IAX = ANAX + 1, SYSMXDIM
         BLC(IAX) = 1
         TRC(IAX) = 1
         STEP(IAX) = 1
         SUM(IAX) = 1
 21   CONTINUE
C
      IF (DATEXIAR (B)) THEN
         CALL DATGETAR (B, BNAX, BNAXIS, BTYPE, BADD)
         IF (ATYPE.NE.BTYPE) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Array types must be the same')
         END IF
      ELSE
         DO 30 IAX = 1, ANAX
            BNAXIS(IAX) = (TRC(IAX) - BLC(IAX) + 1)/STEP(IAX)
  30     CONTINUE
         CALL DATMAKAR (B, ANAX, BNAXIS, ATYPE, BADD)
      END IF
      BELEM = 1
      DO 33 IAX = 1, ANAX
         BELEM = BELEM * BNAXIS(IAX)
 33   CONTINUE
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRSETC(MEMR(BADD), 0.0, BELEM)
         CALL PIXRSUBS (MEMR(AADD), MEMR(BADD), 
     $        ANAXIS(1), ANAXIS(2), ANAXIS(3), ANAXIS(4), ANAXIS(5), 
     $        ANAXIS(6), ANAXIS(7), BNAXIS(1), BNAXIS(2), BNAXIS(3), 
     $        BNAXIS(4), BNAXIS(5), BNAXIS(6), BNAXIS(7), 
     $        BLC, TRC, STEP, SUM)
      ELSE IF (ATYPE.EQ.'D') THEN
         CALL PIXDSETC(MEMD(BADD), 0.0, BELEM)
         CALL PIXDSUBS (MEMD(AADD), MEMD(BADD), 
     $        ANAXIS(1), ANAXIS(2), ANAXIS(3), ANAXIS(4), ANAXIS(5), 
     $        ANAXIS(6), ANAXIS(7), BNAXIS(1), BNAXIS(2), BNAXIS(3), 
     $        BNAXIS(4), BNAXIS(5), BNAXIS(6), BNAXIS(7), 
     $        BLC, TRC, STEP, SUM)
      ELSE IF (ATYPE .EQ. 'X') THEN
         CALL PIXXSETC(MEMX(BADD), 0.0, BELEM)
         CALL PIXXSUBS (MEMX(AADD), MEMX(BADD), ANAXIS(1),
     1     ANAXIS(2), ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6),
     2     ANAXIS(7), BNAXIS(1), BNAXIS(2), BNAXIS(3), BNAXIS(4),
     3     BNAXIS(5), BNAXIS(6), BNAXIS(7), BLC, TRC, STEP, SUM)
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'Cannot sensibly subsection array')
         GO TO 999
      END IF
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
