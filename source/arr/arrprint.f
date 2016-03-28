C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrprint.f	1.6    11/16/92
C
      SUBROUTINE ARRPRINT (ANAME, WINDOW)
C
CD Print an array
C
C
C	ANAME	CH*(*)	input	Directory name of array
C	WINDOW	CH*(*)	input	Directory name of window
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Bug fix in axis handling
C				D.S. Briggs	Nov 5 1992
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ANAME, WINDOW
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRPRINT')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM)
      INTEGER 		AADD, IAX, NDUMMY
      LOGICAL		DATEXIST
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BLC	/SYSMXDIM*1/
      DATA 		TRC	/SYSMXDIM*1/
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETAR (ANAME, ANAX, ANAXIS, ATYPE, AADD)
      DO 1 IAX = 1, ANAX
         ANAXIS(IAX) = MAX(1, ANAXIS(IAX))
  1   CONTINUE
      DO 3 IAX = ANAX+1, SYSMXDIM
         ANAXIS(IAX) = 1
 3    CONTINUE
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
      DO 20 IAX = 1, SYSMXDIM
         BLC(IAX) = MAX(1, BLC(IAX))
         TRC(IAX) = MAX(BLC(IAX), MIN(TRC(IAX), ANAXIS(IAX)))
  20  CONTINUE
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRPRIN (MEMR(AADD), BLC, TRC, ANAXIS(1), ANAXIS(2), 
     1      ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7))
      ELSE IF (ATYPE.EQ.'X') THEN
         CALL PIXXPRIN (MEMX(AADD), BLC, TRC, ANAXIS(1), ANAXIS(2), 
     1      ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7))
      ELSE IF (ATYPE.EQ.'I') THEN
         CALL PIXIPRIN (MEMI(AADD), BLC, TRC, ANAXIS(1), ANAXIS(2), 
     1      ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7))
      ELSE IF (ATYPE.EQ.'D') THEN
         CALL PIXDPRIN (MEMD(AADD), BLC, TRC, ANAXIS(1), ANAXIS(2), 
     1      ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7))
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'CANNOT SENSIBLY PRINT ARRAY')
         GO TO 999
      END IF
C
C Trace errors
C
      IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
