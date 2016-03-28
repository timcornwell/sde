C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrlist.f	1.6	 9/25/93
C
      SUBROUTINE ARRLIST (ARRAY, WHAT, ISKIP)
C
CD List out arrays on MSGPUT
C
C	ARRAY	CH*(*)	input	Name of array
C	WHAT	CH*(*)	input	Identifying comment
C	ISKIP	INT	input	skip this many between printouts
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 1 1990
C	Now understands multidimentional arrays
C				M.A.Holdaway	May 12 1991
C	....and also Integer arrays
C				M.A.Holdaway	Feb 8 1993
C	....and character string arrays
C				D.S.Briggs	Sept 16 1993
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ARRAY
      INTEGER		ISKIP
      CHARACTER*(*)	WHAT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRLIST')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), AADD, NT, I
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR( ARRAY, NAX, NAXIS, ATYPE, AADD)
      NT = 1
      DO 100 I = 1, NAX
         IF (NAXIS(I) .GT. 0)  NT = NT * NAXIS(I)
 100  CONTINUE
C
      IF (ATYPE .EQ. 'R') THEN
         CALL PIXRLIS1 (MEMR(AADD), NT, ISKIP, WHAT)
      ELSE IF (ATYPE .EQ. 'X') THEN
         CALL PIXXLIS1 (MEMX(AADD), NT, ISKIP, WHAT)
      ELSE IF (ATYPE .EQ. 'D') THEN
         CALL PIXDLIS1 (MEMD(AADD), NT, ISKIP, WHAT)
      ELSE IF (ATYPE .EQ. 'I') THEN
         CALL PIXILIS1 (MEMI(AADD), NT, ISKIP, WHAT)
      ELSE IF (ATYPE .EQ. 'S') THEN
         CALL PIXSLIS1 (MEMC(AADD), NT, ISKIP, WHAT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'ARRLIST doesn''t deal with type '//ATYPE)
         GOTO 990
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
