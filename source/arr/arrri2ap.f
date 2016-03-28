C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrri2ap.f	1.4	 7/20/92
C
      SUBROUTINE ARRRI2AP (A, B, C)
C
C Convert complex (real, imag.) input to (real ampl, real phase) out
C
C
C	A	CH*(*)	input	Directory name of array
C	B	CH*(*)	input	Directory name of amplitude
C	C	CH*(*)	input	Directory name of phase
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*) 	A, B, C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRRI2AP')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, OTYPE
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM), ANPIX
      INTEGER 		AADD, BADD, CADD
      LOGICAL		DATEXIST
      DATA 		ANAXIS	/SYSMXDIM*1/
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
      IF (ATYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Input array must be complex')
      END IF
C
      OTYPE = 'R'
      CALL DATMAKAR (B, ANAX, ANAXIS, OTYPE, BADD)
      CALL DATMAKAR (C, ANAX, ANAXIS, OTYPE, CADD)
      IF (ERROR) GO TO 990
C
C Now actually call routine which does the work on the pixels. 
C
      CALL PIXRI2AP (MEMX(AADD), MEMR(BADD),
     1      MEMR(CADD), ANAXIS(1), ANAXIS(2))
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
