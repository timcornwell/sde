C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsetav.f	1.2    11/26/91
C
      SUBROUTINE ARRSETAV (IN, MASK, SAVE, SRMS, AMIN)
C
CD Sets AVE and RMS of the non-zero parts of an array
C
C	IN	CH*(*)	input	Input Image
C	MASK	CH*(*)	input	MASK image for average determination
C	SAVE	REAL	input	Required AVE
C	SRMS	REAL	input	Required RMS
C	AMIN	REAL	input	Do not touch IN(ix,iy) for MASK(ix,iy) < AMIN
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 5 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, MASK
      REAL		SAVE, SRMS, AMIN
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ARRSETAV')
C
      INTEGER           NAX1, NAXIS1(SYSMXDIM)
      INTEGER           NAX2, NAXIS2(SYSMXDIM)
C
      INTEGER		ADD1, ADD2, I, NT
      CHARACTER*1	ATYPE1, ATYPE2
C
      DATA		NAXIS1 /SYSMXDIM * 1/
      DATA		NAXIS2 /SYSMXDIM * 1/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX1, NAXIS1, ATYPE1, ADD1)
      CALL DATGETAR (MASK, NAX2, NAXIS2, ATYPE2, ADD2)
C
      IF (ATYPE1 .NE. ATYPE2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Array types don''t match')
         GOTO 990
      ENDIF
      NT = 1
      DO 100 I = 1, MAX (NAX1, NAX2)
         IF (NAXIS1(I) .GT. 1  .OR.  NAXIS2(I) .GT. 1) THEN
            IF (NAXIS1(I) .NE. NAXIS2(I))  THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $            'Axes don''t match')
               GOTO 990
            ENDIF
         NT = NT * MAX (NAXIS1(1) , 1)
         ENDIF
 100  CONTINUE
C
      IF (ATYPE1 .EQ. 'R') THEN
C
C
C Set RMS to SRMS,  Set AVE to SAVE, only calculate AVE, RMS from
C the pixels greater than AMIN in MEMR(ADD2)
C
         CALL PIXRSETA (MEMR(ADD1), MEMR(ADD2), NT, SAVE, SRMS, AMIN)
C
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Coded only for REAL')
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





