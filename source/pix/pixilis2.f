C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixilis2.f	1.2    7/18/97
C
      SUBROUTINE PIXILIS2 (ARRAY, NAX, NAXIS, WHAT, IMIN, IMAX,
     $   ISKIP, DORANGE, DOEXCLD, RMIN, RMAX)
C
CD List out arrays on MSGPUT  (Many argument version)
C
C	ARRAY	INT(*)	input	array.  (1D, indexed by hand)
C	NAX	INT	input	number of axes in array
C	NAXIS	INT(*)	input	number of pixels along each axis
C	WHAT	CH*(*)	input	identifying comment
C	IMIN	INT(*)	input	minimum pixel along each axis
C	IMAX	INT(*)	input	maximum pixel along each axis
C	ISKIP	INT(*)	input	increment along each axis
C	DORANGE	LOG	input	RMIN, RMAX are ignored if false
C	DOEXCLD LOG	input	reverse sense of RMIN & RMAX
C	RMIN	INT	input	only values >= RMIN
C	RMAX	INT	input	 and <= RMAX are printed
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	17-Sept-1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		ARRAY(*)
      CHARACTER*(*)	WHAT
      INTEGER		NAX
      INTEGER		NAXIS(*), IMIN(*), IMAX(*), ISKIP(*)
      LOGICAL		DORANGE, DOEXCLD
      INTEGER		RMIN, RMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXILIS2')
C
      INTEGER		IPIX(SYSMXDIM), I, J, K, TOTAL, IADD
      INTEGER		IVAL
      LOGICAL		DOPOINT
C
      INTEGER		STRLEN
      CHARACTER*(SYSMXNAM)	STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Listing ARRAY: '//WHAT, 'L')
C
      TOTAL = 1
      DO 100 I = 1, NAX
         TOTAL = TOTAL * ((IMAX(I) - IMIN(I)) / ISKIP(I) + 1)
         IPIX(I) = IMIN(I)
 100  CONTINUE
C
      DO 500 I = 1, TOTAL
C
         IADD = IPIX(1)
         J = 1
         DO 200 K = 2, NAX
            J = J * NAXIS(K-1)
            IADD = IADD + (IPIX(K)-1) * J
 200     CONTINUE
C
         DOPOINT = .TRUE.
         IVAL = ARRAY(IADD)
         IF (DORANGE) THEN
            IF (DOEXCLD) THEN
               IF ((IVAL.LE.RMAX).AND.(IVAL.GE.RMIN)) DOPOINT = .FALSE.
            ELSE
               IF ((IVAL.GT.RMAX).OR.(IVAL.LT.RMIN)) DOPOINT = .FALSE.
            END IF
         END IF
C
         IF (DOPOINT) THEN
            MESSAGE = '(' // STRINT(IPIX(1))
            DO 300 J = 2, NAX
               CALL STRAPNB2 (MESSAGE, ',', STRINT(IPIX(J)))
 300        CONTINUE
            CALL STRAPPEN (MESSAGE, ')')
C
            K = STRLEN(MESSAGE) + 2
            WRITE (MESSAGE(K:), 310) IVAL
 310        FORMAT (I10)
            CALL MSGPUT (MESSAGE, 'L')
         END IF
C
         IPIX(1) = IPIX(1) + ISKIP(1)
         DO 400 J = 1, NAX-1
            IF (IPIX(J).GT.IMAX(J)) THEN
               IPIX(J) = IMIN(J)
               IPIX(J+1) = IPIX(J+1) + ISKIP(J+1)
            END IF
 400     CONTINUE
C
 500  CONTINUE
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
