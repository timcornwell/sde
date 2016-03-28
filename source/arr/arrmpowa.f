C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrmpowa.f	1.2	 5/29/91
C
      SUBROUTINE ARRMPOWA (A, INNER, MIDDLE, OUTER, 
     $   POWER1, POWER2, AVX, AVY, BCEN, B)
C
CD Taper an array with a power law function
C
C
C	A	CH*(*)	input	Directory name of array
C	INNER	REAL	input	Inner scale for power law (pixels (X pixels))
C	OUTER	REAL	input	Outer scale for power law (pixels (X pixels))
C	POWER1	REAL	input	power from INNER to MIDDLE	
C	POWER2	REAL	input	power from MIDDLE to OUTER
C	AVX	REAL	input	Averaging distance X (image space units)
C	AVY	REAL	input	Averaging distance Y (image space units)
C	BCEN	INT(*)	input	Center of taper
C	B	CH*(*)	input	Directory name of tapered array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 15 1991
C	Revamped pre-averaging to properly deal with the velocity
C				M.A.Holdaway	May 29 1991
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B
      REAL		INNER, MIDDLE, OUTER, POWER1, 
     $   		POWER2, AVX, AVY
      INTEGER		BCEN (*)
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRMPOWA')
C
C Local variables
C
      REAL		DELT(SYSMXDIM), RADMAX
      CHARACTER*(1) 	ATYPE, BTYPE
      INTEGER 		ANAX, ANAXIS(SYSMXDIM), 
     1			BNAX, BNAXIS(SYSMXDIM)
      INTEGER 		AADD, BADD, IAX, NREAL, NDUMMY, NSINC, SADD
      LOGICAL		DATEXIAR
      DATA 		ANAXIS	/SYSMXDIM*1/
      DATA 		BNAXIS	/SYSMXDIM*1/
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
C Make SINC function
C
      RADMAX = 20.0
      CALL ARRSINC (NSINC, SADD, RADMAX)
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF ((ATYPE.EQ.'X').AND.(NREAL.EQ.2)) THEN
         CALL DATGETR (A, 'CDELT', DELT, SYSMXDIM, NDUMMY)
         CALL PIX2DPXA (MEMX(AADD), ANAXIS(1), ANAXIS(2), DELT(1),
     $      DELT(2), INNER, MIDDLE, OUTER, POWER1, POWER2,
     $      AVX, AVY, NSINC, MEMR(SADD), RADMAX, BCEN(1), BCEN(2), 
     $      MEMX(BADD))
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
