C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgidelt.f	1.1    2/8/95
C
      SUBROUTINE IMGIDELT (IM1, IM2, IDELT)
C
CD Finds the integer shift necessary to align images
C 
C	IM1	CH*(*)	input	First Image
C	IM2	CH*(*)	input	Second Image
C	IDELT	INT(*)	output	Integer shift
C
C Pixel I+IDELT of the first image is the same position on the sky as 
C  pixel I in the second image.  Normally called with IM1 being a
C  mask which has been trimmed with IMGSTRIM, and IM2 a full sized image.
C  Can also be used with two masks.  Because of the damned pseudo axis
C  problem, we can't check axis sizes sensibly here.
C
C Audit trail:
C	If this routine returns successfully, the images can be aligned
C	as described above.
C				D.S.Briggs	Jan 22 1995
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	IM1, IM2
      INTEGER		IDELT(SYSMXDIM)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGIDELT')
C
      REAL		TOL
      PARAMETER		(TOL=.001)
C
      INTEGER           NAX1, NAX2, NAXIS1(SYSMXDIM), NAXIS2(SYSMXDIM)
      DOUBLE PRECISION  RVAL1(SYSMXDIM), RVAL2(SYSMXDIM)
      REAL              RPIX1(SYSMXDIM), RPIX2(SYSMXDIM),
     $   		DELT1(SYSMXDIM), DELT2(SYSMXDIM),
     $   		ROTA1(SYSMXDIM), ROTA2(SYSMXDIM),
     $   		ABSTOL, AVGDELT
      INTEGER		ADD1, ADD2, RNAX, RNAX1, RNAX2, I
      CHARACTER*1       T1, T2
      CHARACTER*8	TYPE1(SYSMXDIM), TYPE2(SYSMXDIM)
      DOUBLE PRECISION	RVDIFF
C
      INTEGER		CRDRNAX
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IM1, NAX1, NAXIS1, T1, ADD1)
      CALL DATGETAR (IM2, NAX2, NAXIS2, T2, ADD2)
      CALL CRDGET (IM1, NAX1, TYPE1, NAXIS1, RVAL1, RPIX1, DELT1,
     1   ROTA1)
      CALL CRDGET (IM2, NAX2, TYPE2, NAXIS2, RVAL2, RPIX2, DELT2,
     1   ROTA2)
C
      RNAX1 = CRDRNAX(NAX1, NAXIS1)
      RNAX2 = CRDRNAX(NAX2, NAXIS2)
      RNAX = MAX(RNAX1,RNAX2)
C
      DO 100 I = 1, RNAX
         ABSTOL = (ABS(DELT1(I)) + ABS(DELT2(I)))/2.0
         AVGDELT = (DELT1(I) + DELT2(I)) / 2.0
         IF (ABS(DELT1(I)-DELT2(I)) .GT. ABSTOL) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Deltas don''t match')
            GO TO 999
         END IF
         IF (ABS(RVAL1(I)-RVAL2(I)) .GT. ABSTOL) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Refpix don''t match')
            GO TO 999
         END IF
         IDELT(I) = NINT((RVAL2(I) - RVAL1(I))/AVGDELT -
     $      (RPIX2(I) - RPIX1(I)))
 100  CONTINUE
C
      DO 200 I = RNAX+1, SYSMXDIM
         IDELT(I) = 0
 200  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
