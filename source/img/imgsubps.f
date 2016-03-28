C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsubps.f	1.7    5/29/94
C
      SUBROUTINE IMGSUBPS (A, B, BP)
C
CD Subsection a PSF for use in BGC Clean, fix up the header
C
C
C	A	CH*(*)	input	Directory name of image
C	B	CH*(*)	input	Directory name of image
C	BP	INT	input	Size of beampatch
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Correct calling for new PIXRSUBS
C                               R. G. Marson    Feb 2 1993
C	Set output array to zero before calling new PIXRSUBS
C				T.J.Cornwell	Feb 21 1993
C	Bugfix.  Uninitialized NAXIS occasionally not caught by
C	min/max sanity checks.
C				D.S.Briggs	May 25 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	A, B
      INTEGER		BP
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='IMGSUBPS')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE, BTYPE
      INTEGER		BLC (SYSMXDIM), TRC (SYSMXDIM), 
     1			STEP (SYSMXDIM), SUM(SYSMXDIM)
      INTEGER 		AADD, ANAX, ANAXIS(SYSMXDIM), 
     1			BADD, BNAX, BNAXIS(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), EXTSIDE
      INTEGER 		IAX, NDUMMY
      LOGICAL		DATEXIAR
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETAR (A, ANAX, ANAXIS, ATYPE, AADD)
      CALL DATGETR (A, 'CDELT', DELT, ANAX, NDUMMY)
      CALL DATGETR (A, 'CRPIX', RPIX, ANAX, NDUMMY)
      IF (ERROR) GO TO 990
      DO 5 IAX = 1, ANAX
         ANAXIS (IAX) = MAX(1, ANAXIS(IAX))
         BLC(IAX) = MAX(1, NINT(RPIX(IAX))-BP/2+1)
         TRC(IAX) = MIN(ANAXIS(IAX), NINT(RPIX(IAX))+BP/2)
         STEP(IAX) = 1
 5    CONTINUE
C
C Get B
C
      IF (DATEXIAR (B)) THEN
         CALL DATGETAR (B, BNAX, BNAXIS, BTYPE, BADD)
         IF(ERROR) GO TO 990
         IF (ATYPE.NE.BTYPE) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     1         'Array types must be the same')
         END IF
      ELSE
         CALL DATCREAT (B)
         DO 30 IAX = 1, ANAX
            BNAXIS(IAX) = (TRC(IAX) - BLC(IAX) + 1)/STEP(IAX)
  30     CONTINUE
         CALL DATMAKAR (B, ANAX, BNAXIS, ATYPE, BADD)
      END IF
C
C Add dummy values for SUM & STEP
C
      DO 33 IAX = 1, SYSMXDIM
         STEP(IAX) = 1
         SUM(IAX) = 1
 33   CONTINUE
      CALL ARRSETCO (B, 0.0, 0.0)
C
C For the benefit of pixel routines which expect all axis info to be valid
C
      DO 50 IAX = ANAX+1, SYSMXDIM
         ANAXIS(IAX) = 1
         BNAXIS(IAX) = 1
         BLC(IAX) = 1
         TRC(IAX) = 1
 50   CONTINUE

C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRSUBS (MEMR(AADD), MEMR(BADD), ANAXIS(1), 
     1      ANAXIS(2), ANAXIS(3), ANAXIS(4), ANAXIS(5), ANAXIS(6), 
     1      ANAXIS(7), BNAXIS(1), BNAXIS(2), BNAXIS(3),
     1      BNAXIS(4), BNAXIS(5), BNAXIS(6), BNAXIS(7), 
     1      BLC, TRC, STEP, SUM)
         CALL PIXREXTS (MEMR(AADD), ANAXIS(1), ANAXIS(2), ANAXIS(3),
     1      ANAXIS(4), ANAXIS(5), ANAXIS(6), ANAXIS(7), BLC, TRC, 
     2      EXTSIDE)
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'Cannot sensibly subsection array')
         GO TO 999
      END IF
C
C Copy header
C
      CALL HEDCOPY (A, B)
C
C Fix increment and reference pixel
C
      DO 20 IAX = 1, ANAX
         RPIX (IAX) = RPIX(IAX) - BLC(IAX) + 1.0
  20  CONTINUE
      CALL DATPUTR (B, 'CDELT', DELT, ANAX)
      CALL DATPUTR (B, 'CRPIX', RPIX, ANAX)
C
C Add exterior sidelobe level
C
      CALL DATPUTR(B, 'EXTSIDE', EXTSIDE, 1)
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
