C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flyccimg.f	1.2 12/29/92
C
      SUBROUTINE FLYCCIMG (CCLEAN, CCL)
C
CD Insert a clean component list into the corrseponding image.
C The clean component lists are separate.
C
C
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C
C Audit trail:
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	CCLEAN, CCL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYCCIMG')
C
      INTEGER		NITER, BITER, FN
      CHARACTER*1	CATYPE
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, CFLADD
      INTEGER		IAX, NREAL, NDUMMY, NAX, NAXIS(SYSMXDIM)
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Set initial image to be blank
C
      CALL ARRSETCO (CCLEAN, 0.0, 0.0)
C
C Get number of components
C
      IF (DATEXIST(STRM2(CCL, 'BITER'))) THEN
         CALL DATGETI(CCL, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
      END IF
      IF (DATEXIST(STRM2(CCL, 'NITER'))) THEN
         CALL DATGETI(CCL, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 0
      END IF
      IF (DATEXIST(STRM2(CCL, 'FIELD'))) THEN
         CALL DATGETI(CCL, 'FIELD', FN, 1, NDUMMY)
      ELSE
         FN = 0
      END IF
C
      CALL DATGETAR (CCLEAN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
      NREAL = 0
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Get locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCL, 'PIXLIST'))) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No clean components')
         GO TO 999
      ELSE
         CALL DATGETAR (STRM2(CCL, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         IF (DATEXIST(STRM2(CCL, 'PIXFIELD'))) THEN
            CALL DATGETAR (STRM2(CCL, 'PIXFIELD'), NAX, NAXIS,
     $         ATYPE, CFLADD)
         ELSE
            CALL DATMAKAR (STRM2(CCL,'PIXFIELD'), NAX, NAXIS,
     $         'I', CFLADD)
            CALL ARRSETCO (STRM2(CCL,'PIXFIELD'), 0.0, 0.0)
         END IF
         CALL DATGETAR (STRM2(CCL, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
      IF (NITER.EQ.0) NITER = NAXIS(1)
C
C Finally do something
C
      IF (NREAL.EQ.2) THEN
         CALL PIX2DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $      MEMI(CXLADD), MEMI(CYLADD), 
     1      BITER, NITER, MEMR(CADD), CNAXIS(1), CNAXIS(2))
      ELSE IF (NREAL.EQ.3) THEN
         CALL PIX3DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $      MEMI(CXLADD), MEMI(CYLADD), 
     1      MEMI(CZLADD), BITER, NITER, MEMR(CADD), CNAXIS(1), 
     2      CNAXIS(2), CNAXIS(3))
      ELSE
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
