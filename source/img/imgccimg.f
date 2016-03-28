C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgccimg.f	1.7    6/10/93
C
      SUBROUTINE IMGCCIMG (CCLEAN)
C
CD Insert a clean component list into the corrseponding image.
C The clean component lists are attached to the image.
C
C	CCLEAN	CH*(*)	input	Name of CCLEAN image
C
C Audit trail:
C	New subroutine
C				T.J.Cornwell	Feb 15 1989
C	Initialize image to zero
C				T.J.Cornwell	Feb 16 1989
C	Start from BITER
C				T.J.Cornwell	April 17 1991
C	Introduce field numbers
C				T.J.Cornwell	July 16 1992
C	Supply default field numbers if not provided in image
C				D.S.Briggs	Aug 17 1992
C	Support for 1-D images added
C				D.S.Briggs	Oct 21 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	CCLEAN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCCIMG')
C
      INTEGER		NITER, BITER, FN
      CHARACTER*1	CATYPE
      INTEGER		CNAX, CNAXIS(SYSMXDIM), CADD
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, CFLADD
      INTEGER		NREAL, NDUMMY, NAX, NAXIS(SYSMXDIM)
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		CRDRNAX
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
      IF (DATEXIST(STRM2(CCLEAN, 'BITER'))) THEN
         CALL DATGETI(CCLEAN, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'NITER'))) THEN
         CALL DATGETI(CCLEAN, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 0
      END IF
      IF (DATEXIST(STRM2(CCLEAN, 'FIELD'))) THEN
         CALL DATGETI(CCLEAN, 'FIELD', FN, 1, NDUMMY)
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
      NREAL = CRDRNAX(CNAX, CNAXIS)
C
C Get locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCLEAN, 'PIXLIST'))) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No clean components')
         GO TO 999
      ELSE
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         IF (DATEXIST(STRM2(CCLEAN, 'PIXFIELD'))) THEN
            CALL DATGETAR (STRM2(CCLEAN, 'PIXFIELD'), NAX, NAXIS,
     $         ATYPE, CFLADD)
         ELSE
            CALL DATMAKAR (STRM2(CCLEAN,'PIXFIELD'), NAX, NAXIS,
     $         'I', CFLADD)
            CALL ARRSETCO (STRM2(CCLEAN,'PIXFIELD'), 0.0, 0.0)
         END IF
         CALL DATGETAR (STRM2(CCLEAN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
      IF (NITER.EQ.0) NITER = NAXIS(1)
C
C Finally do something
C
      IF (NREAL.EQ.1) THEN
         CALL PIX1DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $      MEMI(CXLADD), BITER, NITER, MEMR(CADD), CNAXIS(1))
      ELSE IF (NREAL.EQ.2) THEN
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
