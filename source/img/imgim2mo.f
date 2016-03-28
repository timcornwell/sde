C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgim2mo.f	1.1    7/15/93
C
      SUBROUTINE IMGIM2MO (IMG, MASK, MODEL)
C
CD Put image pixels into SDE model
C
C	IMG	CH*(*)	input	Name of image
C	MASK	CH*(*)	input	Name of mask image
C	MODEL	CH*(*)	input	Name of SDE model
CC Audit trail:
C	Original version
C				D.S.Briggs	13 July 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, MASK, MODEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGIM2MO')
C
      CHARACTER*1	ATYPE
      INTEGER		ICOMP, NCOMP, NAX, NAXIS(SYSMXDIM),
     $   		IADD, MADD, RNAX, FLADD, RAADD, DECADD,
     $   		BMAJADD, BMINADD, BPAADD, TADD, NDUMMY
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		CRDRNAX, DATADD
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (DATEXIST(MODEL)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Model must not exist')
         GO TO 999
      END IF
C
      CALL CRDCHECK (IMG, MASK)
      IF (ERROR) GO TO 990
C
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, IADD)
      MADD = DATADD (MASK)
      RNAX = CRDRNAX (NAX, NAXIS)
C
      CALL ARRMULT (IMG, MASK, 'Im2MoTmp')
      CALL ARRNSPRT ('Im2MoTmp', NCOMP)
      CALL DATDELET ('Im2MoTmp')
      IF (ERROR) GO TO 999
C
      CALL DATGETR (IMG, 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 999
C
      CALL DATCREAT (MODEL)
      CALL DATMAKAR (STRM2(MODEL, 'FLUX'), 1, NCOMP, 'R', FLADD)
      CALL DATMAKAR (STRM2(MODEL, 'RA'), 1, NCOMP, 'R', RAADD)
      CALL DATMAKAR (STRM2(MODEL, 'DEC'), 1, NCOMP, 'R', DECADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMIN'), 1, NCOMP, 'R', BMINADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'TYPE'), 1, NCOMP, 'S', TADD)
C
C Fill in the bits that we know about
C
      DO 100 ICOMP = 1, NCOMP
         MEMR(BMAJADD+ICOMP-1) = 0.0
         MEMR(BMINADD+ICOMP-1) = 0.0
         MEMR(BPAADD+ICOMP-1) = 0.0
         MEMC(TADD+ICOMP-1) = 'POINT'
 100  CONTINUE
C
C Switch to the pixel level routines
C
      IF ((ATYPE.EQ.'R').AND.(RNAX.EQ.2)) THEN
         CALL PIX2DRMO (MEMR(IADD), MEMR(MADD), NAXIS(1), NAXIS(2),
     $      RPIX(1), RPIX(2), DELT(1)*3600.0, DELT(2)*3600.0,
     $      NCOMP, MEMR(FLADD), MEMR(RAADD), MEMR(DECADD))
      ELSE
         WRITE (MESSAGE, 1000) ATYPE, RNAX
 1000    FORMAT ('Type = ',A,' and RNAX = ',I,' not implemented')
         CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
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
