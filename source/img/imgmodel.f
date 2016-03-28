C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmodel.f	1.4	 2/22/91
C
      SUBROUTINE IMGMODEL (IMG, NCOMP, FLUX, RA, DEC, BMAJ, BMIN, BPA,
     $   TYPE)
C
CD Put a specified model into an image
C
C
C	IMG	CH*(*)	input	Name of image
C	FLUX	REAL	input	Flux of component in Jy
C	RA	REAL	input	Position of component in Ra offset (asec)
C	DEC	REAL	input	input	Position of component in Dec offset
C	BMAJ	REAL	input	Major axis in asec
C	BMIN	REAL	input	Minor axis in asec
C	BPA	REAL	input	Position angle in degrees
C	TYPE	CHAR	input	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
CC Audit trail:
C	Fixed Bugs: **ADD in MEMR(**ADD) was not being incremented,
C	RAADD was used in the place of DECADD
C				M.A.Holdaway	Feb 22 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, TYPE(*)
      INTEGER		NCOMP
      REAL		FLUX(*), RA(*), DEC(*), BMAJ(*), BMIN(*),
     $			BPA(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMODEL')
C
      CHARACTER*(SYSMXNAM)	MODEL, STRM2
      INTEGER		ICOMP, FLADD, RAADD, DECADD, 
     1			TADD, BMAJADD, BMINADD, BPAADD
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      MODEL = STRM2(IMG,'MODEL')
      CALL DATCREAT (MODEL)
      CALL DATMAKAR (STRM2(MODEL, 'FLUX'), 1, NCOMP, 'R', FLADD)
      CALL DATMAKAR (STRM2(MODEL, 'RA'), 1, NCOMP, 'R', RAADD)
      CALL DATMAKAR (STRM2(MODEL, 'DEC'), 1, NCOMP, 'R', DECADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMIN'), 1, NCOMP, 'R', BMINADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'TYPE'), 1, NCOMP, 'S', TADD)
C
C Now fill in model
C
      DO 10 ICOMP = 1, NCOMP
         MEMR(FLADD + ICOMP - 1) =  FLUX(ICOMP)
         MEMR(RAADD + ICOMP - 1) = RA(ICOMP)
         MEMR(DECADD + ICOMP - 1) = DEC(ICOMP)
         MEMR(BMAJADD + ICOMP - 1) = BMAJ(ICOMP)
         MEMR(BMINADD + ICOMP - 1) = BMIN(ICOMP)
         MEMR(BPAADD + ICOMP - 1) = BPA(ICOMP)
         MEMC(TADD + ICOMP - 1) = TYPE(ICOMP)
 10   CONTINUE
C
      CALL MODIMG (STRM2(IMG, 'MODEL'), IMG)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
