C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modscale.f	1.2    6/2/93
C
      SUBROUTINE MODSCALE (MODEL, SCALE, SCMODEL)
C
CD Scale a model by SCALE (ie. 2 -> Double size)
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C
C	MODEL	CH*(*)	input	Name of directory entry
C       SCALE   REAL    input   Scale Factor
C	SCMODEL CH*(*)	input	Name of directory entry
C Audit trail:
C	Cloned from modrotat
C				R. G. Marson    Feb 12 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      CHARACTER*(*)	MODEL, SCMODEL
      REAL              SCALE
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODSCALE')
C
C Function definitions
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER                   DATADD
      LOGICAL                   DATEXIST
C
C Local Variables
C
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      INTEGER		NCOMP, FLADD, RAADD, DECADD, TADD
      INTEGER           BMAJADD, BMINADD, BPAADD
      INTEGER		SFLADD, SRAADD, SDECADD, STADD
      INTEGER           SBMAJADD, SBMINADD, SBPAADD
      CHARACTER*1	ATYPE
      LOGICAL           NEW
C
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get the important bits of the old model
C
      CALL DATGETAR (STRM2(MODEL, 'FLUX'), NAX, NAXIS, ATYPE,
     1   FLADD)
      NCOMP = NAXIS(1)
      RAADD = DATADD(STRM2(MODEL, 'RA'))
      DECADD = DATADD(STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD(STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD(STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD(STRM2(MODEL, 'BPA'))
      TADD = DATADD(STRM2(MODEL, 'TYPE'))
C
C Check if SCMODEL exists otherwise create it
C
      NEW = .TRUE.
      IF (DATEXIST(STRM2(SCMODEL, 'FLUX'))) THEN
         CALL DATGETAR (STRM2(MODEL, 'FLUX'), NAX, NAXIS, ATYPE,
     1        SFLADD)
         IF (NCOMP.LE.NAXIS(1)) NEW = .FALSE.
      END IF
      IF (NEW) THEN
         IF (DATEXIST(SCMODEL)) CALL DATDELET(SCMODEL)
         CALL DATCREAT(SCMODEL)
         NAXIS(1) = NCOMP
         CALL DATMAKAR(STRM2(SCMODEL, 'FLUX'), 1, NAXIS, 'R', SFLADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'RA'), 1, NAXIS, 'R', SRAADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'DEC'), 1, NAXIS, 'R', SDECADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'BMAJ'), 1, NAXIS, 'R', SBMAJADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'BMIN'), 1, NAXIS, 'R', SBMINADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'BPA'), 1, NAXIS, 'R', SBPAADD) 
         CALL DATMAKAR(STRM2(SCMODEL, 'TYPE'), 1, NAXIS, 'S', STADD) 
      END IF
C
C Copy the things that are invarient under scaling
C
      DO I = 0, NCOMP - 1
         MEMR(SFLADD+I) = MEMR(FLADD + I)
         MEMR(SBPAADD+I) = MEMR(BPAADD + I)
         MEMC(STADD+I) = MEMC(TADD + I)
      END DO
C
C Now call the routine that does the work (this is set a seperate subroutine
C  to allow a lower level entry point to the scaling subroutine)
C
      CALL MODSCAL(SCALE, NCOMP, 
     $     MEMR(RAADD), MEMR(DECADD), MEMR(BMAJADD), MEMR(BMINADD),  
     $     MEMR(SRAADD), MEMR(SDECADD), MEMR(SBMAJADD), MEMR(SBMINADD))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modscale.f	1.2    6/2/93
C
      SUBROUTINE MODSCAL (SCALE, NCOMP, RA, DEC, BMAJ, BMIN, 
     $     SRA, SDEC, SBMAJ, SBMIN)
C
CD Scale the relevant bits of a model
C
C
C Audit trail:
C	Cloned from modrot
C				R. G. Marson    Feb 12 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      INTEGER	NCOMP
      REAL	DEC(NCOMP), RA(NCOMP), BMAJ(NCOMP), BMIN(NCOMP), SCALE 
      REAL	SDEC(NCOMP), SRA(NCOMP), SBMAJ(NCOMP), SBMIN(NCOMP)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODSCAL')
C
C Local Variables
C
      INTEGER           I
C
C=====================================================================
      IF (ERROR) GO TO 999
C
C Start scaling
C
      DO I = 1, NCOMP
         SRA(I)  =   RA(I) * SCALE
         SDEC(I)  =  DEC(I) * SCALE
         SBMAJ(I) =  BMAJ(I) * SCALE
         SBMIN(I) = BMIN(I) * SCALE
      END DO
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
