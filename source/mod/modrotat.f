C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modrotat.f	1.2    6/2/93
C
      SUBROUTINE MODROTAT (MODEL, ANGLE, ROTMODEL)
C
CD Rotate a model by angle degrees clockwise
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
C       ANGLE   REAL    input   number of degrees to rotate by
C	ROTMODEL CH*(*)	input	Name of directory entry
C Audit trail:
C	Cloned from modimg
C				R. G. Marson    Feb 12 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      CHARACTER*(*)	MODEL, ROTMODEL
      REAL              ANGLE
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODROTAT')
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
      INTEGER		RFLADD, RRAADD, RDECADD, RTADD
      INTEGER           RBMAJADD, RBMINADD, RBPAADD
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
C Check if ROTMODEL exists otherwise create it
C
      NEW = .TRUE.
      IF (DATEXIST(STRM2(ROTMODEL, 'FLUX'))) THEN
         CALL DATGETAR (STRM2(MODEL, 'FLUX'), NAX, NAXIS, ATYPE,
     1        RFLADD)
         IF (NCOMP.LE.NAXIS(1)) NEW = .FALSE.
      END IF
      IF (NEW) THEN
         IF (DATEXIST(ROTMODEL)) CALL DATDELET(ROTMODEL)
         CALL DATCREAT(ROTMODEL)
         NAXIS(1) = NCOMP
         CALL DATMAKAR(STRM2(ROTMODEL, 'FLUX'), 1, NAXIS, 'R', RFLADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'RA'), 1, NAXIS, 'R', RRAADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'DEC'), 1, NAXIS, 'R', RDECADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'BMAJ'), 1, NAXIS, 'R', RBMAJADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'BMIN'), 1, NAXIS, 'R', RBMINADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'BPA'), 1, NAXIS, 'R', RBPAADD) 
         CALL DATMAKAR(STRM2(ROTMODEL, 'TYPE'), 1, NAXIS, 'S', RTADD) 
      END IF
C
C Copy the things that are invarient under rotation
C
      DO I = 0, NCOMP - 1
         MEMR(RFLADD+I) = MEMR(FLADD + I)
         MEMR(RBMAJADD+I) = MEMR(BMAJADD + I)
         MEMR(RBMINADD+I) = MEMR(BMINADD + I)
         MEMC(RTADD+I) = MEMC(TADD + I)
      END DO
C
C Now call the routine that does the work (this is set a seperate subroutine
C  to allow a lower level entry point to the rotation subroutine)
C
      CALL MODROT(ANGLE, NCOMP, MEMR(RAADD), MEMR(DECADD), MEMR(BPAADD), 
     $     MEMR(RRAADD), MEMR(RDECADD), MEMR(RBPAADD))
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
C @(#)modrotat.f	1.2    6/2/93
C
      SUBROUTINE MODROT (ANGLE, NCOMP, RA, DEC, PA, RRA, RDEC, RPA)
C
CD Rotate the relevant bits of a model
C
C
C Audit trail:
C	Cloned from modimg1d
C				R. G. Marson    Feb 12 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      INTEGER		NCOMP
      REAL		DEC(NCOMP), RA(NCOMP), PA(NCOMP), ANGLE
      REAL		RDEC(NCOMP), RRA(NCOMP), RPA(NCOMP)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODROT')
      REAL	PI
      PARAMETER	(PI=3.14159265358979323846)
C
C Local Variables
C
      REAL		CA, SA
      INTEGER           I
C
C=====================================================================
      IF (ERROR) GO TO 999
C
C Convert ANGLE to sine, cos
C
      CA = COS(PI*ANGLE/180.)
      SA = SIN(PI*ANGLE/180.)
C
C Start rotating
C
      DO I = 1, NCOMP
         RRA(I)  =   RA(I) * CA - DEC(I) * SA
         RDEC(I) =   RA(I) * SA + DEC(I) * CA
         RPA(I)  = PA(I) - ANGLE
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
