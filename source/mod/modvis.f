C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modvis.f	1.2    11/21/94
C
      SUBROUTINE MODVIS (MODEL, IMG, VIS, SUB)
C
CD Fill a visibility data set with the transform of a model
C
C	MODEL/FLUX	REAL(*)	Flux of component in Jy
C	MODEL/RA	REAL(*)	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL(*)	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL(*)	Major axis in asec
C	MODEL/BMIN	REAL(*)	Minor axis in asec
C	MODEL/BPA	REAL(*)	Position angle in degrees
C	MODEL/TYPE	CHAR(*)	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	MODEL/VEL	REAL(*)	velocity magnitude in mas/day
C	MODEL/VPA	REAL(*)	velocity position angle in degrees
C	MODEL/TIMEREF	REAL(*)	time reference for model in days
C
C	IMG	CH*(*)	input	Coordinate information for MODEL
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 28 1994
C	Added time variable models
C				D.S.Briggs	Nov 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, IMG, VIS, SUB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODVIS')
C
      INTEGER		MNAX, MNAXIS(SYSMXDIM), VNAX, VNAXIS(SYSMXDIM),
     $   		INAX, INAXIS, IADD
      INTEGER		NCOMP, FLADD, RAADD, DECADD, TYPADD, TIMADD,
     1			BMAJADD, BMINADD, BPAADD
      CHARACTER*1	MATYPE, VATYPE, IATYPE
C
      LOGICAL		DOTIMEV
      REAL		DELT(SYSMXDIM), TIMEREF
      INTEGER		NDUMMY
      DOUBLE PRECISION	SHIFT(3,3)
      INTEGER		VISADD, WTADD, UADD, VADD, WADD, NVIS,
     $   		VELADD, VPAADD, RNAX
      CHARACTER*(SYSMXNAM)	SVIS
C
      LOGICAL		DATEXIST
      INTEGER		DATADD, CRDRNAX
      REAL		DATFGETR
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
C Find shift required to align data
C
      CALL DATGETAR (IMG, INAX, INAXIS, IATYPE, IADD)
      RNAX = CRDRNAX (INAX, INAXIS)
      SVIS = STRM2 (VIS, SUB)
      CALL CRDSHIFT (SVIS, IMG, SHIFT)
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
C
C Get the model information
C
      CALL DATGETAR (STRM2(MODEL, 'FLUX'), MNAX, MNAXIS, MATYPE,
     1   FLADD)
      NCOMP = MNAXIS(1)
      RAADD = DATADD(STRM2(MODEL, 'RA'))
      DECADD = DATADD(STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD(STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD(STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD(STRM2(MODEL, 'BPA'))
      CALL DATGETAR (STRM2(MODEL, 'TYPE'), MNAX, MNAXIS, MATYPE,
     1   TYPADD)
      DOTIMEV = .FALSE.
      IF (DATEXIST(STRM2(MODEL, 'VEL'))) THEN
         DOTIMEV = .TRUE.
         VELADD = DATADD (STRM2(MODEL, 'VEL'))
         VPAADD = DATADD (STRM2(MODEL, 'VPA'))
         TIMEREF = 0.0
         IF (DATEXIST(STRM2(MODEL,'TIMEREF'))) THEN
            TIMEREF = DATFGETR (MODEL, 'TIMEREF')
         END IF
         CALL MSGPUT ('Using time variable model','I')
         WRITE (MESSAGE, 1000) TIMEREF
 1000    FORMAT ('Time reference is',F8.3,' days')
         CALL MSGPUT (MESSAGE,'I')
      ELSE
         CALL MSGPUT ('Using time static model','I')
      END IF
C
C Find the visibility data
C
      CALL DATGETAR (STRM2(SVIS, 'VIS'), VNAX, VNAXIS, VATYPE,
     1   VISADD)
      NVIS = VNAXIS(1)
      WTADD = DATADD (STRM2(SVIS, 'WT'))
      UADD = DATADD (STRM2(VIS, 'UU'))
      VADD = DATADD (STRM2(VIS, 'VV'))
      WADD = DATADD (STRM2(VIS, 'WW'))
      TIMADD = DATADD (STRM2(VIS,'TIME'))
C
      IF (RNAX.EQ.2) THEN
         CALL MODVIS2D (NCOMP, MEMR(FLADD), MEMR(RAADD), MEMR(DECADD),
     $      MEMR(BMAJADD), MEMR(BMINADD), MEMR(BPAADD), MEMC(TYPADD),
     $	    DOTIMEV, TIMEREF, MEMR(VELADD), MEMR(VPAADD),
     $      DELT(1), MEMX(VISADD), MEMR(WTADD), MEMR(UADD), MEMR(VADD),
     $      MEMR(WADD), MEMR(TIMADD), NVIS, SHIFT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only support 2 real dimensions')
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
