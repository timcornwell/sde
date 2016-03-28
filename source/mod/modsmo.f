C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modsmo.f	1.1    12/3/94
C
      SUBROUTINE MODSMO (INMODEL, BEAM, OUTMODEL)
C
CD Smooth an analytic model with a beam
C
C	INMODEL	CH*(*)	input	Name of input model directory entry
C	BEAM	R(4)	input	Beam to be deconvolved
C	OUTMODEL CH*(*)	input	Name of output model directory entry
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C While SDE models can be of several types, this can only deal with 'POINT'
C and 'GAUSS'.  It will print a warning message and ignore anything else.
C It also doesn't handle time variable models.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Dec 2 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INMODEL, OUTMODEL
      REAL		BEAM(4)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODSMO')
C
      INTEGER		NCOMP, ICOMP, I,
     $   		IBMAJADD, IBMINADD, IBPAADD, ITADD,
     $   		OBMAJADD, OBMINADD, OBPAADD, OTADD
      CHARACTER*1	ATYPE
      REAL		COMP(4), SMOCOMP(4)
C
      LOGICAL		DATEXIST
      INTEGER		DATADD
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
C Create output, if it doesn't already exist
C
      IF (.NOT.DATEXIST(OUTMODEL)) THEN
         CALL DATCREAT(OUTMODEL)
         CALL ARRCOPY (STRM2(INMODEL,'FLUX'),STRM2(OUTMODEL,'FLUX'))
         CALL ARRCOPY (STRM2(INMODEL,'RA'),STRM2(OUTMODEL,'RA'))
         CALL ARRCOPY (STRM2(INMODEL,'DEC'),STRM2(OUTMODEL,'DEC'))
         CALL ARRCOPY (STRM2(INMODEL,'BMAJ'),STRM2(OUTMODEL,'BMAJ'))
         CALL ARRCOPY (STRM2(INMODEL,'BMIN'),STRM2(OUTMODEL,'BMIN'))
         CALL ARRCOPY (STRM2(INMODEL,'BPA'),STRM2(OUTMODEL,'BPA'))
         CALL ARRCOPY (STRM2(INMODEL,'TYPE'),STRM2(OUTMODEL,'TYPE'))
      END IF
C
C Find number of components
C
      CALL DATGETAR (STRM2(INMODEL, 'BMAJ'), I, NCOMP, ATYPE, 
     1   IBMAJADD)
      IBMINADD = DATADD (STRM2(INMODEL, 'BMIN'))
      IBPAADD = DATADD (STRM2(INMODEL, 'BPA'))
      ITADD = DATADD (STRM2(INMODEL, 'TYPE'))
      OBMAJADD = DATADD (STRM2(OUTMODEL, 'BMAJ'))
      OBMINADD = DATADD (STRM2(OUTMODEL, 'BMIN'))
      OBPAADD = DATADD (STRM2(OUTMODEL, 'BPA'))
      OTADD = DATADD (STRM2(OUTMODEL, 'TYPE'))
C
      DO 100 ICOMP = 1, NCOMP
         I = ICOMP - 1
         IF (MEMC(ITADD+I)(1:4).EQ.'POIN') THEN
            MEMR(OBMAJADD+I) = BEAM(1)
            MEMR(OBMINADD+I) = BEAM(2)
            MEMR(OBPAADD+I) = BEAM(3)
            MEMC(OTADD+I) = 'GAUSS'
         ELSE IF (MEMC(ITADD+I)(1:4).EQ.'GAUS') THEN
            COMP(1) = MEMR(IBMAJADD+I)
            COMP(2) = MEMR(IBMINADD+I)
            COMP(3) = MEMR(IBPAADD+I)
            COMP(4) = 0.0
            CALL UTLGCONV(COMP, BEAM, SMOCOMP)
            MEMR(OBMAJADD+I) = SMOCOMP(1)
            MEMR(OBMINADD+I) = SMOCOMP(2)
            MEMR(OBPAADD+I) = SMOCOMP(3)
         ELSE
            MESSAGE = 'Ignoring component of type ' // MEMC(ITADD+I)
            CALL MSGPUT (MESSAGE, 'W')
         END IF
  100 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
