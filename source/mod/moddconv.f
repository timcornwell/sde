C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moddconv.f	1.1	 6/7/93
C
      SUBROUTINE MODDCONV (INMODEL, BEAM, OUTMODEL)
C
CD Deconvolve a BEAM from a model
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
C While SDE models can be of several types, the only one this can deal
C with is GAUSS.  It will print a warning message and ignore anything else.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 23 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INMODEL, OUTMODEL
      REAL		BEAM(4)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODDCONV')
C
      INTEGER		NCOMP, ICOMP, I,
     $   		IBMAJADD, IBMINADD, IBPAADD, ITADD,
     $   		OBMAJADD, OBMINADD, OBPAADD, OTADD
      CHARACTER*1	ATYPE
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
         IF (MEMC(ITADD+I)(1:4).EQ.'GAUS') THEN
            CALL DECONV (MEMR(IBMAJADD+1), MEMR(IBMINADD+I),
     $         MEMR(IBPAADD+I), BEAM(1), BEAM(2), BEAM(3),
     $         MEMR(OBMAJADD+I), MEMR(OBMINADD+I), MEMR(OBPAADD+I))
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
C
      SUBROUTINE DECONV (FMAJ, FMIN, FPA, CMAJ, CMIN, CPA, RMAJ, RMIN,
     *   RPA)
C-----------------------------------------------------------------------
C! deconvolves two gaussians.  Ripped off nearly wholesale from AIPS1
C# Modeling
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   DECONV deconvolves a gaussian "beam" from a gaussian component.
C   Inputs:
C        FMAJ     R    Fitted major axis
C        FMIN     R    Fitted minor axis
C        FPA      R    Fitted position angle of major axis
C        CMAJ     R    Point source major axis
C        CMIN     R    Point source minor axis
C        CPA      R    Point source position angle of major axis
C   Outputs:
C        RMAJ     R    Real major axis
C        RMIN     R    Real minor axis
C        RPA      R    Real position angle of major axis
C-----------------------------------------------------------------------
      REAL      FMAJ, FMIN, FPA, CMAJ, CMIN, CPA, RMAJ, RMIN, RPA
C
      REAL      CMJ2, CMN2, FMJ2, FMN2, SINC, COSC, CONST, RHOC, SIGIC2,
     *   DET, RHOA
C
C Degrees per radian, over two
C
      DATA CONST /28.647888/
C-----------------------------------------------------------------------
C                                       Get useful constants
      CMJ2 = CMAJ * CMAJ
      CMN2 = CMIN * CMIN
      FMJ2 = FMAJ * FMAJ
      FMN2 = FMIN * FMIN
      SINC = (FPA - CPA) / CONST
      COSC = COS(SINC)
      SINC = SIN(SINC)
C                                       Trigonometry now
      RHOC = (FMJ2 - FMN2) * COSC - (CMJ2 - CMN2)
      IF (RHOC.EQ.0.0) THEN
         SIGIC2 = 0.0
         RHOA = 0.0
      ELSE
         SIGIC2 = ATAN((FMJ2 - FMN2) * SINC / RHOC)
         RHOA = ((CMJ2 - CMN2) - (FMJ2 - FMN2) * COSC) /
     *      (2.0 * COS(SIGIC2))
         END IF
 20   RPA = SIGIC2 * CONST + CPA
      DET = ((FMJ2 + FMN2) -(CMJ2 + CMN2)) / 2.0
      RMAJ = DET - RHOA
      RMIN = DET + RHOA
      IF (RMAJ.LE.0.0) THEN
         CALL MSGPUT ('Zero fitted axis','W')
         RMAJ = 1.E-20
      END IF
      IF (RMIN.LE.0.0) THEN
         CALL MSGPUT ('Zero fitted axis','W')
         RMIN = 1.E-20
      END IF
C
      RMAJ = SQRT (RMAJ)
      RMIN = SQRT (RMIN)
C                                       Swap to get major > minor
      IF (RMAJ.LT.RMIN) THEN
         SINC = RMAJ
         RMAJ = RMIN
         RMIN = SINC
         RPA = MOD (RPA+450.0, 180.0)
      END IF
C
 999  RETURN
      END
