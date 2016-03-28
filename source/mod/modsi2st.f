C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modsi2st.f	1.5    2/15/91
C
      SUBROUTINE MODSI2ST (INMOD, OUTMOD, RFREQ, FREQ, IALPHA)
C
CD Convert a spectral index model (INMOD) to a standard SDE model
C
C The Spectral Index MODEL has directory entries: 
C INMOD/FLUX	REAL	Flux of component (Jy)
C INMOD/RADIUS	REAL	Distance of Component from field Centre (masec)
C INMOD/AZ	REAL	Angle of component from ??? (Deg)
C INMOD/BMAJ	REAL	Major axis of Component (masec)
C INMOD/AXRAT	REAL	Axial Ratio 
C INMOD/BPA	REAL	Position angle (deg)
C INMOD/SI      REAL    Spectral index
C all components are gaussian in the SI model
C 
C The Standard MODEL has entries:
C OUTMOD/FLUX	REAL	Flux of component in Jy
C OUTMOD/RA	REAL	Position of component in Ra offset (asec)
C OUTMOD/DEC	REAL	Position of component in Dec offset (asec)
C OUTMOD/BMAJ	REAL	Major axis in asec
C OUTMOD/BMIN	REAL	Minor axis in asec
C OUTMOD/BPA	REAL	Position angle in degrees
C OUTMOD/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C	INMOD	CH*(*)	input	Name of input model directory entry
C	OUTMOD	CH*(*)	input	Name of output model directory entry
C	RFREQ	REAL	input	Reference frequency if SI model
C	FREQ	REAL	input	Actual Frequency of output model
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 5 1990
C       Modified to produce an alpha model an I-alpha model as well
C       as an I model
C                               R.G. Marson     Dec 5 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INMOD, OUTMOD, IALPHA
      REAL              RFREQ, FREQ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODSI2ST')
C
C Function Declarations
C
      CHARACTER*(SYSMXNAM) STRM2
      INTEGER DATADD
      LOGICAL DATEXIST
C
C Local Variable Declarations
C
      CHARACTER*(SYSMXNAM) TYPE(SYSMXDIM)
      REAL NFREQ, D2R, RAD, AZ
      INTEGER IFLADD, SIADD, RADADD, AZADD, IBADD, AXADD, PAADD
      INTEGER FLADD, RAADD, DECADD, BMAJADD, BMINADD, BPAADD, TADD
      INTEGER I, NCOMP, NAX, NAXIS(SYSMXDIM)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Check of output model exists (and if so delete it)
C
      IF (DATEXIST(OUTMOD)) CALL DATDELET(OUTMOD)
C
C Find out how many components in the input model
C
      CALL DATGETAR(STRM2(INMOD, 'FLUX'), NAX, NAXIS, TYPE, IFLADD)
C
C Make the output model array entries
C
      NCOMP = NAXIS(1)
      IF (NCOMP.LE.0) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'No Model Components')
         GOTO 999
      ELSE
         CALL DATCREAT (OUTMOD)
         CALL DATMAKAR (STRM2(OUTMOD, 'FLUX'), NAX, NAXIS, 'R', FLADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'RA'), NAX, NAXIS, 'R', RAADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'DEC'), NAX, NAXIS, 'R', DECADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'BMAJ'), NAX, NAXIS,'R', BMAJADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'BMIN'), NAX, NAXIS,'R', BMINADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'BPA'), NAX, NAXIS, 'R', BPAADD)
         CALL DATMAKAR (STRM2(OUTMOD, 'TYPE'), NAX, NAXIS, 'S', TADD)
      END IF
C
C Convert each component
C
C
C Firstly Flux
C         
      SIADD = DATADD (STRM2(INMOD, 'SI'))
      IF (FREQ.NE.RFREQ) THEN
         NFREQ = FREQ/RFREQ
         IF (IALPHA.EQ.'A ') THEN
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(SIADD + I)
            END DO
         ELSE IF (IALPHA.EQ.'IA') THEN
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(IFLADD + I) * MEMR(SIADD + I) *
     $                           (NFREQ ** (-MEMR(SIADD + I)))
            END DO
         ELSE
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(IFLADD + I) * 
     $                           (NFREQ ** (-MEMR(SIADD +I)))
            END DO
         END IF
      ELSE
         IF (IALPHA.EQ.'A ') THEN
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(SIADD + I)
            END DO
         ELSE IF(IALPHA.EQ.'IA') THEN
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(IFLADD + I) * MEMR(SIADD + I)
            END DO
         ELSE
            DO I = 0, NCOMP - 1
               MEMR(FLADD + I) = MEMR(IFLADD + I)
            END DO
         END IF
      END IF
C
C Now position on the sky (RA & DEC)
C

      RADADD = DATADD (STRM2(INMOD, 'RADIUS'))
      AZADD = DATADD (STRM2(INMOD, 'AZ'))
      D2R = ATAN(1.0)/45.0
      DO I = 0, NCOMP - 1
C Convert from masec to asecs
         RAD = MEMR(RADADD + I)/1000.0
         AZ = MEMR(AZADD + I)*D2R
         MEMR(RAADD + I) =  RAD * SIN (AZ)
         MEMR(DECADD + I) = RAD * COS (AZ)
      END DO
C
C Now the Gaussian component characteristics
C
      IBADD =  DATADD (STRM2(INMOD, 'BMAJ'))
      AXADD =  DATADD (STRM2(INMOD, 'AXRAT'))
      PAADD =  DATADD (STRM2(INMOD, 'BPA'))
      DO I = 0, NCOMP - 1
         MEMR(BMAJADD + I) = MEMR(IBADD + I)/1000.0
         MEMR(BMINADD + I) = MEMR(BMAJADD + I) * MEMR(AXADD + I)
         MEMR(BPAADD + I) = MEMR(PAADD + I)
      END DO
C
C Now just fill in the types (ALL Gaussians)
C
      DO I = 0, NCOMP - 1
         IF ((MEMR(BMAJADD + I).EQ.0.0).AND.
     $       (MEMR(BMINADD + I).EQ.0.0)) THEN
            MEMC(TADD + I) = 'POINT'
         ELSE
            MEMC(TADD + I) = 'GAUS'
         END IF
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
