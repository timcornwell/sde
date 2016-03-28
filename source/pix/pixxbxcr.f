C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxbxcr.f	1.1    10/29/91
C
      SUBROUTINE PIXXBXCR (TIMIN, DATIN, WGTIN, NIN,
     $   		   TIMOUT, DATOUT, WGTOUT, NOUT, TINT, STRIDE)
C
CD Boxcar average an array (real pixel level)
C
C	TIMIN	R(*)	input	Input time array
C	DATIN	X(*)	input	Input data array
C	WGTIN	R(*)	input	Input weight array
C	NIN	INT	input	size of input arrays
C	TIMOUT	R(*)	output	Output time array
C	DATOUT	X(*)	output	Output data array
C	WGTOUT	R(*)	output	Output weight array
C	NOUT	INT	input	size of output arrays
C	TINT	REAL	input	integration time
C	STRIDE	INT	input	stride in input and output data arrays
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S. Briggs	October 25 1991
C-------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NIN, NOUT, STRIDE
      COMPLEX		DATIN(*), DATOUT(*)
      REAL		TIMIN(NIN), WGTIN(NIN),
     $   		TIMOUT(NOUT), WGTOUT(NOUT), TINT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXBXCR')
C
      COMPLEX		ACC
      REAL		TINTSM, TREF, TMIN, TMAX, SUMWT
      INTEGER		I, IOUT, ISTR, IOSTR
      LOGICAL		INRANGE, DOEXPORT
C=========================================================================
      IF (ERROR) GO TO 999
C
      TINTSM = 5.E-5 * TINT
      TINTSM = TINT - TINTSM
      IOUT = 0
C
      ACC = 0.0
      SUMWT = 0.0
      TREF = TIMIN(1)
      TMIN = 1.E20
      TMAX = -1.E20
C
C Loop over the entire array
C
      DO 500 I = 1, NIN+1
         ISTR = (I-1) * STRIDE + 1
         INRANGE  = .FALSE.
         IF (I.LE.NIN) INRANGE = ABS(TIMIN(I)-TREF) .LT. TINTSM
         IF (INRANGE) THEN
            IF (WGTIN(I).GT.0.0) THEN
               ACC = ACC + WGTIN(I) * DATIN(ISTR)
               SUMWT = SUMWT + WGTIN(I)
               TMIN = MIN(TIMIN(I), TMIN)
               TMAX = MAX(TIMIN(I), TMAX)
            END IF
         ELSE
            DOEXPORT = .TRUE.
            IF (I.LE.NIN) DOEXPORT = WGTIN(I) .GT. 0.0
            IF (DOEXPORT) THEN
               IOUT = IOUT + 1
               IOSTR = (IOUT-1) * STRIDE + 1
               IF (IOUT.GT.NOUT) THEN
                  CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $               'Too many averaged integration intervals')
                  GO TO 990
               END IF
               IF (SUMWT.GT.0.0) THEN
                  DATOUT(IOSTR) = ACC / SUMWT
               ELSE
                  DATOUT(IOSTR) = 0.0
               END IF
               WGTOUT(IOUT) = SUMWT
               TIMOUT(IOUT) = (TMAX + TMIN) / 2.0
               IF (I.LE.NIN) THEN
                  ACC = WGTIN(I) * DATIN(ISTR)
                  SUMWT = WGTIN(I)
                  TREF = TIMIN(I)
                  TMIN = TIMIN(I)
                  TMAX = TIMIN(I)
               END IF
            END IF
         END IF
 500  CONTINUE
C
      IF (IOUT.LT.NOUT) THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $      'Not enough averaged integration intervals')
      END IF
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
