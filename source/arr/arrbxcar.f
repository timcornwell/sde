C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrbxcar.f	1.1    10/29/91
C
      SUBROUTINE ARRBXCAR (TIMIN, DATIN, WGTIN, TIMOUT, DATOUT, WGTOUT,
     $   TINT, MODE)
C
CD Boxcar average an array
C
C       TIMIN	C*(*)	input	Name of input time array
C	DATIN	C*(*)	input	Name of input data array
C	WGTIN	C*(*)	input	Name of input weight array
C       TIMOUT	C*(*)	input	Name of output time array
C	DATOUT	C*(*)	input	Name of output data array
C	WGTOUT	C*(*)	input	Name of output weight array
C	TINT	C*(*)	input	Integration time
C	MODE	C*(*)	input	'AMPPHI' or ' '
C
C MODE is only examined when averaging complex arrays.  If it has the
C vaule 'AMPPHI', the array will averaged separately in amplitude and
C phase.  Otherwise, simple real or complex accumulation will be used.
C For the moment, the same timebase scheme as ARRNBOX is used.
C
C If the data array is two dimensional, it is assumed that the time
C axis is the second, and that one dimensional averaging is to
C be done for each value of the first index from DATIN/MIN1 to
C DATIN/MAX1.  The weight and time arrays must be one dimensional.
C
C Audit trail:
C	Original version
C                               D.S.Briggs      Oct 28 1991
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	TIMIN, DATIN, WGTIN, TIMOUT, DATOUT, WGTOUT,
     $   		MODE
      REAL		TINT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRBXCAR')
C
      CHARACTER		ATYPE*1, INTYPE*1
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		TIADD, TOADD, DIADD, DOADD, WIADD, WOADD,
     $   		NIN, NOUT, ISTRIDE, MIN1, MAX1, NDAX, I, NDUMMY
C
      INTEGER		DATADD, DATFGETI
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
C Find the input arrays
C
      CALL DATGETAR (DATIN, NAX, NAXIS, INTYPE, DIADD)
      NDAX = NAX
      IF ((INTYPE.NE.'R').AND.(INTYPE.NE.'X')) THEN
         MESSAGE = 'Input data type ''' // INTYPE // ''' not supported'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 990
      END IF
      IF (NAX.EQ.1) THEN
         NIN = NAXIS(1)
         ISTRIDE = 1
         MIN1 = 1
         MAX1 = 1
      ELSE IF (NAX.EQ.2) THEN
         NIN = NAXIS(2)
         ISTRIDE = NAXIS(1)
         IF (DATEXIST(STRM2(DATIN,'MIN1'))) THEN
            MIN1 = DATFGETI(DATIN, 'MIN1')
         ELSE
            MIN1 = 1
         END IF
         IF (DATEXIST(STRM2(DATIN,'MAX1'))) THEN
            MAX1 = DATFGETI(DATIN, 'MAX1')
         ELSE
            MAX1 = NAXIS(1)
         END IF
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input array has too many axes')
         GO TO 990
      END IF
C
      CALL DATGETAR (TIMIN, NAX, NAXIS, ATYPE, TIADD)
      IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NIN)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Problem with input time array')
         GO TO 990
      END IF
      IF (WGTIN.EQ.' ') THEN
         CALL ARRCOPY (TIMIN, 'Input Weights')
         CALL ARRSETCO ('Input Weights', 0.0, 1.0)
         WIADD = DATADD('Input Weights')
      ELSE
         CALL DATGETAR (WGTIN, NAX, NAXIS, ATYPE, WIADD)
         IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NIN)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Problem with input weight array')
            GO TO 990
         END IF
      END IF
      IF (ERROR) GO TO 990
C
C Now deal with the output arrays
C
      IF (WGTIN.EQ.' ') THEN
         CALL ARRNBOX (TIMIN, 'Input Weights', TINT, NOUT)
      ELSE
         CALL ARRNBOX (TIMIN, WGTIN, TINT, NOUT)
      END IF
      IF (ERROR) GO TO 990
C
      IF (DATEXIST(DATOUT)) THEN
         CALL DATGETAR (DATOUT, NAX, NAXIS, ATYPE, DOADD)
         IF ((NAX.NE.NDAX).OR.(ATYPE.NE.INTYPE).OR.
     $       ((NDAX.EQ.1).AND.(NAXIS(1).NE.NOUT)).OR.
     $       ((NDAX.EQ.2).AND.(NAXIS(2).NE.NOUT))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Problem with output data array')
            GO TO 990
         END IF
      ELSE
         IF (NDAX.EQ.1) THEN
            NAX = 1
            NAXIS(1) = NOUT
         ELSE
            NAX = 2
            NAXIS(1) = ISTRIDE
            NAXIS(2) = NOUT
         END IF
         CALL DATMAKAR (DATOUT, NAX, NAXIS, INTYPE, DOADD)
      END IF
      IF (DATEXIST(TIMOUT)) THEN
         CALL DATGETAR (TIMOUT, NAX, NAXIS, ATYPE, TOADD)
         IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NOUT)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Problem with output time array')
            GO TO 990
         END IF
      ELSE
         CALL DATMAKAR (TIMOUT, 1, NOUT, 'R', TOADD)
      END IF
      IF (WGTOUT.EQ.' ') THEN
         CALL ARRCOPY (TIMOUT, 'Output Weights')
         WOADD = DATADD('Output Weights')
      ELSE IF (DATEXIST(WGTOUT)) THEN
         CALL DATGETAR (WGTOUT, NAX, NAXIS, ATYPE, WOADD)
         IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NOUT)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Problem with output weight array')
            GO TO 990
         END IF
      ELSE
         CALL DATMAKAR (WGTOUT, 1, NOUT, 'R', WOADD)
      END IF
      IF (ERROR) GO TO 990
C
C Now actually do something useful
C
      IF (INTYPE.EQ.'R') THEN
         DO 500 I = MIN1, MAX1
            CALL PIXRBXCR (MEMR(TIADD), MEMR(DIADD+I-1), MEMR(WIADD),
     $         NIN, MEMR(TOADD), MEMR(DOADD+I-1), MEMR(WOADD), NOUT,
     $         TINT, ISTRIDE)
 500     CONTINUE
      ELSE IF (INTYPE.EQ.'X') THEN
         IF (MODE.EQ.'AMPPHI') THEN
            CALL ARRX2AP (DATIN, 'Amp In', 'Phs In')
            CALL DATGETAR (DATOUT, NAX, NAXIS, ATYPE, NDUMMY)
            CALL DATMAKAR ('Phs Out', NAX, NAXIS, 'R', DOADD)
            DIADD = DATADD('Phs In')
            DO 600 I = MIN1, MAX1
               CALL PIXPHFIX (MEMR(DIADD+I-1), MEMR(DIADD+I-1),
     $            NIN, ISTRIDE)
               CALL PIXRBXCR (MEMR(TIADD), MEMR(DIADD+I-1), MEMR(WIADD),
     $            NIN, MEMR(TOADD), MEMR(DOADD+I-1), MEMR(WOADD), NOUT,
     $            TINT, ISTRIDE)
 600        CONTINUE
            DIADD = DATADD('Amp In')
            CALL DATMAKAR ('Amp Out', NAX, NAXIS, 'R', DOADD)
            DO 650 I = MIN1, MAX1
               CALL PIXRBXCR (MEMR(TIADD), MEMR(DIADD+I-1), MEMR(WIADD),
     $            NIN, MEMR(TOADD), MEMR(DOADD+I-1), MEMR(WOADD), NOUT,
     $            TINT, ISTRIDE)
 650        CONTINUE
            CALL ARRAP2X ('Amp Out', 'Phs Out', DATOUT)
            CALL DATDELAR ('Amp In')
            CALL DATDELAR ('Amp Out')
            CALL DATDELAR ('Phs In')
            CALL DATDELAR ('Phs Out')
            IF (ERROR) GO TO 990
         ELSE
            DO 700 I = MIN1, MAX1
               CALL PIXXBXCR (MEMR(TIADD), MEMX(DIADD+I-1), MEMR(WIADD),
     $            NIN, MEMR(TOADD), MEMX(DOADD+I-1), MEMR(WOADD), NOUT,
     $            TINT, ISTRIDE)
 700        CONTINUE
         END IF
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Funky array type')
         GO TO 990
      END IF
C
C Nuke the weight arrays if needed
C
      IF (WGTIN.EQ.' ')  CALL DATDELAR ('Input Weights')
      IF (WGTOUT.EQ.' ') CALL DATDELAR ('Output Weights')
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
