C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gaitrans.f	1.1    7/13/94
C
      SUBROUTINE GAITRANS (VISIN, VISEXAMP, VISREF, VISOUT, MODE)
C
CD Tranfer the calibration from one (pair) of sets to another.
C
C     VISIN	CH*(*)	input	Input vis set, eg VIS/OBS/I
C     VISEXAMP	CH*(*)	input	Vis set with cal to be transferred
C     VISREF	CH*(*)	input	Reference vis set  (well calibrated)
C     VISOUT	CH*(*)	input	Vis set to receive cal
C     MODE	CH*(*)	input	AMPPHASE, AMP, PHASE
C
C
C This routine provides a means of transferring a calibration from an
C existing data set to a simulation.  VISREF should be the best
C calibrated set (presumably with small or negligible calibration
C errors).  VISEXAMP is an ealier calibration that you wish transferred
C to the simulation.  Each visibility in the output will be corrupted
C with
C
C  VisOut = VisIn * VisExample / VisRef
C
C All three datasets must correspond exactly, which pretty much
C restricts this feature to VisObs style simulations.  If Example & Ref
C are the same data set that has been processed with closure, then the
C introduced errors will close.  If Ref is a model, or baseline
C dependent corrections have been done, then the introduced errors may
C not close.
C
C The output must already exist, and may be the same as the input
C
C Audit trail:
C	Original version:
C				D.S.Briggs	July 8 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VISIN, VISOUT, VISREF, VISEXAMP, MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAITRANS')
C
      CHARACTER		ATYPE*1
      INTEGER		NAXIN,  NAXISIN(SYSMXDIM), VADDIN, WADDIN
      INTEGER		NAXOUT, NAXISOUT(SYSMXDIM), VADDOUT, WADDOUT
      INTEGER		NAXREF, NAXISREF(SYSMXDIM), VADDREF, WADDREF
      INTEGER		NAXEX,  NAXISEX(SYSMXDIM), VADDEX, WADDEX
      INTEGER		IIN, IOUT, IEX, IREF, RNAX, NVIS
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		DATADD, CRDRNAX
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2(VISIN,'VIS'),
     $     NAXIN, NAXISIN, ATYPE, VADDIN)
      CALL DATGETAR (STRM2(VISOUT,'VIS'),
     $     NAXOUT, NAXISOUT, ATYPE, VADDOUT)
      CALL DATGETAR (STRM2(VISEXAMP,'VIS'),
     $     NAXEX, NAXISEX, ATYPE, VADDEX)
      CALL DATGETAR (STRM2(VISREF,'VIS'),
     $     NAXREF, NAXISREF, ATYPE, VADDREF)
      WADDIN = DATADD(STRM2(VISIN,'WT'))
      WADDOUT = DATADD(STRM2(VISOUT,'WT'))
      WADDEX = DATADD(STRM2(VISEXAMP,'WT'))
      WADDREF = DATADD(STRM2(VISREF,'WT'))
      IF (ERROR) GO TO 990
C
      RNAX = 1
      IIN = CRDRNAX(NAXIN, NAXISIN)
      IOUT = CRDRNAX(NAXOUT, NAXISOUT)
      IEX = CRDRNAX(NAXEX, NAXISEX)
      IREF = CRDRNAX(NAXREF, NAXISREF)
C
      IF ((RNAX.NE.IIN).OR.(RNAX.NE.IOUT).OR.(RNAX.NE.IEX)
     $     .OR.(RNAX.NE.IREF)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Bad number of real axes in an input')
         GO TO 999
      END IF
C
      NVIS = NAXISIN(1)
      IF ((NVIS.NE.NAXISOUT(1)).OR.(NVIS.NE.NAXISEX(1)).OR.
     $    (NVIS.NE.NAXISREF(1))) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Visibilities not all same size')
         GO TO 999
      END IF
C
C Now do the real work
C
      CALL GAITRANP (MEMX(VADDIN), MEMR(WADDIN), MEMX(VADDEX),
     $   MEMR(WADDEX), MEMX(VADDREF), MEMR(WADDREF), MEMX(VADDOUT),
     $   MEMR(WADDOUT), NAXISOUT(1), MODE)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
