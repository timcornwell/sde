C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flyclean.f	1.9    2/11/93
C
      SUBROUTINE FLYCLEAN (DRT, PSF, CLN, CCL)
C
C Do limited minor cycle clean only. This is called by fly.
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	CLN	CH*(*)	input	Name of CLN image
C
C Audit trail:
C	Removed redundant MSGPUTs (This is turned off by fly) and
C	also removed initial calculation of MAXRES since this should
C	be passed down. Also initialized TFLUX correctly.
C					T.J. Cornwell	Feb 24 1990
C	Removed WIN
C					T.J. Cornwell	October 1 1992
C	Restored WIN
C					T.J. Cornwell	December 23 1992
C	Don't accumulate TFLUX twice
C					T.J. Cornwell	January 3 1993
C	Hang dirty image PIX arrays from CCL/DIRTY
C					T.J. Cornwell	January 5 1993
C	Removed WIN
C					T.J. Cornwell	January 30 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, CLN, CCL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYCLEAN')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX, SLIM, ASLIM, 
     $			SPEED, TTFLUX
      INTEGER		NITER, BITER, ANITER, FN, GITER
      CHARACTER*1	DATYPE, PATYPE, CATYPE, WATYPE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		WNAX, WADD, WNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, CFLADD
      INTEGER		DLADD, DXLADD, DYLADD, DZLADD
      INTEGER		IAX, NREAL, NDUMMY, NL, ANL, NAX
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      IF (DATEXIST (STRM2 (CCL, 'CCNL'))) THEN
         CALL DATGETI (CCL, 'CCNL', NL, 1, NDUMMY)
      ELSE
         NL = 16384
      END IF
      IF (DATEXIST(STRM2(CCL, 'FIELD'))) THEN
         CALL DATGETI(CCL, 'FIELD', FN, 1, NDUMMY)
      ELSE
         FN = 1
         CALL DATPUTI(CCL, 'FIELD', FN, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'NITER'))) THEN
         CALL DATGETI(CCL, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 100
         CALL DATPUTI(CCL, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'BITER'))) THEN
         CALL DATGETI(CCL, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(CCL, 'BITER', BITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'GITER'))) THEN
         CALL DATGETI(CCL, 'GITER', GITER, 1, NDUMMY)
      ELSE
         GITER = BITER
         CALL DATPUTI(CCL, 'GITER', GITER, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'GAIN'))) THEN
         CALL DATGETR(CCL, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
         CALL DATPUTR(CCL, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'FLUX'))) THEN
         CALL DATGETR(CCL, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CCL, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'TFLUX'))) THEN
         CALL DATGETR(CCL, 'TFLUX', TFLUX, 1, NDUMMY)
      ELSE
         TFLUX = 0.0
         CALL DATPUTR(CCL, 'TFLUX', TFLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'SLIM'))) THEN
         CALL DATGETR(CCL, 'SLIM', SLIM, 1, NDUMMY)
      ELSE
         SLIM = 0.0
         CALL DATPUTR(CCL, 'SLIM', SLIM, 1)
      END IF
      IF (DATEXIST(STRM2(CCL, 'SPEED'))) THEN
         CALL DATGETR(CCL, 'SPEED', SPEED, 1, NDUMMY)
      ELSE
         SPEED = 1.0
         CALL DATPUTR(CCL, 'SPEED', SPEED, 1)
      END IF
C
C Make lists
C
      CALL DATGETAR (DRT, DNAX, DNAXIS, DATYPE, DADD)
      IF (ERROR) GO TO 990
      IF (DATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DATYPE//' for Dirty Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (PSF, PNAX, PNAXIS, PATYPE, PADD)
      IF (ERROR) GO TO 990
      IF (PATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//PATYPE//' for PSF')
         GO TO 990
      END IF
C
      CALL DATGETAR (CLN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
      NREAL = 0
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean and Dirty Axes disagree')
            GO TO 990
         END IF
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Make locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CCL, 'PIXLIST'))) THEN
         CALL DATCREAT(CCL)
         CALL DATCREAT(STRM2(CCL,'DIRTY'))
         NAX = 1
         NAXIS(1) = ABS(NITER)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CCL, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         ATYPE = 'I'
         CALL DATMAKAR (STRM2(CCL, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CCL, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
         CALL ARRSETCO (STRM2(CCL, 'PIXLIST'), 0.0, 0.0)
         CALL ARRSETCO (STRM2(CCL, 'PIXFIELD'), 0.0, 0.0)
         CALL ARRSETCO (STRM2(CCL, 'PIXLOC'), 0.0, 0.0)
      ELSE
         CALL DATGETAR (STRM2(CCL, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         IF(NITER.GT.NAXIS(1)) THEN
            CALL MSGPUT ('Increasing number of clean components', 'I')
            CALL ARREXPAN (STRM2(CCL,'PIXLIST'), 1, NITER)
            CALL ARREXPAN (STRM2(CCL,'PIXFIELD'), 1, NITER)
            NAXIS(1) = NITER
            CALL ARREXPAN (STRM2(CCL,'PIXLOC'), 2, NAXIS)
         ENDIF
         CALL DATGETAR (STRM2(CCL, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         IF (ERROR) GOTO 999
         CALL DATGETAR (STRM2(CCL, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         CALL DATGETAR (STRM2(CCL, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
C Now make locations for residual list
C
      IF (.NOT.DATEXIST (STRM2(CCL, 'DIRTY/PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = NL
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CCL, 'DIRTY/PIXLIST'), NAX, NAXIS, ATYPE,
     1     DLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CCL, 'DIRTY/PIXLOC'), NAX, NAXIS, ATYPE,
     1     DXLADD)
         DYLADD = DXLADD + NAXIS(1)
         DZLADD = DXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(CCL, 'DIRTY/PIXLIST'), NAX, NAXIS, ATYPE,
     1     DLADD)
         CALL DATGETAR (STRM2(CCL, 'DIRTY/PIXLOC'), NAX, NAXIS, ATYPE,
     1     DXLADD)
         DYLADD = DXLADD + NAXIS(1)
         DZLADD = DXLADD + 2 * NAXIS(1)
      END IF
C
C Construct histogram and find limiting value such that no more than
C NL points will be selected. Then ensure that this limit is no
C lower than the flux limit passed.
C
      CALL ARRABSHI (DRT, STRM2(CCL, 'ABSHIST'))
      CALL ARRFLIM (STRM2(CCL, 'ABSHIST'), NL, ASLIM)
      IF (ASLIM.LT.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $        'Limit in absolute value < 0.0')
         GO TO 999
      END IF
C
C This next line is changed for multi-field cleans. We need to
C set SLIM based upon the FLUX limit for all fields which is what is
C passed to us.
C
      SLIM = MAX(SLIM, ASLIM)
C
C Finally do something
C
      IF (NREAL.EQ.2) THEN
C
C Construct the list of pixels having values > SLIM. ANL is the actual
C number of such values.
C
         CALL PIX2DRLI (MEMR(DADD), DNAXIS(1), DNAXIS(2), SLIM, 
     1      MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD), NL, ANL)
         IF (ERROR) GO TO 990
C
C Now do minor cycle clean on the list of residuals stopping at the greater
C of the specified flux limit or the cutoff in residuals. We must pass
C both FLUX and SLIM since the latter must be used to limit the
C number of clean components found.
C
         CALL PIX2DRCC (MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD),
     1      ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), GAIN, NITER, 
     2      BITER, GITER, FLUX, SPEED, SLIM, MEMR(CLADD), MEMI(CXLADD), 
     3      MEMI(CYLADD), MEMI(CFLADD), FN, ANITER, MAXRES, TFLUX)
C
C Convert the list of clean components into an image (we actually
C add the specified clean components to the current image)
C
         CALL PIX2DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $      MEMI(CXLADD), MEMI(CYLADD), 
     1      BITER, ANITER, MEMR(CADD), CNAXIS(1), CNAXIS(2))
      ELSE IF (NREAL.EQ.3) THEN
         CALL PIX3DRLI (MEMR(DADD), DNAXIS(1), DNAXIS(2), 
     1      DNAXIS(3), SLIM, MEMR(DLADD), MEMI(DXLADD), 
     2      MEMI(DYLADD), MEMI(DZLADD), NL, ANL)
         CALL PIX3DRCC (MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD),
     1      MEMI(DZLADD), ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), 
     2      PNAXIS(3), GAIN, NITER, BITER, GITER, FLUX, SPEED, SLIM, 
     3      MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), MEMI(CZLADD), 
     4      MEMI(CFLADD), FN, ANITER, MAXRES, TFLUX)
         CALL PIX3DRMA (MEMR(CLADD), MEMI(CFLADD), FN, 
     &      MEMI(CXLADD), MEMI(CYLADD), 
     1      MEMI(CZLADD), BITER, ANITER, MEMR(CADD), CNAXIS(1), 
     2      CNAXIS(2), CNAXIS(3))
      ELSE
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
C Store goodies
C
      CALL DATPUTR (CCL, 'FLUX', MAXRES, 1)
      CALL DATPUTR (CCL, 'TFLUX', TFLUX, 1)
      CALL DATPUTI (CCL, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
