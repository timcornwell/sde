C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsdicl.f	1.1    5/22/92
C
      SUBROUTINE IMGSDICL (DRT, XFR, CLN, RES, BOX, MVIS)
C
CD Clean an image.  (SDI style)
C
C	DRT	CH*(*)	input	Name of Dirty image
C	XFR	CH*(*)	input	Name of transfer function
C	CLN	CH*(*)	input	Name of Component image
C	RES	CH*(*)	input	Name of Residual image
C	BOX	CH*(*)	input	Name of Clean Box image
C	MVIS	CH*(*)	input	Name of FT file
C Audit trail:
C	Original version: Cloned from IMGCLEAN/IMGCCLEA
C				D.S.Briggs	Feb 25 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, XFR, CLN, RES, BOX, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSDICL')
C
      REAL		GAIN, FLUX, MAXRES, MINRES, TFLUX, TRIM, SUM
      INTEGER		NSUB, BSUB, NLOC, NPIX, WADD, NL, ANL, PX, PY,
     $   		NITER
      CHARACTER*1	DATYPE, CATYPE, RATYPE, ATYPE, BATYPE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		RNAX, RADD, RNAXIS(SYSMXDIM)
      INTEGER		BNAX, BADD, BNAXIS(SYSMXDIM)
      INTEGER		IAX, NREAL, NDUMMY, NAX, NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD
C
      REAL		DATFGETR
      LOGICAL		DATEXIST
      INTEGER		DATFGETI, DATADD
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (DRT, DNAX, DNAXIS, DATYPE, DADD)
      IF (ERROR) GO TO 990
      IF (DATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DATYPE//' for Dirty Image')
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
      CALL DATGETAR (RES, RNAX, RNAXIS, RATYPE, RADD)
      IF (ERROR) GO TO 990
      IF (RATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//RATYPE//' for Residual Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (BOX, BNAX, BNAXIS, BATYPE, BADD)
      IF (ERROR) GO TO 990
      IF (BATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//BATYPE//' for Clean Box Image')
         GO TO 990
      END IF
C
      NREAL = 0
      NPIX = 1
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean and Dirty Axes disagree')
            GO TO 990
         END IF
         IF (RNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Residual and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (BNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean Box and Dirty Axes disagree')
            GO TO 999
         END IF
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
            NPIX = NPIX * DNAXIS(IAX)
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Get control parameters
C
      IF (DATEXIST(STRM2(CLN, 'NITER'))) THEN
         CALL DATGETI(CLN, 'NITER', NSUB, 1, NDUMMY)
      ELSE
         NSUB = 100
         CALL DATPUTI(CLN, 'NITER', NSUB, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'BITER'))) THEN
         CALL DATGETI(CLN, 'BITER', BSUB, 1, NDUMMY)
      ELSE
         BSUB = 1
         CALL DATPUTI(CLN, 'BITER', BSUB, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'TRIM'))) THEN
         CALL DATGETR(CLN, 'TRIM', TRIM, 1, NDUMMY)
      ELSE
         TRIM = .5
         CALL DATPUTR(CLN, 'TRIM', 1, NDUMMY)
      END IF
      IF (DATEXIST(STRM2(CLN, 'GAIN'))) THEN
         CALL DATGETR(CLN, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
         CALL DATPUTR(CLN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'FLUX'))) THEN
         CALL DATGETR(CLN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CLN, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'TFLUX'))) THEN
         CALL DATGETR(CLN, 'TFLUX', TFLUX, 1, NDUMMY)
      ELSE
         TFLUX = 0.0
         CALL DATPUTR(CLN, 'TFLUX', TFLUX, 1)
      END IF
C
C Make clean component list
C
      IF (.NOT.DATEXIST (STRM2(CLN, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = ABS(NSUB)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
      ELSE
         CALL DATGETAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
      END IF
      CYLADD = CXLADD + NAXIS(1)
      CZLADD = CXLADD + 2*NAXIS(1)
      NL = NAXIS(1)
C
C Make a work array
C
      CALL IMGCLONE (DRT, 'SDI-Work')
      WADD = DATADD('SDI-Work')
C
C Finally do something (could be a restart, so do full calculation)
C
      NITER = BSUB
      CALL IMGRESID (CLN, DRT, XFR, RES, MVIS)
      CALL MSGPUT (
     $   '  Nlocations     Ncomps   Residual(Jy/beam)   Total Flux(Jy)',
     $   'I')
      IF (NITER.GT.1) THEN
         CALL ARRSTAT (RES, ' ')
         MAXRES = DATFGETR (CLN, 'ARRMAX')
         MINRES = DATFGETR (CLN, 'ARRMIN')
         NLOC = DATFGETI (CLN, 'ARRNLOC')
         IF (ABS(MAXRES).LT.ABS(MINRES)) MAXRES = MINRES
         WRITE (MESSAGE, 1000) NLOC, NITER, MAXRES, TFLUX
 1000    FORMAT (2X,I6,6X,I7,6X,1PE12.4,7X,1PE12.4)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C ***************** Start of major CLEANING **********************
C
  1   CONTINUE
C
C Select components (included box selection and trim contour)
C
      CALL PIX2DRTS (MEMR(RADD), MEMR(BADD), TRIM, RNAXIS(1),
     $   RNAXIS(2), MAXRES, PX, PY)
C
C Convolve it with the beam, into the work array
C
c      CALL FILIMGPU (RES, 'RES.TST', ' ')

      CALL IMGCONV (RES, ' ', XFR, 'SDI-Work', MVIS)

c      CALL FILIMGPU ('SDI-Work', 'RES1.TST', ' ')

C
C Rescale the components.
C
      CALL PIX2DRRS (MEMR(RADD), MEMR(WADD), GAIN, RNAXIS(1),
     $   RNAXIS(2), SUM)
      TFLUX = TFLUX + SUM

c      CALL FILIMGPU (RES, 'RES2.TST', ' ')

C
C Stuff the compnents into the CC list.  (Only used for output subsequently)
C
      CALL PIX2DRLI (MEMR(RADD), RNAXIS(1), RNAXIS(2), 0.0,
     $   MEMR(CLADD+NITER-1), MEMI(CXLADD+NITER-1),
     $   MEMI(CYLADD+NITER-1), NL-NITER+1, ANL)
      NITER = NITER + ANL
C
C Add the current batch of components into the main component image.
C
      CALL PIXRADD (MEMR(RADD), MEMR(CADD), MEMR(CADD), NPIX)
C
C Remake the residuals
C
      CALL IMGRESID (CLN, DRT, XFR, RES, MVIS)
C
C Tell us about it
C
      CALL ARRSTAT (RES, ' ')
      MAXRES = DATFGETR (RES, 'ARRMAX')
      MINRES = DATFGETR (RES, 'ARRMIN')
      CALL ARRSTAT (CLN, ' ')
      NLOC = DATFGETI (CLN, 'ARRNLOC')
      IF (ABS(MAXRES).LT.ABS(MINRES)) MAXRES = MINRES
      WRITE (MESSAGE, 1000) NLOC, NITER-1, MAXRES, TFLUX
      CALL MSGPUT (MESSAGE, 'I')
C
C Loopback
C
      IF (SYSINTRP.AND.SYSINTAC.EQ.'QUIT') GO TO 2
      IF ((MAXRES.LE.0.0).AND.(FLUX.LT.0.0)) GO TO 2
      IF ((ABS(MAXRES).GT.FLUX).AND.(NITER.LT.NSUB)) GO TO 1
 2    CONTINUE
C
C Store goodies
C
      CALL DATPUTI (CLN, 'NITER', NITER, 1)
      CALL DATPUTR (CLN, 'FLUX', MAXRES, 1)
      CALL DATPUTR (CLN, 'TFLUX', TFLUX, 1)
C
C Cleanup
C
      CALL DATDELET ('SDI-Work')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

