C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmspsf.f	1.3    11/7/90
C
      SUBROUTINE IMGMSPSF (VIS, SUB, WT, PSF)
C
CD Make a nice PSF for mosaicing by minimizing sidelobes outside
C the primary beam and then imposing a Gaussian taper. The optimum
C weights will be returned in VIS/SUB/'WT'. The old weights will be put
C in VIS/SUB/'OLDWT'
C
C	VIS	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C	WT	CH*(*)	input	Name of grid to use
C	PSF	CH*(*)	input	Name of image to use for PSF
C Audit trail:
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, PSF, WT
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'IMGMSPSF')
C
      REAL		BEAM(4)
      CHARACTER*(SYSMXNAM)	STRM3
      INTEGER		NDUMMY
      DOUBLE PRECISION	CRVAL(SYSMXDIM), OBSRA, OBSDEC
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Save stuff
C
      CALL ARRCOPY (STRM3(VIS, SUB, 'VIS'), STRM3(VIS, SUB, 'OLDVIS'))
      CALL ARRCOPY (STRM3(VIS, SUB, 'WT'), STRM3(VIS, SUB, 'OLDWT'))
C
C Do uniform weighting part inside primary beam i.e. smooth with
C primary beam before calculating weights
C
      CALL VISTOIMG (VIS, SUB, PSF, WT, .TRUE.)
C
C Apply primary beam: first reset beam center so that it is applied
C to the phase center
C
      CALL DATGETD (PSF, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATGETD (PSF, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATGETD  (PSF, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATPUTD (PSF, 'OBSRA', CRVAL(1), 1)
      CALL DATPUTD (PSF, 'OBSDEC', CRVAL(2), 1)
      CALL IMGPB (PSF, PSF, 'APPLY')
      CALL DATPUTD (PSF, 'OBSRA', OBSRA, 1)
      CALL DATPUTD (PSF, 'OBSDEC', OBSDEC, 1)
C
C Now find width of PSF by fitting a Gaussian
C
      SYSMSG = .FALSE.
      CALL IMGBMSHP (PSF)
      SYSMSG = .TRUE.
      CALL DATGETR(PSF, 'BMAJ', BEAM(1), 1, NDUMMY)
      CALL DATGETR(PSF, 'BMIN', BEAM(2), 1, NDUMMY)
      CALL DATGETR(PSF, 'BPA', BEAM(3), 1, NDUMMY)
      CALL DATGETR(PSF, 'BZ', BEAM(4), 1, NDUMMY)
      BEAM(1) = 3600.0 * BEAM(1)
      BEAM(2) = 3600.0 * BEAM(2)
      BEAM(4) = 3600.0 * BEAM(4)
      CALL IMGCLONE (PSF, ROUTINE)
      CALL IMGMODEL (ROUTINE, 1, 1.0, 0.0, 0.0, BEAM(1), BEAM(2),
     $   BEAM(3), 'GAUS')
      CALL IMGPB (ROUTINE, ROUTINE, 'APPLY')
      CALL IMGGRIDC (ROUTINE, ROUTINE, 'CORRECT')
      CALL IMGFFT (ROUTINE, WT)
      CALL VISDEGRI(VIS, SUB, WT)
      CALL ARRABS (STRM3(VIS, SUB, 'VIS'),  STRM3(VIS, SUB, 'GWT'))
      CALL DATDELET (ROUTINE)
C
C Now transform back
C
      CALL IMGGRIDC (PSF, PSF, 'CORRECT')
      CALL IMGFFT (PSF, WT)
      CALL VISDEGRI(VIS, SUB, WT)
C
C Find weights by taking absolute value and inverting.
C
      CALL ARRABS (STRM3(VIS, SUB, 'VIS'),  STRM3(VIS, SUB, 'WT'))
      CALL ARRCOPY (STRM3(VIS, SUB, 'OLDVIS'), STRM3(VIS, SUB, 'VIS'))
      CALL ARRCDIV (STRM3(VIS, SUB, 'OLDWT'), STRM3(VIS, SUB, 'WT'),
     $   1E-8, STRM3(VIS, SUB, 'WT'))
      CALL ARRMULT (STRM3(VIS, SUB, 'WT'), STRM3(VIS, SUB, 'GWT'),
     $   STRM3(VIS, SUB, 'WT'))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
