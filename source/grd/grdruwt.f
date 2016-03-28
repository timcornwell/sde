C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdruwt.f	1.2    11/1/94
C
      SUBROUTINE GRDRUWT (VIS, SUB, WT)
C
CD Reweight a visibility data set. The output image must exist before 
C calling. The visibility data must be in a standard form.
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	WT	CH*(*)	input	Name of output grid
C	WT/WTFOV  REAL	input	Minimize sidelobes over this fraction of FOV
C	WT/ROBUST REAL	input	Flux reference for robust weighting.
C	WT/DS	  REAL	input	Delta S per correlator for unit weight
C
C If the ROBUST parameter is non-negative, it is specified in terms of
C the average local (gridded) weight.  A value of 1 means that the
C equivalent flux is that corresponding to the average gridded weight.
C If negative, it is being specified in terms of true flux, the
C conversion constant being DS.
C
C Audit trail:
C	Cloned from GRDUWT 1.4
C				D.S.Briggs	Aug 24 1993
C	Many existing programs are calling this with WT as a complex
C	argument instead of a real.  Fix it here, though programs which
C	insist on doing this won't be able to examine the resulting grid
C	easily.  The weighting will be done properly and safely, however.
C				D.S.Briggs	Aug 29 1993
C	Tweaked to normalize to the average local weight.
C				D.S.Briggs	Sept 25 1993
C	Defaults changed so that this becomes a superset of GRDUWT.  If
C	the ROBUST parameter is not found, it dispatches to the old
C	pixel level routines.
C				D.S.Briggs	March 3 1994
C	Added azimuthal robust weighting
C				D.S.Briggs	Oct 24 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, WT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDRUWT')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      DOUBLE  PRECISION	SMAT (3,3)
      INTEGER		INAX, IAX, INAXIS(SYSMXDIM), INREAL
      INTEGER		DATADD, VSADD, WTADD, IADD, IWADD, UADD, VADD,
     1			WADD
      REAL		USCALE, UOFFSET
      REAL		VSCALE, VOFFSET
      REAL		WSCALE, WOFFSET
      REAL		WTFOV, ROBUST(3), DS
      INTEGER		UORIGIN, VORIGIN, WORIGIN
      CHARACTER*(SYSMXNAM)	SVIS, RMODE, SVWTS, RDWTS
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST, SLOWZ
      INTEGER		NDUMMY
C==========================================================================
      IF (ERROR) GO TO 999
C
C Get optional parameters
C
      IF (DATEXIST (STRM2(WT, 'WTFOV'))) THEN
         CALL DATGETR (WT, 'WTFOV', WTFOV, 1, NDUMMY)
      ELSE
         WTFOV = 1.0
      END IF
C
      IF (DATEXIST(STRM2(WT,'ROBUST'))) THEN
         CALL DATGETR (WT, 'ROBUST', ROBUST, 3, NDUMMY)
C
         IF (DATEXIST(STRM2(WT,'RMODE'))) THEN
            CALL DATGETC (WT, 'RMODE', RMODE, 1, NDUMMY)
         ELSE
            RMODE = 'NORM'
         END IF
C
         IF (RMODE.EQ.'ABS') THEN
            CALL DATGETR (WT, 'DS', DS, 1, NDUMMY)
         END IF
      ELSE
         RMODE = 'NONE'
      END IF
C
      IF (DATEXIST(STRM2(WT,'SAVEWEIGHTS'))) THEN
         CALL DATGETC (WT, 'SAVEWEIGHTS', SVWTS, 1, NDUMMY)
      ELSE
         SVWTS = ' '
      END IF
C
      IF (DATEXIST(STRM2(WT,'READWEIGHTS'))) THEN
         CALL DATGETC (WT, 'READWEIGHTS', RDWTS, 1, NDUMMY)
      ELSE
         RDWTS = ' '
      END IF
C
C Find coordinate information
C
      SVIS = STRM2 (VIS, SUB)
      CALL CRDGET (SVIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for input data')
         GO TO 990
      END IF
      CALL CRDGET (WT, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find coordinates for output image')
         GO TO 990
      END IF
C
C Find rotation matrix required to align data
C
      CALL CRDSHIFT (SVIS, WT, SMAT)
C
C Is the Z-transform slow?
C
      SLOWZ = ITYPE(3).EQ.'N----SIN'
C
C Find addresses of data for the input visibilities, input weights,
C output image, and output weights image
C
      CALL DATGETAR (STRM2(SVIS, 'WT'), VNAX, VNAXIS, VATYPE, WTADD)
      CALL DATGETAR (WT, INAX, INAXIS, IATYPE, IADD)
C
C Now actually do something: first find number of real axes of the
C output image
C
      INREAL = 0
      DO 5 IAX = 1, INAX
         IF (INAXIS(IAX).GT.1) INREAL = IAX
  5   CONTINUE
      IF ((INREAL.EQ.2).OR.((INREAL.EQ.3).AND.SLOWZ)) THEN
C
C ***********************  Two dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         USCALE = WTFOV / IDELT(1)
         UOFFSET = 0.0
         UORIGIN = NINT (IRPIX(1))
         VSCALE = WTFOV / IDELT(2)
         VOFFSET = 0.0
         VORIGIN = NINT (IRPIX(2))
         IF (IATYPE.EQ.'X') THEN
            IF (RMODE.EQ.'NONE') THEN
               CALL GRDUWT2D (MEMR(WTADD), MEMR(UADD), MEMR(VADD),
     $            MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE,
     $            VOFFSET, UORIGIN, VORIGIN, MEMR(WTADD), MEMX(IADD),
     $            INAXIS(1), INAXIS(2), SMAT)
            ELSE
               CALL GRDRUW2D (MEMR(WTADD), MEMR(UADD), MEMR(VADD),
     $            MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE,
     $            VOFFSET, UORIGIN, VORIGIN, MEMR(WTADD), MEMX(IADD),
     $            INAXIS(1), INAXIS(2), SMAT, RMODE, ROBUST, DS)
            END IF
         ELSE IF (IATYPE.EQ.'R') THEN
            IF (RMODE.EQ.'NONE') THEN
               CALL GRDUWT2D (MEMR(WTADD), MEMR(UADD), MEMR(VADD),
     $            MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE,
     $            VOFFSET, UORIGIN, VORIGIN, MEMR(WTADD), MEMR(IADD),
     $            INAXIS(1), INAXIS(2), SMAT)
            ELSE
               CALL GRDRUW2D (MEMR(WTADD), MEMR(UADD), MEMR(VADD),
     $            MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE,
     $            VOFFSET, UORIGIN, VORIGIN, MEMR(WTADD), MEMR(IADD),
     $            INAXIS(1), INAXIS(2), SMAT, ROBUST, DS)
            END IF
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad type for WT')
            GO TO 999
         END IF
      ELSEIF (INREAL.EQ.3) THEN
C
C ***********************  Three dimensions *************************
C
         UADD = DATADD (STRM2(VIS, 'UU'))
         VADD = DATADD (STRM2(VIS, 'VV'))
         WADD = DATADD (STRM2(VIS, 'WW'))
         USCALE = WTFOV / IDELT(1)
         UOFFSET = 0.0
         UORIGIN = NINT (IRPIX(1))
         VSCALE = WTFOV / IDELT(2)
         VOFFSET = 0.0
         VORIGIN = NINT (IRPIX(2))
         WSCALE = WTFOV / IDELT(3)
         WOFFSET = 0.0
         WORIGIN = NINT (IRPIX(3))
         IF (RMODE.NE.'NONE') THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Robust weighting not implemented in 3D!')
            GO TO 999
         END IF
         IF (IATYPE.EQ.'X') THEN
            CALL GRDUWT3D (MEMR(WTADD), MEMR(UADD), MEMR(VADD), 
     1         MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE, VOFFSET, 
     2         WSCALE, WOFFSET, UORIGIN, VORIGIN, WORIGIN, 
     3         MEMR(WTADD), MEMX(IADD), INAXIS(1), INAXIS(2),
     3         INAXIS(3))
         ELSE IF (IATYPE.EQ.'R') THEN
            CALL GRDUWT3D (MEMR(WTADD), MEMR(UADD), MEMR(VADD), 
     1         MEMR(WADD), VNAXIS(1), USCALE, UOFFSET, VSCALE, VOFFSET, 
     2         WSCALE, WOFFSET, UORIGIN, VORIGIN, WORIGIN, 
     3         MEMR(WTADD), MEMR(IADD), INAXIS(1), INAXIS(2),
     3         INAXIS(3))
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad type for WT')
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot re-weight more than 3 dimensions')
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
