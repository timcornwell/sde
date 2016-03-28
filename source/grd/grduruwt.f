C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grduruwt.f	1.3 1/12/95
C
      SUBROUTINE GRDURUWT (VIS, SUB, WT)
C
CD Reweight a visibility data set.  Robust & Ungridded!
C
C	VIS	CH*(*)	input	Name of visibility set
C	SUB	CH*(*)	input	Name of sub-class to grid e.g. OBS/I
C	WT	CH*(*)	input	Name of output grid
C	WT/WTFOV  REAL	input	Minimize sidelobes over this fraction of FOV
C	WT/RMODE  CH*(*)input	Robustness mode
C	WT/ROBUST REAL	input	Flux reference for robust weighting.
C	WT/DS	  REAL	input	Delta S per correlator for unit weight
C	WT/WTGRDSIZ INT(2) input Size of bucket sort grid.
C	WT/SAVEWEIGHTS	CH*(*)	input	Save summed weights to this file
C	WT/READWEIGHTS	CH*(*)	input	Read summed weights from this file
C
C The routine does use a grid for speed purposes, but only to reduce the
C number of visibilities which must be checked.  The actual weighting
C is done with the local density of ungridded visibilities.  A robustness
C parameter may also be included.  It's fairly slow, but not completely
C unreasonable.
C
C The output image must exist before calling, and the visibility data must
C be in a standard form.
C
C This routine does not actually use the WT array for anything, but does
C take a number of arguments from the header and coordinate information.
C
C Since we are doing the calculations without gridding, it's cheap to do
C the local density calculations radially, instead of seperately in U & V.
C Arguably, this is a better way.  FOV is interpreted to be proportional to
C a 1 over a radius, normalized such that FOV = 1 means a circle with
C an area of 1 square pixel.
C
C SAVEWEIGHTS and READWEIGHTS are meant for the calculation of robustness
C curves, where the same data set must be reweighted again and again,
C changing only the ROBUST parameter.  If nearly anything else is changed,
C then the saved weights from a previous run will be wrong!  Use this only
C if you know what you are doing.
C
C WT/BINDEBUG is a general purpose debugging selction parameter.  This is
C still slightly developmental, and at the moment it selections between
C mild variations of the pixel level routine.
C
C Audit trail:
C	Cloned from GRDUWT 1.4
C				D.S.Briggs	Aug 24 1993
C	Read Robust as a REAL(3) instead of REAL(1).  This is to track
C	the new mode, AZ-NORM which requires more parameters than before.
C				D.S.Briggs	Oct 26 1994
C	Elim compiler warnings (no functional mods).
C				M. Stupar	Dec 28 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, SUB, WT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDURUWT')
C
      REAL	PI
      PARAMETER (PI=3.1415927)
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      DOUBLE  PRECISION	SMAT (3,3)
      INTEGER		INAX, INAXIS(SYSMXDIM), INREAL
      INTEGER		DATADD, WTADD, UADD, VADD,
     1			WADD, NWADD, SWADD
      REAL		URADIUS, VRADIUS
      REAL		WTFOV, ROBUST(3), DS
      INTEGER		WTGRDSIZ(2)
      INTEGER		HAX, HAXIS(SYSMXDIM), HADD, LADD, BINDEBUG
      CHARACTER*(SYSMXNAM)	SVIS, SVWTS, RDWTS, WTFILE, RMODE
      LOGICAL		SLOWZ
      INTEGER		NDUMMY
C
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		ARRNPIX, CRDRNAX
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
      IF (DATEXIST(STRM2(WT,'RMODE'))) THEN
         CALL DATGETC (WT, 'RMODE', RMODE, 1, NDUMMY)
      ELSE
         RMODE = 'NORM'
      END IF
C
      IF (DATEXIST(STRM2(WT,'ROBUST'))) THEN
         CALL DATGETR (WT, 'ROBUST', ROBUST, 3, NDUMMY)
      ELSE
         ROBUST(1) = 0.0
         ROBUST(2) = 0.0
         ROBUST(3) = 0.0
      END IF
C
      IF (RMODE.EQ.'ABS') THEN
         CALL DATGETR (WT, 'DS', DS, 1, NDUMMY)
      ELSE
         DS = 1.0
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
      IF (DATEXIST(STRM2(WT,'WTGRDSIZ'))) THEN
         CALL DATGETI (WT, 'WTGRDSIZ', WTGRDSIZ, 2, NDUMMY)
      ELSE
         WTGRDSIZ(1) = 0
         WTGRDSIZ(2) = 0
      END IF
C
      IF (DATEXIST(STRM2(WT,'BINDEBUG'))) THEN
         CALL DATGETL (WT, 'BINDEBUG', BINDEBUG, 1, NDUMMY)
      ELSE
         BINDEBUG = 0
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
      INREAL = CRDRNAX (INAX, INAXIS)
C
C Find addresses of data for the input weights
C
      CALL DATGETAR (STRM2(SVIS, 'WT'), VNAX, VNAXIS, VATYPE, WTADD)
      UADD = DATADD (STRM2(VIS, 'UU'))
      VADD = DATADD (STRM2(VIS, 'VV'))
      WADD = DATADD (STRM2(VIS, 'WW'))
C
C This should probably be generalized for shifted & rotated data.
C
      URADIUS = ABS(IDELT(1)/(WTFOV*SQRT(PI)))
      VRADIUS = ABS(IDELT(2)/(WTFOV*SQRT(PI)))
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) URADIUS, VRADIUS
 1000    FORMAT ('Uradius = ',1PE12.5,'  Vradius = ',1PE12.5)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Find rotation matrix required to align data
C
      CALL CRDSHIFT (SVIS, WT, SMAT)
C
C Estimate a bin size if we have to.  This 
C
      IF (RDWTS.EQ.' ') THEN

         IF (WTGRDSIZ(1).LE.0) THEN
C
C GRDEUSZ0 is at least as good as GRDEUSIZ, as well as a lot simpler and
C a lot faster.
C
C           CALL GRDEUSIZ (UADD, VADD, WADD, WTADD, VNAXIS(1),
C     $        URADIUS, VRADIUS, SMAT, WTGRDSIZ)
            CALL GRDEUSZ0 (MEMR(UADD), MEMR(VADD), MEMR(WADD),
     $         MEMR(WTADD), VNAXIS(1), URADIUS, VRADIUS, SMAT,
     $         WTGRDSIZ(1), WTGRDSIZ(2))
         END IF
         IF (WTGRDSIZ(2).LE.0) WTGRDSIZ(2) = 2*WTGRDSIZ(1)
C
         WRITE (MESSAGE, 1010) WTGRDSIZ(1), WTGRDSIZ(2)
 1010    FORMAT ('Binning array size is ',I4,' by ',I4)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Is the Z-transform slow?
C
      SLOWZ = ITYPE(3).EQ.'N----SIN'
C
C Some work arrays that we'll need
C
      IF (RDWTS.EQ.' ') THEN
         CALL DATMAKAR ('GRD-WTLINKS', 1, ARRNPIX(STRM2(SVIS,'WT')),
     $      'I', LADD)
         CALL DATMAKAR ('GRD-NEWWTS', 1, ARRNPIX(STRM2(SVIS,'WT')),
     $      'R', NWADD)
         CALL DATMAKAR ('GRD-SUMWTS', 1, ARRNPIX(STRM2(SVIS,'WT')),
     $      'R', SWADD)
         HAX = 2
         HAXIS(1) = WTGRDSIZ(1)
         HAXIS(2) = WTGRDSIZ(2)
         CALL DATMAKAR ('GRD-HEADLINKS', HAX, HAXIS, 'I', HADD)
         IF (ERROR) GO TO 999
      ELSE
C
C Or just read in the summed weights from a previous run
C
         CALL FILSYSRT (RDWTS, WTFILE)
         CALL STRAPPEN (WTFILE, '.SDE')
         CALL FILIMGGE ('GRD-NEWWTS', WTFILE, ' ')
         NWADD = DATADD ('GRD-NEWWTS')
         IF (ERROR) GO TO 990
      END IF
C
C Now actually do something:
C
      IF ((INREAL.EQ.2).OR.((INREAL.EQ.3).AND.SLOWZ)) THEN
C
C ***********************  Two dimensions *************************
C
         IF (BINDEBUG.LE.0) THEN
C
C Extended Half Plane version, individual cell testing.
C
            CALL GRDURUW2 (MEMR(UADD), MEMR(VADD), MEMR(WADD),
     $         VNAXIS(1), URADIUS, VRADIUS, MEMR(WTADD), MEMR(NWADD),
     $         MEMI(HADD), HAXIS(1), HAXIS(2), MEMI(LADD), MEMR(SWADD),
     $         SMAT, RMODE, ROBUST, DS, SVWTS, RDWTS, 'GRD-NEWWTS')
         ELSE IF (BINDEBUG.EQ.1) THEN
C
C Strict Half Plane version.  Outdated.
C
            CALL GRDURUW2A (MEMR(UADD), MEMR(VADD), MEMR(WADD),
     $         VNAXIS(1), URADIUS, VRADIUS, MEMR(WTADD), MEMR(NWADD),
     $         MEMI(HADD), HAXIS(1), HAXIS(2), MEMI(LADD), MEMR(SWADD),
     $         SMAT, RMODE, ROBUST, DS, SVWTS, RDWTS, 'GRD-NEWWTS')
         ELSE IF (BINDEBUG.EQ.2) THEN
C
C Extended Half Plane version, raster scan of ellipse.
C
            CALL GRDURUW2B (MEMR(UADD), MEMR(VADD), MEMR(WADD),
     $         VNAXIS(1), URADIUS, VRADIUS, MEMR(WTADD), MEMR(NWADD),
     $         MEMI(HADD), HAXIS(1), HAXIS(2), MEMI(LADD), MEMR(SWADD),
     $         SMAT, RMODE, ROBUST, DS, SVWTS, RDWTS, 'GRD-NEWWTS')
         END IF
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only 2-D ungridded weighting implmented')
         GO TO 999
      END IF
C
      CALL ARRCOPY ('GRD-NEWWTS', STRM2(SVIS, 'WT'))
      IF (DATEXIST('GRD-NEWWTS')) CALL DATDELET ('GRD-NEWWTS')
      IF (DATEXIST('GRD-WTLINKS')) CALL DATDELET ('GRD-WTLINKS')
      IF (DATEXIST('GRD-HEADLINKS')) CALL DATDELET ('GRD-HEADLINKS')
C
c      call datmakar ('foo', inax, inaxis, 'R', ndummy)
c      call pixrcopy (memx(iadd), 0, 1, memr(ndummy), 0, 1,
c     $   inaxis(1)*inaxis(2))
c      call filimgpu ('foo', 'WT', ' ')
c      call arrlist2 ('Vis/OBS/I/WT', 'weights',
c     $   .false., 0, 0, .false., 0,
c     $   .false., .false., 0.d0, 0.d0)
c      call filimgpu ('Vis/OBS/I/WT', 'w.SDE', ' ')

 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
