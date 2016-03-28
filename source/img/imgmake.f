C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmake.f	1.6    2/18/92
C
      SUBROUTINE IMGMAKE (VIS, CELLSIZE, IMSIZE, SHIFT, MATYPE, IMG)
C
CD Make a directory entry for an image matching the coordinates
C in VIS. Copy standard parts of the header. Can make 2 or 3-D
C images. SHIFT is the shift in the tangent point in arcseconds i.e.
C this is not a shift on the tangent plane but a shift of the tangent
C point around the celestial sphere. If MATYPE is null then only the
C header is made.
C
C
C	VIS		CH*(*)	input	Name of visibility data
C	CELLSIZE	REAL(*)	input	Cell size of image in arcsec
C	IMSIZE		INT(*)	input	Image size in pixels
C	SHIFT		REAL	input	Shift in arcseconds
C	MATYPE		CH*1	input	Type of map e.g. 'R' or 'X'
C	IMG		CH*(*)	input	Name of output image
C Audit trail:
C	Added copying of history info
C				T.J.Cornwell	Jan 10 1989
C	Added SDETYPE setting
C				T.J.Cornwell	Feb 4 1989
C	Changed SHIFT to keep on celestial sphere
C				T.J.Cornwell	Feb 13, 1989
C	Changed logic so that header items are correctly copied.
C	Previously the use of IF(DATEXIST(STRM2(VIS,'OBJECT')))
C	constructs was leading to incorrect results since the 
C	corresponding item did not exist.
C				T.J. Cornwell	Sept 10 1989
C	Was copying OBSRA to OBSDEC
C				T.J. Cornwell	March 3 1991
C	Will make only header if MATYPE.EQ.' '
C				T.J. Cornwell	May 31 1991
C	Corrected reference pixel calculations for odd image size.
C				D.S. Briggs	Jan 15 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, IMG, MATYPE
      REAL		CELLSIZE(*), SHIFT(*)
      INTEGER		IMSIZE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMAKE')
C
      CHARACTER*1	VATYPE
      CHARACTER*8	VTYPE(SYSMXDIM)
      REAL		VRPIX(SYSMXDIM), VDELT(SYSMXDIM),
     1			VROTA(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      INTEGER		INAX, IADD, INAXIS(SYSMXDIM)
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, VNAXIS(SYSMXDIM)
      INTEGER 		NDUMMY, I, DIR, STRSEARC, IAX
      DOUBLE PRECISION	DVALUE
      REAL		RVALUE
      CHARACTER*(SYSMXNAM)	DATE, CVALUE, STRM2
      LOGICAL		DATEXIST
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (VIS, VNAX, VTYPE, VNAXIS, VRVAL, VRPIX, VDELT, 
     1   VROTA)
C
C First look for RA axis
C
      IATYPE = MATYPE
      INAX = 0
      IAX = STRSEARC ('RA', VTYPE, VNAX)
      IF (IAX.NE.0) THEN
          INAX = INAX + 1
          ITYPE(1) = 'RA---SIN'
C
C Use AIPS convention for RA axis
C
          IDELT(INAX) = - CELLSIZE(INAX) / 3600.0
          IRVAL(INAX) = VRVAL(IAX) + SHIFT(1) /3600.0
          INAXIS(INAX) = IMSIZE(INAX)
          IRPIX(INAX) = (IMSIZE(INAX)+1)/2
          IROTA(INAX) = VROTA(IAX)
      ELSE
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'No RA axis')
          GO TO 999
      END IF
C
C Next look for DEC axis
C
      IAX = STRSEARC ('DEC', VTYPE, VNAX)
      IF (IAX.NE.0) THEN
          INAX = INAX + 1
          ITYPE(INAX) = 'DEC--SIN'
          IDELT(INAX) = CELLSIZE(INAX) / 3600.0
          IRVAL(INAX) = VRVAL(IAX) + SHIFT(2) / 3600.0
          INAXIS(INAX) = IMSIZE(INAX)
          IRPIX(INAX) = (IMSIZE(INAX)+1)/2
          IROTA(INAX) = VROTA(IAX)
      ELSE
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'No DEC axis')
          GO TO 999
      END IF
C
C Is this a 3-D transform?
C
      IF (IMSIZE(3).NE.1) THEN
          INAX = INAX + 1
          ITYPE(INAX) = 'N----SIN'
          IDELT(INAX) = CELLSIZE(INAX) / 3600.0
          IRVAL(INAX) = 0.0D0
          INAXIS(INAX) = IMSIZE(INAX)
          IRPIX(INAX) = (IMSIZE(INAX)+1)/2
          IROTA(INAX) = VROTA(IAX)
      END IF
C
C Now do frequency
C
      IAX = STRSEARC ('FREQ', VTYPE, VNAX)
      IF (IAX.NE.0) THEN
          INAX = INAX + 1
          ITYPE(INAX) = 'FREQ'
          IRVAL(INAX) = VRVAL(IAX)
          IDELT(INAX) = VDELT(IAX)
          INAXIS(INAX) = 1
          IRPIX(INAX) = 1.0
          IROTA(INAX) = VROTA(IAX)
      ELSE
          CALL ERRREPOR (ERRBDARG, ROUTINE, 'No FREQ axis')
          GO TO 999
      END IF
C
C Only make the array if required
C
      IF(IATYPE.NE.' ') THEN
         CALL DATMAKAR(IMG, INAX, INAXIS, IATYPE, IADD)
      END IF
C
      CALL CRDPUT (IMG, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
C Add in the standard stuff
C
      CALL DATPUTC (IMG, 'BUNIT', 'JY/BEAM', 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'OBJECT', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'OBJECT', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'INSTRUME', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'INSTRUME', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'TELESCOP', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'TELESCOP', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'OBSERVER', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'OBSERVER', CVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETC (VIS, 'DATE-OBS', CVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         CVALUE = ' '
      END IF
      CALL DATPUTC (IMG, 'DATE-OBS', CVALUE, 1)
C
      CALL SYSDATEC (DATE)
      CALL DATPUTC (IMG, 'DATE-MAP', DATE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETD (VIS, 'OBSRA', DVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         DVALUE = 0.0D0
      END IF
      CALL DATPUTD (IMG, 'OBSRA', DVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETD (VIS, 'OBSDEC', DVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         DVALUE = 0.0D0
      END IF
      CALL DATPUTD (IMG, 'OBSDEC', DVALUE, 1)
C
      IF (ERROR) GOTO 990
      CALL DATGETR (VIS, 'EPOCH', RVALUE, 1, NDUMMY)
      IF(ERROR) THEN
         CALL ERRCANCE
         RVALUE = 0.0
      END IF
      CALL DATPUTR (IMG, 'EPOCH', RVALUE, 1)
C
C Copy history info
C
      CALL HISCOPY (VIS, IMG)
C
      CALL DATSETTP (IMG, 'IMAGE')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
