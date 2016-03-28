C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmake0.f	1.1    12/11/92
C
      SUBROUTINE IMGMAKE0 (IMG, IMSIZE, CELLSIZE, SHIFT, RA, DEC, FREQ,
     $   MATYPE)
C
CD Make a directory entry for an image. Can make 1, 2 or 3-D
C images. SHIFT is the shift in the tangent point in arcseconds i.e.
C this is not a shift on the tangent plane but a shift of the tangent
C point around the celestial sphere. If MATYPE is null then only the
C header is made.
C
C	IMG		CH*(*)	input	Name of output image
C	IMSIZE		INT(*)	input	Image size in pixels
C	CELLSIZE	REAL(*)	input	Cell size of image in arcsec
C	SHIFT		REAL(*)	input	Shift in arc seconds
C	RA		DOUBLE	input	RA in degrees
C	DEC		DOUBLE	input	Dec in degrees
C	FREQ		DOUBLE	input	Frequency in Hz
C	MATYPE		CH*1	input	Type of map e.g. 'R' or 'X'
C
C Audit trail:
C	Cloned from IMGMAKE 1.6
C				D.S.Briggs	Oct 15 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, MATYPE
      REAL		CELLSIZE(*), SHIFT(*)
      DOUBLE PRECISION	RA, DEC, FREQ
      INTEGER		IMSIZE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMAKE0')
C
      INTEGER		INAX, IADD, INAXIS(SYSMXDIM)
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	DATE
C==================================================================
      IF (ERROR) GO TO 999
C
C First make an RA axis
C
      IATYPE = MATYPE
      INAX = 1
C
C Use AIPS convention for RA axis
C
      ITYPE(INAX) = 'RA---SIN'
      IDELT(INAX) = - CELLSIZE(INAX) / 3600.0
      IRVAL(INAX) = RA + SHIFT(1) / 3600.D0
      INAXIS(INAX) = IMSIZE(INAX)
      IRPIX(INAX) = (IMSIZE(INAX)+1)/2
      IROTA(INAX) = 0.0
C
C DEC axis
C
      IF (IMSIZE(2).NE.1) THEN
         INAX = INAX + 1
         ITYPE(INAX) = 'DEC--SIN'
         IDELT(INAX) = CELLSIZE(INAX) / 3600.0
         IRVAL(INAX) = DEC + SHIFT(2) / 3600.D0
         INAXIS(INAX) = IMSIZE(INAX)
         IRPIX(INAX) = (IMSIZE(INAX)+1)/2
         IROTA(INAX) = 0.0
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
          IROTA(INAX) = 0.0
      END IF
C
C Now do a frequency axis, since some things die without it
C
      INAX = INAX + 1
      ITYPE(INAX) = 'FREQ'
      IRVAL(INAX) = FREQ
      IDELT(INAX) = FREQ / 10.0
      INAXIS(INAX) = 1
      IRPIX(INAX) = 1.0
      IROTA(INAX) = 0.0
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
      CALL DATPUTC (IMG, 'OBJECT', ' ', 1)
      CALL DATPUTC (IMG, 'INSTRUME', ' ', 1)
      CALL DATPUTC (IMG, 'TELESCOP', ' ', 1)
      CALL DATPUTC (IMG, 'OBSERVER', ' ', 1)
      CALL DATPUTC (IMG, 'DATE-OBS', ' ', 1)
      CALL SYSDATEC (DATE)
      CALL DATPUTC (IMG, 'DATE-MAP', DATE, 1)
      CALL DATPUTD (IMG, 'OBSRA', RA, 1)
      CALL DATPUTD (IMG, 'OBSDEC', DEC, 1)
      CALL DATPUTR (IMG, 'EPOCH', 0.0, 1)
      CALL DATSETTP (IMG, 'IMAGE')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
