C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgomake.f	1.1    1/25/92
C
      SUBROUTINE IMGOMAKE (RA, DEC, FREQ, CELLSIZE, IMSIZE, SHIFT,
     $   MATYPE, IMG)
C
CD Make a directory entry for an image. Can make 2 or 3-D
C images. SHIFT is the shift in the tangent point in arcseconds i.e.
C this is not a shift on the tangent plane but a shift of the tangent
C point around the celestial sphere. If MATYPE is null then only the
C header is made.
C
C
C	CELLSIZE	REAL(*)	input	Cell size of image in arcsec
C	IMSIZE		INT(*)	input	Image size in pixels
C	SHIFT		REAL	input	Shift in arcseconds
C	MATYPE		CH*1	input	Type of map e.g. 'R' or 'X'
C	IMG		CH*(*)	input	Name of output image
C Audit trail:
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, MATYPE
      DOUBLE PRECISION	RA, DEC, FREQ
      REAL		CELLSIZE(*), SHIFT(*)
      INTEGER		IMSIZE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGOMAKE')
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
C First make RA axis
C
      IATYPE = MATYPE
      INAX = 0
      INAX = INAX + 1
      ITYPE(1) = 'RA---SIN'
      IDELT(INAX) = - CELLSIZE(INAX) / 3600.0
      IRVAL(INAX) = RA + SHIFT(1) /3600.0
      INAXIS(INAX) = IMSIZE(INAX)
      IRPIX(INAX) = FLOAT(IMSIZE(INAX))/2.0
      IROTA(INAX) = 0.0
C
C Next make DEC axis
C
      INAX = INAX + 1
      ITYPE(INAX) = 'DEC--SIN'
      IDELT(INAX) = CELLSIZE(INAX) / 3600.0
      IRVAL(INAX) = DEC + SHIFT(2) / 3600.0
      INAXIS(INAX) = IMSIZE(INAX)
      IRPIX(INAX) = FLOAT(IMSIZE(INAX))/2.0
      IROTA(INAX) = 0.0
C
C Is this a 3-D transform?
C
      IF (IMSIZE(3).NE.1) THEN
          INAX = INAX + 1
          ITYPE(INAX) = 'N----SIN'
          IDELT(INAX) = CELLSIZE(INAX) / 3600.0
          IRVAL(INAX) = 0.0D0
          INAXIS(INAX) = IMSIZE(INAX)
          IRPIX(INAX) = FLOAT(IMSIZE(INAX))/2.0
          IROTA(INAX) = 0.0
      END IF
C
C Now do frequency
C
      INAX = INAX + 1
      ITYPE(INAX) = 'FREQ'
      IRVAL(INAX) = FREQ
      IDELT(INAX) = FREQ
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
C
      CALL DATPUTC (IMG, 'OBJECT', 'NONE', 1)
C
      CALL DATPUTC (IMG, 'INSTRUME', 'NONE', 1)
C
      CALL DATPUTC (IMG, 'TELESCOP', 'NONE', 1)
C
      CALL DATPUTC (IMG, 'OBSERVER', 'GOD', 1)
C
      CALL SYSDATEC (DATE)
      CALL DATPUTC (IMG, 'DATE-OBS', DATE, 1)
      CALL DATPUTC (IMG, 'DATE-MAP', DATE, 1)
C
      CALL DATPUTD (IMG, 'OBSRA', RA, 1)
C
      CALL DATPUTD (IMG, 'OBSDEC', DEC, 1)
C
      CALL DATPUTR (IMG, 'EPOCH', 2000.0, 1)
C
      CALL DATSETTP (IMG, 'IMAGE')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
