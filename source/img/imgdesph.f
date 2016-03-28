C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdesph.f	1.3    11/7/90
C
      SUBROUTINE IMGDESPH (IM3D, IM2D)
C
CD Project a sphere onto the plane X-Y. 
C
C
C	IM3D	CH*(*)	input	Input 3D sphere
C	IM2D	CH*(*)	output	Output 2D plane
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to do Gaussian fitting if BZ defined. Otherwise do
C	SINC interpolation as before.
C				T.J. Cornwell	August 23 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IM3D, IM2D
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDESPH')
C
      INTEGER		INAX, INAXIS(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
C
      INTEGER		NX, NY, NZ
      REAL		CX, CY, CZ, RX, RY, RZ
      INTEGER		IM2DADD, IM3DADD, DATADD, NDUMMY
      CHARACTER*(SYSMXNAM)	DATE, STRM2, INTFN
      REAL		WIDTH, BZ
C
      LOGICAL		DATEXIST, DATEXIAR
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IM3D, INAX, ITYPE, INAXIS, IRVAL, IRPIX,
     1   IDELT, IROTA)
      IF (ERROR) GO TO 990
C
      NX = INAXIS(1)
      NY = INAXIS(2)
      NZ = INAXIS(3)
C
C Find radii of sphere
C
      RX = ABS(180.0 / (4.0 * ATAN(1.0) * IDELT(1)))
      RY = ABS(180.0 / (4.0 * ATAN(1.0) * IDELT(2)))
      RZ = ABS(180.0 / (4.0 * ATAN(1.0) * IDELT(3)))
C
C Find centre of sphere in cube
C
      CX = IRPIX(1)
      CY = IRPIX(2)
      CZ = IRPIX(3) - RZ
C
C Make output image if it does not yet exist
C
      IF (.NOT.DATEXIAR(IM2D)) THEN
         INAXIS(3) = 1
         CALL DATMAKAR(IM2D, 2, INAXIS, 'R', IM2DADD)
         CALL HEDCOPY(IM3D, IM2D)
         IRPIX(3) = 1.0
         CALL CRDPUT (IM2D, INAX, ITYPE, INAXIS, IRVAL, IRPIX,
     1      IDELT, IROTA)
         CALL SYSDATEC (DATE)
         CALL DATPUTC (IM2D, 'DATE-MAP', DATE, 1)
      END IF
C
C Find interpolation function
C
      IF (DATEXIST(STRM2(IM3D, 'BZ'))) THEN
         CALL DATGETR (IM3D, 'BZ', BZ, 1, NDUMMY)
         WIDTH = BZ / IDELT(3)
         INTFN = 'GAUSS'
      ELSE
         WIDTH = 0.0
         INTFN = 'SINC'
      END IF
C
C Do reprojection
C
      IM2DADD = DATADD(IM2D)
      IM3DADD = DATADD(IM3D)
      CALL PIXDESPH (MEMR(IM3DADD), NX, NY, NZ, MEMR(IM2DADD), 
     1   RX, RY, RZ, CX, CY, CZ, INTFN, WIDTH)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
