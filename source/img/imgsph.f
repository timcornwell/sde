C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsph.f	1.3    11/7/90
C
      SUBROUTINE IMGSPH (IM2D, IM3D, NZ, CELLZ)
C
CD Expand into a sphere from the plane X-Y. 
C
C
C	IM2D	CH*(*)	input	Input 2D sphere
C	IM3D	CH*(*)	output	Output 3D plane
C	NZ	INT	input	Number of pixels on Z-axis
C	CELLZ	REAL	input	Cell spacing in Z: degrees
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NZ
      REAL		CELLZ
      CHARACTER*(*)	IM2D, IM3D
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSPH')
C
      INTEGER		INAX, INAXIS(SYSMXDIM), IAX
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
C
      INTEGER		NX, NY
      REAL		CX, CY, CZ, RX, RY, RZ
      INTEGER		IM2DADD, IM3DADD, DATADD
      CHARACTER*(SYSMXNAM)	DATE
      LOGICAL		DATEXIST
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Expand coordinates
C
      IF (DATEXIST(IM3D)) THEN
         CALL CRDGET (IM3D, INAX, ITYPE, INAXIS, IRVAL, IRPIX,
     1      IDELT, IROTA)
         NZ = INAXIS(3)
         CELLZ = IDELT(3)
         IM3DADD = DATADD(IM3D)
      ELSE
         CALL CRDGET (IM2D, INAX, ITYPE, INAXIS, IRVAL, IRPIX,
     1      IDELT, IROTA)
         IF (ITYPE(3).NE.'N----SIN') THEN
            DO 10 IAX = SYSMXDIM-1, 3, -1
               ITYPE(IAX+1)  = ITYPE(IAX)
               INAXIS(IAX+1) = INAXIS(IAX)
               IRVAL(IAX+1)  = IRVAL(IAX)
               IRPIX(IAX+1)  = IRPIX(IAX)
               IDELT(IAX+1)  = IDELT(IAX)
               IROTA(IAX+1)  = IROTA(IAX)
 10         CONTINUE
            ITYPE(3) = 'N----SIN'
            IRVAL(3) = 0.0D0
            IRPIX(3) = 1.0
            IROTA(3) = 0.0
            INAX = INAX + 1
         END IF
         INAXIS(3) = NZ
         IDELT(3) = CELLZ
         CALL DATMAKAR(IM3D, 3, INAXIS, 'R', IM3DADD)
         CALL HEDCOPY(IM2D, IM3D)
         CALL CRDPUT (IM3D, INAX, ITYPE, INAXIS, IRVAL, IRPIX,
     1      IDELT, IROTA)
         CALL SYSDATEC (DATE)
         CALL DATPUTC (IM3D, 'DATE-MAP', DATE, 1)
      END IF
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
C Do reprojection
C
      IM2DADD = DATADD(IM2D)
      CALL PIXSPH (MEMR(IM2DADD), NX, NY, NZ, MEMR(IM3DADD), 
     1   RX, RY, RZ, CX, CY, CZ)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
