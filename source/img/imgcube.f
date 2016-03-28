C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcube.f	1.3    11/7/90
C
      SUBROUTINE IMGCUBE (IN, OUT, NZ, TYPEZ, RVALZ, RPIXZ, DELTZ, 
     1   ROTAZ)
C
CD Make a cube from a 2-d image
C
C Arguments : CALL IMGCUBE (IN, OUT, NZ, TYPEZ, RVALZ, RPIXZ, 
C
C	IN	CH*(*)	input	Name of input image
C	OUT	CH*(*)	input	Name of output image
C	NZ	INT	input	Number of z planes
C	TYPEZ	CHAR*(*)	input	Name of z plane
C	RVALZ	DOUBLE	input	Reference value
C	RPIXZ	REAL	input	Reference pixel
C	DELTZ	REAL	input	Coordinate increment
C	ROTAZ	REAL	input	Coordinate rotation
C Audit trail:
C	New subroutine
C					T.J. Cornwell Feb 22 1989
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT, TYPEZ
      INTEGER		NZ
      REAL		DELTZ, RPIXZ, ROTAZ
      DOUBLE PRECISION	RVALZ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCUBE')
C
      INTEGER		INAX, IADD, INAXIS(SYSMXDIM)
      CHARACTER*1	IATYPE
      CHARACTER*8	ITYPE(SYSMXDIM)
      REAL		IRPIX(SYSMXDIM), IDELT(SYSMXDIM),
     1			IROTA(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		ADD, IAX
      CHARACTER*1	ATYPE
      LOGICAL		DATEXIST
C======================================================================
      IF (ERROR) GO TO 999
C
      IF(.NOT.DATEXIST(IN)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Original does not exist')
         GO TO 999
      END IF
C
      IF(DATEXIST(OUT)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Target already exists')
         GO TO 999
      END IF
C
      CALL DATCREAT (OUT)
C
      CALL HEDCOPY (IN, OUT)
C
      CALL DATGETAR (IN, INAX, INAXIS, ATYPE, ADD)
      CALL CRDGET (IN, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
      INAX = MIN(SYSMXDIM, INAX + 1)
      DO 10 IAX = 4, INAX
         INAXIS(IAX) = INAXIS(IAX-1)
         ITYPE(IAX) = ITYPE(IAX-1)
         IRVAL(IAX) = IRVAL(IAX-1)
         IRPIX(IAX) = IRPIX(IAX-1)
         IDELT(IAX) = IDELT(IAX-1)
         IROTA(IAX) = IROTA(IAX-1)
  10  CONTINUE
      INAXIS(3) = NZ
      IRVAL(3) = RVALZ
      ITYPE(3) = TYPEZ
      IRPIX(3) = RPIXZ
      IDELT(3) = DELTZ
      IROTA(3) = ROTAZ
      CALL DATMAKAR (OUT, INAX, INAXIS, ATYPE, ADD)
      CALL CRDPUT (OUT, INAX, ITYPE, INAXIS, IRVAL, IRPIX, IDELT, 
     1   IROTA)
C
      CALL HISCOPY (IN, OUT)
C
      IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
