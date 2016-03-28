C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdput.f	1.3    11/7/90
C
      SUBROUTINE CRDPUT (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
CD PUT coordinate information for a directory entry
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	NAX	INT	input 	Number of real axes
C	TYPE	CH*(*)	input 	Types of axes
C	NAXIS	INT(*)	input 	Number of pixels for each axis
C	RVAL	DBLE(*)	input 	Reference values
C	RPIX	REAL(*)	input 	Reference pixels
C	DELT	REAL(*)	input 	Coordinate increments
C	ROTA	REAL(*)	input 	Rotations of coordinates
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	NAME
      INTEGER		NAX, NAXIS(*)
      DOUBLE PRECISION	RVAL(*)
      REAL		RPIX(*), DELT(*), ROTA(*)
      CHARACTER*(*)	TYPE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDPUT')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	ANAME, LNNAME
      CHARACTER		STRM2*(SYSMXNAM)
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
C PUT number of pixels
C
      IF(.NOT.DATEXIST(NAME)) THEN
         CALL DATCREAT (NAME)
         CALL DATCREAT (STRM2(NAME, 'ARRAY'))
         CALL DATPUTI (STRM2(NAME, 'ARRAY'), 'NAXIS', NAXIS, NAX)
      ELSE
         IF(DATEXIST(STRM2(NAME, 'DATLNARR'))) THEN
            CALL DATGETC (NAME, 'DATLNARR', LNNAME, 1, NDUMMY)
            ANAME = STRM2(LNNAME, 'ARRAY')
         ELSE
            ANAME = STRM2 (NAME, 'ARRAY')
         END IF
         CALL DATCREAT (ANAME)
         CALL DATPUTI (ANAME, 'NAXIS', NAXIS,  NAX)
      END IF
C
C PUT types of axes
C
      CALL DATPUTC (NAME, 'CTYPE', TYPE, NAX)
C
C PUT reference value
C
      CALL DATPUTD (NAME, 'CRVAL', RVAL, NAX)
C
C PUT reference pixel
C
      CALL DATPUTR (NAME, 'CRPIX', RPIX, NAX)
C
C PUT coordinate increment
C
      CALL DATPUTR (NAME, 'CDELT', DELT, NAX)
C
C PUT coordinate rotation
C
      CALL DATPUTR (NAME, 'CROTA', ROTA, NAX)
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
