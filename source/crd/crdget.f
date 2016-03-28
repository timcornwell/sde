C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdget.f	1.3    11/7/90
C
      SUBROUTINE CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
CD Get coordinate information for a directory entry. An error is
C generated if the NAXIS* information cannot be found. Any other
C information may be missing.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	NAX	INT	output	Number of real axes
C	TYPE	CH*(*)	output	Types of axes
C	NAXIS	INT(*)	output	Number of pixels for each axis
C	RVAL	DBLE(*)	output	Reference values
C	RPIX	REAL(*)	output	Reference pixels
C	DELT	REAL(*)	output	Coordinate increments
C	ROTA	REAL(*)	output	Rotations of coordinates
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)     NAME
      INTEGER           NAX, NAXIS(*)
      DOUBLE PRECISION  RVAL(*)
      REAL              RPIX(*), DELT(*), ROTA(*)
      CHARACTER*8       TYPE(*)
C
      CHARACTER*(*)     ROUTINE
      PARAMETER		(ROUTINE = 'CRDGET')
C
      INTEGER           NDUMMY, IAX
      CHARACTER*(SYSMXNAM)	ANAME, LNNAME
      CHARACTER		STRM2*(SYSMXNAM)
      LOGICAL		DATEXIST
C=====================================================================
      IF(ERROR) GO TO 999
C
      IF(.NOT.DATEXIST(NAME)) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Missing array')
         GO TO 999
      END IF
C
      DO 10 IAX = 1, SYSMXDIM
         TYPE(IAX) = ' '
         RVAL(IAX) = 0.0D0
         RPIX(IAX) = 0.0
         DELT(IAX) = 0.0
         ROTA(IAX) = 0.0
         NAXIS(IAX) = 1
 10   CONTINUE
C
      IF (ERROR) GO TO 999
C
C Get number of pixels on each axis, etc.
C
      IF(DATEXIST(STRM2(NAME, 'DATLNARR'))) THEN
         IF(ERROR) GO TO 990
         CALL DATGETC (NAME, 'DATLNARR', LNNAME, 1, NDUMMY)
         CALL ERRCANCE
         ANAME = STRM2(LNNAME, 'ARRAY')
      ELSE
         ANAME = STRM2 (NAME, 'ARRAY')
      END IF
      CALL DATGETI (ANAME, 'NAXIS', NAXIS, SYSMXDIM, NAX)
C
C Get types of axes
C
      IF (DATEXIST(STRM2(NAME, 'CTYPE'))) THEN
         CALL DATGETC (NAME, 'CTYPE', TYPE, SYSMXDIM, NDUMMY)
      END IF
C
C Get reference value
C
      IF (DATEXIST(STRM2(NAME, 'CRVAL'))) THEN
         CALL DATGETD (NAME, 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
      END IF
C
C Get reference pixel
C
      IF (DATEXIST(STRM2(NAME, 'CRPIX'))) THEN
         CALL DATGETR (NAME, 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      END IF
C
C Get coordinate increment
C
      IF (DATEXIST(STRM2(NAME, 'CDELT'))) THEN
         CALL DATGETR (NAME, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      END IF
C
C Get coordinate rotation
C
      IF (DATEXIST(STRM2(NAME, 'CROTA'))) THEN
         CALL DATGETR (NAME, 'CROTA', ROTA, SYSMXDIM, NDUMMY)
      END IF
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
