C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgtype.f	1.3	 8/5/91
C
      SUBROUTINE IMGTYPE (IMAGE, TYPE)
C
CD From CRD TYPEs, find out if this is an X IMAGE or a GRIDDED VIS
C
C	IMAGE	CH*(*)	in	Name of image to test
C	TYPE	CH*(*)	out	'IMAGE' or 'VIS' or 'UNKNOWN'
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Nov 30 1990
C	Added RX,RY,RZ as "IMAGE", KX,KY,KZ as "VIS"
C				M.A.Holdaway	May 10 1991
C       Added DTX, DTY, FRX, FRY to types known
C                               R.G. Marson     Aug 4 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, TYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGTYPE')
C
      REAL		RPIX(SYSMXDIM), ROT(SYSMXDIM), DELT(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      CHARACTER*8	CTYPE(SYSMXDIM), CC
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      TYPE = 'UNKNOWN'
      CALL CRDGET (IMAGE, NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROT)
      DO 100 I = 1, NAX
         CC = CTYPE(I)
         IF (CC(1:2) .EQ. 'RA' .OR. CC(1:3) .EQ. 'DEC') THEN
            TYPE = 'IMAGE'
            GO TO 200
         ELSE IF (CC(1:2) .EQ. 'RX' .OR. CC(1:2) .EQ. 'RY') THEN
            TYPE = 'IMAGE'
            GO TO 200
         ELSE IF (CC(1:2) .EQ. 'KX' .OR. CC(1:2) .EQ. 'KY') THEN
            TYPE = 'VIS'
            GO TO 200
         ELSE IF (CC(1:2) .EQ. 'UU' .OR.
     $         CC(1:2) .EQ. 'VV' .OR. CC(1:2).EQ. 'WW') THEN
            TYPE = 'VIS'
            GOTO 200
         ELSE IF (CC(1:3) .EQ. 'DTX' .OR. CC(1:3) .EQ. 'DTY') THEN
            TYPE = 'IMAGE'
            GOTO 200
         ELSE IF(CC(1:3) .EQ. 'FRX' .OR. CC(1:3) .EQ. 'FRY') THEN
            TYPE = 'VIS'
            GOTO 200
         ENDIF
 100  CONTINUE
 200  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
