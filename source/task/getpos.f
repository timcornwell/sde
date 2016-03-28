C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)getpos.f	1.1	 9/23/94
C
      SUBROUTINE SDEMAIN
C
CD Program to get Long, Lat (or RA DEC) from DEM and Pixel Positions
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sep 22 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GETPOS')
C
      CHARACTER*(SYSMXNAM)	IMGFILE, POSFILE, DEGOUT, DMSOUT
      LOGICAL			RELPIX
      CHARACTER*1		PTYPE
      INTEGER           NAX, NAXIS(SYSMXDIM), NDUMMY
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      INTEGER		PNAX, PNAXIS(SYSMXDIM), XADD, YADD, I
C
      INTEGER		D1, D2, M1, M2, SIGN
      REAL		S1, S2
      REAL		PIXEL(SYSMXDIM)
      DOUBLE PRECISION	WORLD(SYSMXDIM)
C==================================================================
C
      CALL MSGWELCO ('I find positions')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('PositionFile', POSFILE, 1, NDUMMY)
      CALL USRGETL ('RelativePixels', RELPIX, 1, NDUMMY)
      CALL USRGETC ('DegreeOut', DEGOUT, 1, NDUMMY)
      CALL USRGETC ('DMSOut', DMSOUT, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMGFILE, ' ')
      CALL FILGETN  ('Positions', POSFILE)
      IF (ERROR) GO TO 999
C
       CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
       CALL DATGETAR ('Positions/NX', PNAX, PNAXIS, PTYPE, XADD)
       CALL DATGETAR ('Positions/NY', PNAX, PNAXIS, PTYPE, YADD)
C
       MESSAGE='Antenna Positions for '//POSFILE(1:20)//IMGFILE(1:20)
       IF (DEGOUT .NE. ' ') THEN
          CALL TXTOPEN ('DegOut', DEGOUT, 'WRITE')
          CALL TXTWRITE ('DegOut', MESSAGE)
       ENDIF
       IF (DMSOUT .NE. ' ') THEN
          CALL TXTOPEN ('DMSOut', DMSOUT, 'WRITE')
          CALL TXTWRITE ('DMSOut', MESSAGE)
       ENDIF
C
       DO 100 I = 1, PNAXIS(1)
          IF (RELPIX) THEN
             PIXEL(1) = RPIX(1) + MEMR(XADD + I-1)
             PIXEL(2) = RPIX(2) + MEMR(YADD + I-1)
          ELSE
             PIXEL(1) = MEMR(XADD + I-1)
             PIXEL(2) = MEMR(YADD + I-1)
          ENDIF
          CALL CRDDPTOW ('Image', PIXEL, WORLD)
          IF (DEGOUT .NE. ' ') THEN
             WRITE (MESSAGE, 1010) I, WORLD(1), WORLD(2)
 1010        FORMAT (I4, F17.9, F17.9)
             CALL TXTWRITE ('DegOut', MESSAGE)
          ENDIF
          IF (DMSOUT .NE. ' ') THEN
             D1 = INT(WORLD(1))
             M1 = INT( (WORLD(1)-D1)*60)
             S1 = ((WORLD(1)-D1)*60-M1)*60
             SIGN = WORLD(2)/ABS(WORLD(2))
             WORLD(2) = ABS(WORLD(2))
             D2 = INT(WORLD(2))
             M2 = INT( (WORLD(2)-D2)*60)
             S2 = ((WORLD(2)-D2)*60-M2)*60
             D2 = D2 * SIGN
             WORLD(2) = WORLD(2) * SIGN
             WRITE (MESSAGE, 1020) I, D1, M1, S1, D2, M2, S2
 1020        FORMAT(I4, 4X, I3, I3, F9.4, 4X, I3, I3, F9.4)
             CALL TXTWRITE ('DMSOut', MESSAGE)
          ENDIF
 100   CONTINUE
C
       IF (DEGOUT .NE. ' ') THEN
          CALL TXTCLOSE ('DegOut')
       ENDIF
       IF (DMSOUT .NE. ' ') THEN
          CALL TXTCLOSE ('DMSOut')
       ENDIF
C
 999  CONTINUE
      END



