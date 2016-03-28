C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpbclp.f	1.1    8/9/91
C
      SUBROUTINE IMGPBCLP (IMAGE, PBLEV, SENCE)
C
CD clips image below(above) the PBLEV primary beam level
C
C	IMAGE	CH*(*)	input	Name of input image
C	PBLEV	REAL	input	PB clip level (like .10)
C	SENCE	CH*(*)	input	'BELOW' or 'ABOVE'
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 8 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, SENCE
      REAL		PBLEV
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPBCLP')
C 
      CHARACTER*(SYSMXNAM)      TELESCOP
      INTEGER           NDUMMY
      INTEGER           PBADD, NPB
      LOGICAL           BEAMTHRO
      REAL              BMX, BMY, RCONST, TELDIAM
      REAL              RADMAX
      REAL              C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM)
      REAL              PBERR, DELT(SYSMXDIM) 
      REAL		RADIUS
      REAL		CRMIN, CRMAX
      CHARACTER*(SYSMXNAM)      AROUTINE, PROUTINE
      DATA      C       /SYSMXDIM*0.0/
      DATA      CBT     /SYSMXDIM*0.0/
      DATA      PC      /SYSMXDIM*0.0/

C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL HEDGETPB (IMAGE, IMAGE, IMAGE, .TRUE., TELESCOP, 
     $   TELDIAM, PBERR, PROUTINE, AROUTINE, RADMAX, NPB, PBADD, 
     $   RCONST, C, BEAMTHRO, CBT, BMX, BMY)
C
      CALL DATGETR (IMAGE, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL PIXPBRAD (MEMR(PBADD), NPB, RADMAX, BMX, DELT(1), PBLEV, 
     $   RADIUS)
C
      CALL CRDRP2PC (IMAGE)
C
      IF (SENCE .EQ. 'ABOVE') THEN
         CRMIN = RADIUS
         CRMAX = 9E+20
         CALL ARRRDCLP (IMAGE, CRMIN, CRMAX, 0.0)
      ELSE
         CRMIN = 0.0
         CRMAX = RADIUS
         CALL ARRRDCLP (IMAGE, CRMIN, CRMAX, 0.0)
      ENDIF
C
      CALL CRDRPCEN (IMAGE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
