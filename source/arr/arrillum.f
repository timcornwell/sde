C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrillum.f	1.1	 03 Aug 1995
C
      SUBROUTINE ARRILLUM (IMAGE, NX, NY, RX, RY, DX, DY, MAXRAD, 
     $     POWER, PED)
C
CD 
C
C	IMAGE	R ARR	in/out	IMAGE to be clipped
C	NX	INT	in	X size of the IMAGE
C	RX	REAL	in	reference pixel
C	DX	REAL	in	pixel size, X (deg)
C	DY	REAL	in	pixel size, Y (deg)
C	MAXRAD	REAL	in	r = rad/maxrad (rad in units of DX)
C	POWER	REAL	in	(1-r^2)^POWER
C	PED	REAL	in	Pedestal on which (1-r^2)^power sits
C
C Audit trail:
C	Original version: 
C				M.A.Holdaway	Aug 3 1995
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY
      REAL		IMAGE(NX, *), RX, RY, DX, DY
      REAL		MAXRAD, PED, POWER
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRILLUM')
C
      INTEGER		IX, IY
      REAL		RAD2, MAXRAD2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Actually do some real clipping
C
      MAXRAD2 = MAXRAD**2
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            RAD2 = ((IY - RY)*DY)**2 + ((IX - RX)*DX)**2
            IF (RAD2 .GT. MAXRAD2) THEN
               IMAGE(IX, IY) = 0.0
            ELSE
               IMAGE(IX, IY) = (1 - RAD2/MAXRAD2)**POWER *
     $              (1 - PED)  +  PED
            ENDIF
 10      CONTINUE
 20   CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END




