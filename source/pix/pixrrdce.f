C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrrdce.f	1.1	 3/15/93
C
      SUBROUTINE PIXRRDCE (IMAGE, NX, NY, RX, RY, DX, DY,
     $   MINRAD, MAXRAD, CLIP)
C
CD CLIPS the IMAGE to CLIP outside of MINRAD, MAXRAD degrees; ELIPSES, not CIRC
C  Right now, this ignores PA, makes Major Axis in Y direction
C
C	IMAGE	R ARR	in	IMAGE to be clipped
C	NX	INT	in	X size of the IMAGE
C	RX	REAL	in	reference pixel
C	DX	REAL	in	pixel size, X (deg)
C	DY	REAL	in	pixel size, Y (deg)
C	MAXRAD	REAL(3)	in	radius outside which we clip
C				 Maj, Min, and PA
C	MINRAD	REAL(3)	in	radius inside which we clip
C				 Maj, Min, and PA
C	CLIP	REAL	in	the value to which we CLIP
C
C Audit trail:
C	Original version: 
C				M.A.Holdaway	Oct 11 1989
C	Added MINRAD.   Unclipped for MINRAD < R < MAXRAD
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY
      REAL		IMAGE(NX, *), CLIP, RX, RY, DX, DY
      REAL		MAXRAD(3), MINRAD(3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRRDCE')
C
      INTEGER		IX, IY
      REAL		XMINFACT(1024), XMAXFACT(1024), YFACT(1024)
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Actually do some real clipping
C
      IF (NX .GT. 1024) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Increase 1024')
         GOTO 990
      ENDIF
C
      DO 5 IY = 1, NY
         YFACT(IY) = ((IY - RY)*DY)**2
 5    CONTINUE
      DO 8 IX = 1, NX
         XMAXFACT(IX) = MAXRAD(1)**2*(1.0 - ((IX - RX)*DX/MAXRAD(2))**2)
         XMINFACT(IX) = MINRAD(1)**2*(1.0 - ((IX - RX)*DX/MINRAD(2))**2)
 8    CONTINUE
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            IF (YFACT(IY) .GT. XMAXFACT(IX)) THEN
               IMAGE(IX, IY) = CLIP
            ELSE IF (YFACT(IY) .LT. XMINFACT(IX)) THEN
               IMAGE(IX, IY) = CLIP
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




