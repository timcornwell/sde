C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrrdcl.f	1.1	 3/15/91
C
      SUBROUTINE PIXRRDCL (IMAGE, NX, NY, RX, RY, DX, DY,
     $   MINRAD, MAXRAD, CLIP)
C
CD CLIPS the IMAGE to CLIP outside of MINRAD, MAXRAD degrees
C
C	IMAGE	R ARR	in	IMAGE to be clipped
C	NX	INT	in	X size of the IMAGE
C	RX	REAL	in	reference pixel
C	DX	REAL	in	pixel size, X (deg)
C	DY	REAL	in	pixel size, Y (deg)
C	MAXRAD	REAL	in	radius outside which we clip
C	MINRAD	REAL	in	radius inside which we clip
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
      REAL		MAXRAD, MINRAD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRRDCL')
C
      CHARACTER*(SYSMXNAM)	STRREAL
      INTEGER		IX, IY
      REAL		RAD, XFACT(1024), YFACT(1024), RAD2,
     $   		MINRAD2, MAXRAD2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RAD = MAX( SQRT( ((NX-RX)*DX)**2 + ((NY-RY)*DY)**2 ),
     $           SQRT( ((   RX)*DX)**2 + ((   RY)*DY)**2 )  )
      RAD = MAX( SQRT( ((NX-RX)*DX)**2 + ((   RY)*DY)**2 ), RAD)
      RAD = MAX( SQRT( ((   RX)*DX)**2 + ((NY-RY)*DY)**2 ), RAD)
C
C Handle exceptional cases
C
      IF (MAXRAD .LT. MINRAD) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'MINRAD is greater than MAXRAD')
         GOTO 990
      ENDIF
      IF (MAXRAD .GT. RAD) THEN
         CALL MSGPUT ('MAXRAD too big: no outer clipping', 'W')
         IF (MINRAD .LE. 0.)  GOTO 999
      ENDIF
      IF (MINRAD .GT. RAD) THEN
         CALL MSGPUT ('MINRAD too big: entire image clipped', 'W')
         DO 4 IY = 1, NY
            DO 3 IX = 1, NX
               IMAGE(IX, IY) = CLIP
 3          CONTINUE
 4       CONTINUE
         GOTO 999
      ENDIF
C
C Actually do some real clipping
C
      MINRAD2 = MINRAD**2
      MAXRAD2 = MAXRAD**2
      DO 5 IY = 1, NY
         YFACT(IY) = ((IY - RY)*DY)**2
 5    CONTINUE
      DO 8 IX = 1, NX
         XFACT(IX) = ((IX - RX)*DX)**2
 8    CONTINUE
      DO 20 IY = 1, NY
         DO 10 IX = 1, NX
            RAD2 = XFACT(IX) + YFACT(IY) 
            IF (RAD2 .GT. MAXRAD2) THEN
               IMAGE(IX, IY) = CLIP
            ELSE IF (RAD2 .LT. MINRAD2) THEN
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




