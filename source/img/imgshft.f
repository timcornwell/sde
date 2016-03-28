C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgshft.f	1.3    11/7/90
C
      SUBROUTINE IMGSHFT (IN, OUT, SHIFT)
C
CD Shift the array by SHIFT pixels in each direction
C 
C
C
C
C	IN	CH*(*)	input	Image IN
C	OUT	CH*(*)	input	Image OUT
C	SHIFT	INT*(*)	input	Shift, in PIXELS
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 7 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      INTEGER		SHIFT(SYSMXDIM)
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'IMGSHFT')
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER		ADDIN, ADDOUT, ADDTMP
      CHARACTER*1       T
      CHARACTER*8	TYPE(SYSMXDIM)
C
      LOGICAL		DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, T, ADDIN)
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
      IF (DATEXIST(OUT)) THEN
         CALL DATGETAR (IN, NAX, NAXIS, T, ADDOUT)
         CALL CRDCHECK (IN, OUT)
         IF (ERROR) GOTO 999
      ELSE
         CALL DATMAKAR (OUT, NAX, NAXIS, T, ADDOUT)
      ENDIF
C
      IF (T.EQ. 'R') THEN
            CALL DATMAKAR ('STemp', NAX, NAXIS, T, ADDTMP)
            CALL ARRRSHFT (MEMR(ADDIN), MEMR(ADDOUT), MEMR(ADDTMP),
     $         NAXIS, DELT, SHIFT)
            CALL DATDELET ('STemp')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'No Code for non-REAL images')
         GOTO 990
      ENDIF         
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
