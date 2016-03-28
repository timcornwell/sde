C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpb.f	1.10	 8/12/91
C
      SUBROUTINE IMGPB (IN, OUT, MODE)
C
CD Primary beam correction routine.
C
C
C	IN	CH*(*)	input	Name of directory entry
C	OUT	CH*(*)	input	Name of directory entry
C	MODE	CH*(*)	input	'APPLY', 'CORRECT'
C Audit trail:
C	Added BIMA telescopes: Airy disk for D = 6.1m
C				T.J.Cornwell	Jan 24 1989
C	Added AIRY telescope type
C				T.J.Cornwell	May 2 1990
C	Added PIXPBCOR: corrupts primary beam
C				M.A.Holdaway	Sep 11 1990
C	Beam Switching controlled by THROWRA, THROWDEC
C				M.A.Holdaway	Jan 10 1991
C	Implemented HEDGETPB to get all PB info
C				M.A.Holdaway	March 28 1991
C	Added SYSDEBUG message about BEAM SWITCHING
C				M.A.Holdaway	May 1 1991
C	Removed seperate pixel level VLA PB application technique
C				M.A.Holdaway	Aug 12 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IN, OUT, MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPB')
C
      CHARACTER*(SYSMXNAM)	STRM2, TELESCOP
      INTEGER		NDUMMY
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		RNAX, CRDRNAX
      INTEGER		INADD, OUTADD, DATADD, PBADD, NPB
      LOGICAL		BEAMTHRO
      REAL		BMX, BMY, RCONST, TELDIAM
      CHARACTER*1	TYPE
C
      REAL		RADMAX
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM)
C
      REAL		PBLEVEL, GAIN1, GAIN2
      CHARACTER*(SYSMXNAM)	AROUTINE, PROUTINE
      DATA	C	/SYSMXDIM*0.0/
      DATA	CBT	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get all relevent PB information
C
      CALL HEDGETPB (IN, IN, IN, .TRUE., TELESCOP, TELDIAM, PBLEVEL, 
     $   PROUTINE, AROUTINE, RADMAX, NPB, PBADD, RCONST, 
     $   C, BEAMTHRO, CBT, BMX, BMY)
C
      CALL DATGETAR (IN, NAX, NAXIS, TYPE, INADD)
      OUTADD = DATADD (OUT)
      RNAX = CRDRNAX(NAX, NAXIS)
C
C Apply primary beam
C
      IF (RNAX.EQ.2) THEN
C
         IF (.NOT. BEAMTHRO) THEN
            CALL PIXPB2D (MEMR(INADD), NAXIS(1), NAXIS(2), 
     1         C(1), C(2), BMX, BMY, 0.0, MODE, NPB, MEMR(PBADD),
     2         RADMAX, MEMR(OUTADD))
         ELSE
C
C Do Beam Switching
C NOTE:  we assume the same beams for ON and OFF here, but this
C is not required by PIXBS2D
C
            GAIN1 = 1.0
            GAIN2 = -1.0
            IF (SYSDEBUG) THEN
               WRITE (MESSAGE, 9374) C(1), C(2), CBT(1), CBT(2)
 9374          FORMAT ('IMGPB: X1,Y1, X2,Y2 ', 4F10.2)
               CALL MSGPUT (MESSAGE, 'D')
            ENDIF
            CALL PIXBS2D (MEMR(INADD), NAXIS(1), NAXIS(2), 
     1         C(1), C(2), CBT(1), CBT(2), BMX, BMY, 0.0, MODE, 
     $         GAIN1, NPB, MEMR(PBADD), RADMAX, 
     $         GAIN2, NPB, MEMR(PBADD), RADMAX, MEMR(OUTADD))
         ENDIF
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Can only treat 2D images')
         GO TO 999
      END IF
C
      IF (ERROR) GOTO 999

C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
