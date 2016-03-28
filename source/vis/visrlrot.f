C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrlrot.f	1.1	 5/4/93
C
      SUBROUTINE VISRLROT (VIS, THETA)
C
CD Rotates the RL phase of the polarized visibilities
C
C	VIS	CH*(*)	inp	Visibility Database
C	THETA	REAL	inp	Angle to rotate RL vis by
C
C Audit trail:
C	New
C				M.A.Holdaway	April 28 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS
      REAL		THETA
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRLROT')
C
      COMPLEX		ONEX, JX, MJX, HALFX, HALFJX, MHALFJX
      REAL		MYROT
      CHARACTER*(SYSMXNAM)	STRM3
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      ONEX = CMPLX(1.0, 0.0)
      JX   = CMPLX(0.0, 1.0)
      MJX  = CMPLX(0.0,-1.0)
      HALFX= CMPLX(0.5, 0.0)
      HALFJX = CMPLX(0.0, 0.5)
      MHALFJX = CMPLX(0.0, -0.5)
      CALL ARRXLC (STRM3(VIS, 'OBS', 'Q/VIS'), ONEX, 
     $             STRM3(VIS, 'OBS', 'U/VIS'), JX, 'VISRLROT-RL')
      CALL ARRXLC (STRM3(VIS, 'OBS', 'Q/VIS'), ONEX, 
     $             STRM3(VIS, 'OBS', 'U/VIS'), MJX, 'VISRLROT-LR')
C
      MYROT = THETA
      CALL ARRXROT ('VISRLROT-RL', MYROT, 'VISRLROT-RL')
      MYROT = -THETA
      CALL ARRXROT ('VISRLROT-LR', MYROT, 'VISRLROT-LR')
C
      CALL ARRXLC ('VISRLROT-RL', HALFX, 'VISRLROT-LR', HALFX,
     $   STRM3(VIS, 'OBS', 'Q/VIS') )
      CALL ARRXLC ('VISRLROT-RL', MHALFJX, 'VISRLROT-LR', HALFJX,
     $   STRM3(VIS, 'OBS', 'U/VIS') )
C
      CALL DATDELET ('VISRLROT-RL')
      CALL DATDELET ('VISRLROT-LR')

C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



