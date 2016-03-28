C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdrotat.f	1.1	 5/4/93
C
      SUBROUTINE VSDROTAT (PDT, ROTANG)
C
CD Rotates D terms by ROTANG degrees.  Rotation is CCW on Dr + Dl* plots
C
C	PDT	CH*(*)	inp	D Terms Database
C	ROTANG	REAL	inp	Angle to rotate D terms by, degrees
C
C Audit trail:
C	New
C				M.A.Holdaway	April 14 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT
      REAL		ROTANG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDROTAT')
C
      INTEGER		DLADD, DRADD
      INTEGER		NAX, NAXIS(SYSMXDIM), NT
      CHARACTER*1	T
      REAL		MYROT
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, T, DRADD)
      CALL DATGETAR (STRM2(PDT, 'DL'), NAX, NAXIS, T, DLADD)
      IF (T .NE. 'X') THEN
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'D Terms not X')
         GOTO 999
      ENDIF
      NT = NAXIS(1) * NAXIS(2)
C
      MYROT = ROTANG
      CALL ARRX2AP (STRM2(PDT, 'DR'), 'VSDROT-AMP', 'VSDROT-PHA')
      CALL ARRSCALE ('VSDROT-PHA', 1.0, MYROT, 'VSDROT-PHA')
      CALL ARRAP2X ('VSDROT-AMP', 'VSDROT-PHA', STRM2(PDT, 'DR') )
      CALL DATDELET ('VSDROT-AMP')
      CALL DATDELET ('VSDROT-PHA')
C
      MYROT = -ROTANG
      CALL ARRX2AP (STRM2(PDT, 'DL'), 'VSDROT-AMP', 'VSDROT-PHA')
      CALL ARRSCALE ('VSDROT-PHA', 1.0, MYROT, 'VSDROT-PHA')
      CALL ARRAP2X ('VSDROT-AMP', 'VSDROT-PHA', STRM2(PDT, 'DL') )
      CALL DATDELET ('VSDROT-AMP')
      CALL DATDELET ('VSDROT-PHA')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



