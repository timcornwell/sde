C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipcalav.f	1.1    3/26/93
C
      SUBROUTINE TIPCALAV (DB, STIME, CTIME, SKYWT, CALWT)
C
CD We calibrate Allan Variance, Structure Functions
C
C	DB	CH*(*)	inp	Tipper DataBase
C	STIME	INT(2)	inp	SKY Selection
C	CTIME	INT	inp	Calibrator selection
C	SKYWT	REAL	inp	Weight of CAL run when correcting SKY
C	CALWT	REAL	inp	Weight of CAL run when correcting CAL
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	23 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	DB
      INTEGER		STIME(2), CTIME
      REAL		CALWT, SKYWT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPCALAV')
C
      INTEGER		I1, I2, I, NDUMMY
      CHARACTER*1	TYPE
      LOGICAL		ISACALIB
      INTEGER		NAX, NAXIS(SYSMXDIM), LADD
      CHARACTER*(SYSMXNAM)	CALNAME, ANAME
C
      REAL			WEIGHT2
      CHARACTER*(SYSMXNAM)	STRINT, STRM2
      LOGICAL			DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
      CALNAME = STRM2(DB, STRINT(CTIME))
      IF (.NOT. DATEXIST (CALNAME)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Calibration directory doesn''t exist: '//CALNAME)
         GOTO 990
      ENDIF
C
      DO 200 I = I1, I2
         ANAME = STRM2(DB, STRINT(MEMI(LADD+I)))
         CALL DATGETL (ANAME, 'Calibrator', ISACALIB, 1, NDUMMY)
         IF (ISACALIB) THEN
            WEIGHT2 = -ABS(CALWT)
         ELSE
            WEIGHT2 = -ABS(SKYWT)
         ENDIF
C
         IF (.NOT.DATEXIST (STRM2(CALNAME, 'AV'))) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Calibration run needs AV: '//CALNAME)
            GOTO 999
         ENDIF
         IF (DATEXIST (STRM2(ANAME, 'CAV'))) THEN
            CALL DATDELET (STRM2(ANAME, 'CAV'))
         ENDIF
         CALL DATCREAT (STRM2(ANAME, 'CAV'))
         CALL ARRLC (STRM2(ANAME, 'AV/AV'), 1.0, 
     $               STRM2(CALNAME, 'AV/AV'), WEIGHT2,
     $               STRM2(ANAME, 'CAV/AV') )
         CALL ARRCOPY (STRM2(ANAME, 'AV/TIME'), 
     $      	       STRM2(ANAME, 'CAV/TIME') )
C
         IF (ERROR) THEN
            CALL MSGPUT ('Error encountered in '//ANAME, 'E')
            GOTO 990
         ENDIF
         CALL ARRPOWER (STRM2(ANAME, 'CAV/AV'), 0.5, -99.0,
     $      STRM2(ANAME, 'CAV/ASD') )
         IF (ERROR) GOTO 990
         WRITE (MESSAGE, 1955) ANAME(1:20), CALNAME(1:20)
 1955    FORMAT ('Calibrated AV for ',A20, ' on ',A20)
         CALL MSGPUT (MESSAGE, 'I')
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
