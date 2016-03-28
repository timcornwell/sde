C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W%    %G%
C
      SUBROUTINE TIP1PLT (DB, WHAT, STIME, CALIB, 
     $   GRLIM, DEV, DOLIST)
C
CD Make a plot or a list of one THING from the database
C  we plot RAW, AV, ASD, SF, SSF, CAV, CASD, CSF, or CSSF
C
C	DB	CH*(*)	inp	Tipper DataBase
C	WHAT	CH*(*)	inp	What to PLot
C	STIME	INT(2)	inp	Time to select on
C	CALIB	INT	inp	0 = Do Sky;  1 = Do Calib Run; 2 = Do both
C	GRLIM	R(4)	inp	XMIN/XMAX/YMIN/YMAX
C	DEV	CH*(*)	inp	Plot Device or Listing File
C	DOLIST	L	inp	T: Make List   F: Make Plot
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
      CHARACTER*(*)	DB, WHAT, DEV
      REAL		GRLIM(4)
      LOGICAL		DOLIST
      INTEGER		STIME(2), CALIB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIP1PLT')
C
      INTEGER		I1, I2, NPTS, X0ADD, YADD, LADD, STYLE, AXIS,
     $   		XADD
      INTEGER		I, NAX, NAXIS(SYSMXDIM), IL, NDUMMY, K, Y0ADD
      REAL		NOTVAL
      CHARACTER*1	TYPE
      CHARACTER*(SYSMXNAM)	XLAB, YLAB, TLAB, ANAME, DEV1
      LOGICAL		ISACALIB

C
      CHARACTER*(SYSMXNAM)	STRM2, STRRMBL
      CHARACTER*15		STRINT
      LOGICAL			DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
C
      DO 800 IL = I1, I2
C
         ANAME = STRM2(DB, STRINT(MEMI(LADD+IL)))
         CALL DATGETL (ANAME, 'Calibrator', ISACALIB, 1, NDUMMY)
         IF (ERROR) GOTO 990
         IF (( ISACALIB .AND. (CALIB .EQ. 1 .OR. CALIB .EQ. 2)).OR.
     $       (.NOT.ISACALIB .AND. (CALIB .EQ. 0 .OR. CALIB .EQ. 2)))
     $      THEN
C
C Do this one
C
         ELSE
            GOTO 800
         ENDIF
C
         IF (ERROR) GOTO 990
         XLAB = 'Lag Time, s'
         STYLE = 17
         AXIS = 0
        TLAB = ANAME(1:20)//WHAT
         IF (WHAT(1:3) .EQ. 'RAW') THEN
            CALL TIPREAD (ANAME)
            CALL DATGETAR ( STRM2(ANAME, 'RAWDATA'), NAX, NAXIS, TYPE, 
     $         YADD)
            Y0ADD = YADD
            CALL DATMAKAR ( STRM2(ANAME, 'SCRATCH'), NAX, NAXIS, 'R',
     $         XADD)
            X0ADD = XADD
            NPTS = NAXIS(1)
            DO 10 I = 0, NPTS-1
               MEMR(XADD + I) = 3.5 * I
 10         CONTINUE
            XLAB = 'Time, Seconds'
            YLAB = 'Raw Sky Temp, K'
            STYLE = 1
            AXIS = 0
         ELSE IF (WHAT(1:2) .EQ. 'AV') THEN
            CALL DATGETAR ( STRM2(ANAME, 'AV/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'AV/AV'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Uncalibrated Allan Variance, K2 '
         ELSE IF (WHAT(1:3) .EQ. 'ASD') THEN
            CALL DATGETAR ( STRM2(ANAME, 'AV/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'AV/ASD'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Uncalibrated Allan Standard Deviation, K'
         ELSE IF (WHAT(1:2) .EQ. 'SF') THEN
            CALL DATGETAR ( STRM2(ANAME, 'SF/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'SF/SF'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Uncalibrated Structure Function, K2'
         ELSE IF (WHAT(1:3) .EQ. 'SSF') THEN
            CALL DATGETAR ( STRM2(ANAME, 'SF/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'SF/SSF'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Uncalibrated SQRT( Structure Function), K'
         ELSE IF (WHAT(1:3) .EQ. 'CAV') THEN
            CALL DATGETAR ( STRM2(ANAME, 'CAV/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'CAV/AV'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Calibrated Allan Variance, K2'
         ELSE IF (WHAT(1:4) .EQ. 'CASD') THEN
            CALL DATGETAR ( STRM2(ANAME, 'CAV/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'CAV/ASD'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            CALL DATMAKAR ( STRM2(ANAME, 'TEMP'), NAX, NAXIS, TYPE, 
     $         YADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Calibrated Allan Standard Deviation, K'
         ELSE IF (WHAT(1:3) .EQ. 'CSF') THEN
            CALL DATGETAR ( STRM2(ANAME, 'CSF/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'CSF/SF'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'Calibrated Structure Function, K2'
         ELSE IF (WHAT(1:4) .EQ. 'CSSF') THEN
            CALL DATGETAR ( STRM2(ANAME, 'CSF/TIME'), NAX, NAXIS, TYPE, 
     $         X0ADD)
            CALL DATGETAR ( STRM2(ANAME, 'CSF/SSF'), NAX, NAXIS, TYPE, 
     $         Y0ADD)
            NPTS = NAXIS(1)
            YLAB = 'LOG Calibrated SQRT( Structure Function), K'
         ENDIF
C
         IF (ERROR) THEN
            CALL MSGPUT ('Plot/List Item probably doesn''t exist: '//
     $         WHAT, 'E')
            GOTO 990
         ENDIF
C
         IF (.NOT. DOLIST .AND. WHAT(1:3) .NE. 'RAW') THEN
            CALL DATMAKAR ( STRM2(ANAME, 'TEMPY'), NAX, NAXIS, 'R', 
     $         YADD)
            CALL DATMAKAR ( STRM2(ANAME, 'TEMPX'), NAX, NAXIS, 'R', 
     $         XADD)
            NOTVAL = 9999.
            DO 21 K = 0, NAXIS(1) - 1
               IF (MEMR(Y0ADD + K) .GT. 0.0 .AND.
     $            MEMR(Y0ADD + K) .LT. NOTVAL) THEN
                  NOTVAL = MEMR(Y0ADD + K)
               ENDIF
 21         CONTINUE
            NOTVAL = NOTVAL * .5
            DO 22 K = 0, NAXIS(1) - 1
               IF (MEMR(Y0ADD +K) .LE. 0.0) THEN
                  MEMR(YADD + K) = ALOG10 (NOTVAL)
               ELSE
                  MEMR(YADD + K) = ALOG10 ( MEMR(Y0ADD + K) )
               ENDIF
               MEMR(XADD + K) = ALOG10 ( MEMR(X0ADD + K) )
 22         CONTINUE
         ENDIF
C
         IF (DOLIST) THEN
C
C List the unadulterated data
C
            CALL TXTOPEN (ROUTINE, DEV, 'WRITE')
            CALL TXTWRITE (ROUTINE, '   ')
            CALL TXTWRITE (ROUTINE, '   '//ANAME )
            CALL TXTWRITE (ROUTINE, '   '//XLAB(1:20)//YLAB(1:40) )
            CALL TXTWRITE (ROUTINE, ' ')
            DO 100 I = 0, NPTS-1
               WRITE (MESSAGE, '(5X, F10.5, 10X, F10.5)') MEMR(X0ADD+I),
     $            MEMR(Y0ADD+I)
               CALL TXTWRITE (ROUTINE, MESSAGE)
 100        CONTINUE
            CALL TXTCLOSE (ROUTINE)
         ELSE
C
C But plot edited data with no negatives
C
            IF (DEV(1:1) .EQ. '/') THEN
               DEV1 = DEV
            ELSE
               DEV1 = STRRMBL ('P'//STRINT(IL)//DEV)
            ENDIF
            CALL PIXPGGRF (NPTS, MEMR(XADD), MEMR(YADD),
     $         GRLIM(1), GRLIM(2), GRLIM(3), GRLIM(4),
     $         DEV1, XLAB, YLAB, TLAB, STYLE, AXIS)
         ENDIF
C
         IF (DATEXIST(STRM2(ANAME, 'TEMPX'))) THEN
            CALL DATDELET (STRM2(ANAME, 'TEMPX'))
            CALL DATDELET (STRM2(ANAME, 'TEMPY'))
         ENDIF
         IF (WHAT(1:3) .EQ. 'RAW') THEN
            CALL DATDELET (STRM2(ANAME, 'RAWDATA'))
            CALL DATDELET (STRM2(ANAME, 'SCRATCH'))
         ENDIF
C
 800  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
