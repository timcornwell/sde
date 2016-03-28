C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tiptplt.f	1.1    3/26/93
C
      SUBROUTINE TIPTPLT (DB, WHAT, T, STIME, CALIB, 
     $   GRLIM, DEV, DOLIST)
C
CD Make a plot over time of some quantity over some characteristic time
C  we plot AV, ASD, SF, SSF, CAV, CASD, CSF, or CSSF
C
C	DB	CH*(*)	inp	Tipper DataBase
C	WHAT	CH*(*)	inp	What to PLot
C	T	REAL	inp	Characteristic Time (3, 7, 14, 28....900)
C	STIME	INT(2)	inp	Time to select on
C	CALIB	INT	inp	0 = Do Sky;  1 = Do Calib; 2 = Do All
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
      REAL		T, GRLIM(4)
      LOGICAL		DOLIST
      INTEGER		STIME(2), CALIB
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPTPLT')
C
      INTEGER		I1, I2, NPTS, XADD, YADD, DADD, LADD, STYLE
      INTEGER		I, NAX, NAXIS(SYSMXDIM), IT, JT, IL, MAXPTS
      INTEGER		NDUMMY
      REAL		TEMP
      CHARACTER*1	TYPE
      CHARACTER*5	CHARTIME
      CHARACTER*(SYSMXNAM)	XLAB, YLAB, TLAB, ANAME, TIME0, YIT,
     $   			TIMENOW
      LOGICAL		ISACALIB
C
      REAL			DAYSSINC
      CHARACTER*(SYSMXNAM)	STRM2, STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL TIPRANGE (DB, STIME, I1, I2)
      CALL DATGETAR (STRM2(DB, 'LIST'), NAX, NAXIS, TYPE, LADD)
C
      MAXPTS = I2-I1+1
      IF (MAXPTS .LE. 0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No points to plot')
         GOTO 999
      ENDIF
      CALL DATMAKAR (STRM2(DB, 'XTEMP'), 1, MAXPTS, 'R', XADD)
      CALL DATMAKAR (STRM2(DB, 'YTEMP'), 1, MAXPTS, 'R', YADD)
      NPTS = 0
C
C
      IF (T .LT. 3.5) T = 3.5
      IT = NINT( ALOG10 (T/3.5) / ALOG10(2.0) )
      IF (IT .GT. 8) IT = 8
      JT = 3.5 * 2.0**FLOAT(IT)
      WRITE(CHARTIME, '(I5)') JT
      TIME0 = STRINT(MEMI(LADD+I1))
      XLAB = 'TIME, days since '//TIME0
      WRITE (TLAB, 1965) DB(1:10), CALIB, WHAT(1:5), T
 1965 FORMAT ('Data vs Time: ',A10, ' Calib=',I1,3X,A5,' on ',F6.2,' s')
      STYLE = 17
      IF (WHAT(1:2) .EQ. 'AV') THEN
         YIT = 'AV/AV'
         YLAB = 'Uncalibrated Allan Variance @'//CHARTIME//' s'
      ELSE IF (WHAT(1:3) .EQ. 'ASD') THEN
         YIT = 'AV/ASD'
         YLAB = 'Uncalibrated Standard Deviation @'//CHARTIME//' s'
      ELSE IF (WHAT(1:2) .EQ. 'SF') THEN
         YIT = 'SF/SF'
         YLAB = 'Uncalibrated Structure Function @'//CHARTIME//' s'
      ELSE IF (WHAT(1:3) .EQ. 'SSF') THEN
         YIT = 'SF/SSF'
         YLAB = 'Uncalibrated SQRT(Struc Func) @'//CHARTIME//' s'
      ELSE IF (WHAT(1:3) .EQ. 'CAV') THEN
         YIT = 'CAV/AV'
         YLAB = 'Calibrated Allan Variance @'//CHARTIME//' s'
      ELSE IF (WHAT(1:4) .EQ. 'CASD') THEN
         YIT = 'CAV/ASD'
         YLAB = 'Calibrated Allan Standard Deviation @'//CHARTIME//' s'
      ELSE IF (WHAT(1:3) .EQ. 'CSF') THEN
         YIT = 'CSF/SF'
         YLAB = 'Calibrated Structure Function @'//CHARTIME//' s'
      ELSE IF (WHAT(1:4) .EQ. 'CSSF') THEN
         YIT = 'CSF/SSF'
         YLAB = 'Calibrated SQRT(Strc Func) @'//CHARTIME//' s'
      ENDIF
C
      DO 500 IL = I1, I2
         TIMENOW = STRINT(MEMI(LADD+IL))
         ANAME = STRM2(DB, TIMENOW)
         CALL DATGETL (ANAME, 'Calibrator', ISACALIB, 1, NDUMMY)
         IF (ERROR) GOTO 990
         IF (( ISACALIB .AND. (CALIB .EQ. 1 .OR. CALIB .EQ. 2)).OR.
     $       (.NOT.ISACALIB .AND. (CALIB .EQ. 0 .OR. CALIB .EQ. 2)))
     $      THEN
            CALL DATGETAR ( STRM2(ANAME, YIT), NAX, NAXIS, TYPE, 
     $         DADD)
            MEMR(YADD + NPTS) = MEMR(DADD + IT)
            TEMP = DAYSSINC (TIMENOW, TIME0)
            MEMR(XADD + NPTS) = TEMP
            NPTS = NPTS + 1
         ENDIF
 500  CONTINUE
C
      IF (DOLIST) THEN
         CALL TXTOPEN (ROUTINE, DEV, 'WRITE')
         CALL TXTWRITE (ROUTINE, '   '//TLAB )
         CALL TXTWRITE (ROUTINE, '   '//XLAB(1:20)//YLAB(1:40) )
         CALL TXTWRITE (ROUTINE, ' ')
         DO 100 I = 0, NPTS-1
            WRITE (MESSAGE, '(5X, F10.5, 10X, F10.5)') 
     $         MEMR(XADD+I), MEMR(YADD+I)
            CALL TXTWRITE (ROUTINE, MESSAGE)
 100     CONTINUE
         CALL TXTCLOSE (ROUTINE)
      ELSE
         CALL PIXPGGRF (NPTS, MEMR(XADD), MEMR(YADD),
     $      GRLIM(1), GRLIM(2), GRLIM(3), GRLIM(4),
     $      DEV, XLAB, YLAB, TLAB, STYLE, 0)
      ENDIF
C
      CALL DATDELET (STRM2(DB, 'XTEMP') )
      CALL DATDELET (STRM2(DB, 'YTEMP') )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
      REAL FUNCTION DAYSSINC (NAME1, NAME0)
#include		"stdinc.h"
      CHARACTER*(*)	NAME1, NAME0
      INTEGER		I1, I0
      INTEGER		Y1, Y0, M1, M0, D1, D0, H1, H0
      INTEGER		DD, HD, I
      REAL		DIFF
      INTEGER		DINMO(12)
      INTEGER		YDINDAYS, MDINDAYS
      DATA		DINMO /31, 28, 31, 30, 31, 30,
     $   		       31, 31, 30, 31, 30, 31/
C

      READ(NAME0, *) I0
      READ(NAME1, *) I1
C
      READ(NAME0(1:2), *) Y0
      READ(NAME1(1:2), *) Y1
      READ(NAME0(3:4), *) M0
      READ(NAME1(3:4), *) M1
      READ(NAME0(5:6), *) D0
      READ(NAME1(5:6), *) D1
      READ(NAME0(7:8), *) H0
      READ(NAME1(7:8), *) H1
C
      HD = H1 - H0
      DD = D1 - D0
C
      MDINDAYS = 0
      IF (M1 .GT. M0) THEN
         DO 10 I = M0, M1-1
            MDINDAYS = MDINDAYS + DINMO(I)
 10      CONTINUE
      ELSE IF (M1 .LT. M0) THEN
         DO 20 I = M0, 12
            MDINDAYS = MDINDAYS + DINMO(I)
 20      CONTINUE
         DO 30 I = 1, M1-1
            MDINDAYS = MDINDAYS + DINMO(I)
 30      CONTINUE
      ENDIF
C
      IF (Y1 .GT. Y0 .AND. M1 .GT. M0) THEN
         YDINDAYS = (Y1 - Y0)*365.0
      ELSE
         YDINDAYS = 0
      ENDIF
C
      DIFF = YDINDAYS + MDINDAYS + DD + FLOAT(HD)/4.0
C
      DAYSSINC = DIFF
      RETURN
      END

