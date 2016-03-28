C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipcumpl.f	1.1    3/26/93
C
      SUBROUTINE TIPCUMPL (DB, WHAT, T, STIME, CALIB, 
     $   GRLIM, DEV, DOLIST)
C
CD Plot fraction of time a quantity is better than some value
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
      PARAMETER		(ROUTINE = 'TIPCUMPL')
C
      INTEGER		I1, I2, NPTS, XADD, YADD, DADD, LADD, STYLE
      INTEGER		I, NAX, NAXIS(SYSMXDIM), IT, JT, IL, MAXPTS
      INTEGER		NDUMMY, Y2ADD
      CHARACTER*1	TYPE
      CHARACTER*5	CHARTIME
      CHARACTER*(SYSMXNAM)	XLAB, YLAB, TLAB, ANAME, ANAME0, YIT
      LOGICAL		ISACALIB
C
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
      CALL DATMAKAR (STRM2(DB, 'Y2TEMP'), 1, MAXPTS, 'R', Y2ADD)
      NPTS = 0
C
      ANAME0 = STRM2(DB, STRINT(MEMI(LADD+I1)))
      XLAB = 'Fraction of Time'
C
C NOTE:  Y is actually plotted on X AXIS, X on Y axis
C
      IF (T .LT. 3.5) T = 3.5
      IT = NINT( ALOG10 (T/3.5) / ALOG10(2.0) )
      IF (IT .GT. 8) IT = 8
      JT = 3.5 * 2.0**FLOAT(IT)
      WRITE (TLAB, 1965) DB(1:10), CALIB, WHAT(1:5), T
 1965 FORMAT ('Cumulative: ',A10, ' Calib=',I1,3X,A5,' on ',F6.2,' s')
      WRITE(CHARTIME, '(I5)') JT
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
         ANAME = STRM2(DB, STRINT(MEMI(LADD+IL)))
         CALL DATGETL (ANAME, 'Calibrator', ISACALIB, 1, NDUMMY)
         IF (ERROR) GOTO 990
         IF (( ISACALIB .AND. (CALIB .EQ. 1 .OR. CALIB .EQ. 2)).OR.
     $       (.NOT.ISACALIB .AND. (CALIB .EQ. 0 .OR. CALIB .EQ. 2)))
     $      THEN
            CALL DATGETAR ( STRM2(ANAME, YIT), NAX, NAXIS, TYPE, 
     $         DADD)
            MEMR(YADD + NPTS) = MEMR(DADD + IT)
            NPTS = NPTS + 1
         ENDIF
 500  CONTINUE
      CALL UTLRSORT (NPTS, MEMR(YADD), MEMR(Y2ADD) )
      DO 600 I = 0, NPTS - 1
         MEMR(XADD+I) = FLOAT(I) / FLOAT(NPTS-1)
 600  CONTINUE
C
      IF (DOLIST) THEN
         CALL TXTOPEN (ROUTINE, DEV, 'WRITE')
         CALL TXTWRITE (ROUTINE, '   '//TLAB )
         CALL TXTWRITE (ROUTINE, '   '//XLAB(1:20)//YLAB(1:40) )
         CALL TXTWRITE (ROUTINE, ' ')
         DO 100 I = 0, NPTS-1
            WRITE (MESSAGE, '(5X, F10.5, 10X, F10.5)') MEMR(XADD+I),
     $         MEMR(Y2ADD+I)
            CALL TXTWRITE (ROUTINE, MESSAGE)
 100     CONTINUE
         CALL TXTCLOSE (ROUTINE)
      ELSE
         CALL PIXPGGRF (NPTS, MEMR(Y2ADD), MEMR(XADD), 
     $      GRLIM(1), GRLIM(2), GRLIM(3), GRLIM(4),
     $      DEV, YLAB, XLAB, TLAB, 20, 0)
      ENDIF
C
      CALL DATDELET (STRM2(DB, 'XTEMP') )
      CALL DATDELET (STRM2(DB, 'Y2TEMP') )
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

