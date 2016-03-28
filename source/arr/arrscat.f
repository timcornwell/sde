C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrscat.f	1.8    2/11/93
C
      SUBROUTINE ARRSCAT (XNAME, YNAME, DATNAME, OUTNAME, WINDOW)
C
CD Make a scatter array using two arrays for coordinates
C  Put the result in the output array
C     
C       XNAME		CH*(*)	input	Directory name of x array
C       YNAME		CH*(*)	input	Directory name of y array
C       DATNAME	        CH*(*)	input	Directory name of data array
C       OUTNAME         CH*(*)  input   Directory name of the result
C       WINDOW          CH*(*)  input   Window to apply to x,y,data arrays
C     
C    Audit trail:
C    Cloned from arr2dplt                                  
C                                        R.G Marson    Jan 13, 1990
C    Added support for integer arrays
C                                        R.G. Marson   Feb 9, 1990
C    Added the datname parameter and required that it and outname
C        be Real arrays
C                                        R.G. Marson   Feb 19, 1990
C    Added window parameter
C                                        R.G. Marson   Jun 20, 1991
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	XNAME, YNAME, DATNAME, OUTNAME, WINDOW
C     
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSCAT')
C
C Function Definitions
C
      INTEGER           CRDRNAXM
C     
C     Local variables
C     
      INTEGER		NUMXPTS, NUMYPTS, NUMDPTS, NUMPTS, ANVALS
      INTEGER           WMAX, WMIN, BLC(SYSMXDIM), TRC(SYSMXDIM)
      CHARACTER*(1) 	TYPE, AXTYPE
      INTEGER 		NAX, NAXIS(SYSMXDIM)
      INTEGER 		XADD, YADD, OUTADD, DATADD
      INTEGER           XSIZE, YSIZE
C=====================================================================
C     
C     If there is an error on entry then return immediately
C     
      IF (ERROR) GO TO 999
C     
C     Get x-array attributes
C     
      CALL DATGETAR (XNAME, NAX, NAXIS, TYPE, XADD)
      IF ((TYPE.EQ.'R').OR.(TYPE.EQ.'I')) THEN
         AXTYPE = TYPE
         IF(CRDRNAXM(NAX, NAXIS).EQ.1)THEN
            NUMXPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'X-Axis not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'X-Axis not Real or Integer')
         GOTO 999
      END IF
C     
C     Get y-array attributes
C     
      CALL DATGETAR (YNAME, NAX, NAXIS, TYPE, YADD)
      IF ((TYPE.EQ.'R').OR.(TYPE.EQ.'I')) THEN
         IF (TYPE.NE.AXTYPE) THEN
            CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $           'X axis and Y axis are of different types')
            GOTO 999
         END IF
         IF(CRDRNAXM(NAX, NAXIS).EQ.1)THEN
            NUMYPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $           'Y-Axis not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Y-Axis not Real or Integer')
         GOTO 999
      END IF
C     
C     Get data array attributes
C     
      CALL DATGETAR (DATNAME, NAX, NAXIS, TYPE, DATADD)
      IF (TYPE.EQ.'R') THEN
         IF(CRDRNAXM(NAX, NAXIS).EQ.1)THEN
            NUMDPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $           'Data not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data array must be real')
         GOTO 999
      END IF
C
C     Get output array attributes
C     
      CALL DATGETAR (OUTNAME, NAX, NAXIS, TYPE, OUTADD)
      IF (TYPE.EQ.'R') THEN
         IF (NAX.EQ.2) THEN
            XSIZE = NAXIS(1)
            YSIZE = NAXIS(2)
         ELSE IF (CRDRNAXM(NAX, NAXIS).GE.2) THEN
            XSIZE = NAXIS(1)
            YSIZE = NAXIS(2)
         ELSE 
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $           'Scatter array must have more than 1 dimension')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Output array type not Real')
         GOTO 999
      END IF
C
C Sort out the window (default window is the entire array)
C
      IF (WINDOW.NE.' ') THEN
         CALL DATGETI(WINDOW, 'BLC', BLC, SYSMXDIM, ANVALS)
         CALL DATGETI(WINDOW, 'TRC', TRC, SYSMXDIM, ANVALS)
         IF (ERROR) GOTO 990
         WMAX = MAX(TRC(1), BLC(1))
         WMIN = MIN(TRC(1), BLC(1))
      ELSE
         WMAX = NUMXPTS
         WMIN = 1
      END IF
C     
C     Check sizes
C     
      NUMPTS = MIN(MAX(0,NUMXPTS),MAX(0,NUMYPTS),MAX(0,NUMDPTS))
      IF(NUMPTS .LT. 0)THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'Negative number of data points is illegal')
         GOTO 999
      ELSE IF (NUMPTS.EQ.0) THEN
         MESSAGE = 'No data points to plot'
         CALL MSGPUT(MESSAGE, 'W')
      END IF
      IF (NUMPTS .LT. MAX(MAX(NUMXPTS,NUMYPTS),NUMDPTS)) THEN
         MESSAGE = 'Arrays '//XNAME//' and '//' YNAME '//
     $        ' and '//DATNAME//' have unequal sizes'
         CALL MSGPUT(MESSAGE, 'W')
      END IF
      WMAX = MIN(NUMPTS, WMAX)
      NUMPTS = MIN(NUMPTS, MAX(0, WMAX-WMIN+1))
      WMIN = WMIN - 1
      IF (AXTYPE.EQ.'R') THEN
         CALL PIXRSCAT (MEMR(XADD+WMIN), MEMR(YADD+WMIN), 
     $        MEMR(DATADD+WMIN), NUMPTS, 
     $        MEMR(OUTADD), XSIZE, YSIZE)
      ELSE
         CALL PIXISCAT (MEMI(XADD+WMIN), MEMI(YADD+WMIN), 
     $        MEMR(DATADD+WMIN), NUMPTS, 
     $        MEMR(OUTADD), XSIZE, YSIZE)
      END IF
C     
C     Trace errors
C     
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
