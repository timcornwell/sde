C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrdescat.f	1.4    1/31/93
C
      SUBROUTINE ARRDESCAT (SCATNAME, XNAME, YNAME, DATNAME, NPTS)
CD     
C    Convert a array (scatname) into a three one dimensional
C    arrays representing the X-coords, Y coords, and data values.
C    Zero pixel values are not represented in these arrays.
C    npts is the number of points extracted
C    
C       xname		CH*(*)	output	Directory name of x array
C       yname		CH*(*)	output	Directory name of y array
C       datname	CH*(*)	output	Directory name of data array
C       scatname       CH*(*)  input   Directory name of the input
C                                      2D array
C       npts           INT     output  Real size of x, y, dat-name
C       
C     
C    Audit trail:
C    Cloned from arrscat                                  
C                                        R.G Marson    Feb 13, 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	XNAME, YNAME, DATNAME, SCATNAME
      INTEGER           NPTS
C     
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRDESCAT')
C     
C     Local variables
C     
      INTEGER		NUMXPTS, NUMYPTS, NUMDPTS, NUMPTS, I
      CHARACTER*(1) 	TYPE, AXTYPE, ATYPE
      INTEGER 		ANAX, NAX, NAXIS(SYSMXDIM)
      INTEGER 		XADD, YADD, DATADD, SCADD
      INTEGER           XSIZE, YSIZE
C=====================================================================
C     
C     If there is an error on entry then return immediately
C     
      IF (ERROR) GO TO 999
C     
C     Get input array attributes
C     
      CALL DATGETAR (SCATNAME, NAX, NAXIS, TYPE, SCADD)
      IF ((TYPE.EQ.'R').OR.(TYPE.EQ.'I').OR.
     $     (TYPE.EQ.'X').OR.(TYPE.EQ.'D')) THEN
         ATYPE = TYPE
         ANAX = 0
         DO I = 1, NAX
            IF (NAXIS(I).NE.1) THEN
               ANAX = ANAX + 1
               NAXIS(ANAX) = NAXIS(I)
            END IF
         END DO
         IF(ANAX.EQ.2)THEN
            XSIZE = NAXIS(1)
            YSIZE = NAXIS(2)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'Scatter Array is not two dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Scatter Array not Real or Integer or Complex or Double')
         GOTO 999
      END IF
C
C Check X array attributes are compatible
C
      CALL DATGETAR (XNAME, NAX, NAXIS, TYPE, XADD)
      IF (TYPE.EQ.'R') THEN
         AXTYPE = TYPE
         ANAX = 0
         DO I = 1, NAX
            IF (NAXIS(I).NE.1) THEN
               ANAX = ANAX + 1
               NAXIS(ANAX) = NAXIS(I)
            END IF
         END DO
         IF(ANAX.EQ.1)THEN
            NUMXPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'X-Axis not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'X-Axis not Real')
         GOTO 999
      END IF
C     
C     Get y-array attributes
C     
      CALL DATGETAR (YNAME, NAX, NAXIS, TYPE, YADD)
      IF (TYPE.EQ.'R') THEN
         ANAX = 0
         DO I = 1, NAX
            IF (NAXIS(I).NE.1) THEN
               ANAX = ANAX + 1
               NAXIS(ANAX) = NAXIS(I)
            END IF
         END DO
         IF(ANAX.EQ.1)THEN
            NUMYPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $           'Y-Axis not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Y-Axis not Real')
         GOTO 999
      END IF
C     
C     Get data array attributes
C
      CALL DATGETAR (DATNAME, NAX, NAXIS, TYPE, DATADD)
      IF ((TYPE.EQ.'R').OR.(TYPE.EQ.'I')
     $     .OR.(TYPE.EQ.'X').OR.(TYPE.EQ.'D')) THEN
         IF (TYPE.NE.ATYPE) THEN
            CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $           'Scatter array and Data array are of different types')
            GOTO 999
         END IF
         ANAX = 0
         DO I = 1, NAX
            IF (NAXIS(I).NE.1) THEN
               ANAX = ANAX + 1
               NAXIS(ANAX) = NAXIS(I)
            END IF
         END DO
         IF(ANAX.EQ.1)THEN
            NUMDPTS = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $           'Data Array not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data array must be Real, Integer, Complex or Double')
         GOTO 999
      END IF
C     
C     Check sizes
C     
      NUMPTS = MIN(MIN(NUMXPTS, NUMYPTS), NUMDPTS)
      IF(NUMPTS .LT. 0)THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'Array(s) cannot hold any data points')
         GOTO 999
      END IF
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRDSCAT (MEMR(SCADD), XSIZE, YSIZE, MEMR(XADD), 
     $        MEMR(YADD), MEMR(DATADD), NUMPTS)
      ELSE IF (ATYPE.EQ.'I') THEN
         CALL PIXIDSCAT (MEMI(SCADD), XSIZE, YSIZE, MEMR(XADD), 
     $        MEMR(YADD), MEMI(DATADD), NUMPTS)
      ELSE IF (ATYPE.EQ.'X') THEN
         CALL PIXXDSCAT (MEMX(SCADD), XSIZE, YSIZE, MEMR(XADD), 
     $        MEMR(YADD), MEMX(DATADD), NUMPTS)
      ELSE IF (ATYPE.EQ.'D') THEN
         CALL PIXDDSCAT (MEMD(SCADD), XSIZE, YSIZE, MEMR(XADD), 
     $        MEMR(YADD), MEMD(DATADD), NUMPTS)
      END IF
      NPTS = NUMPTS
C     
C     Trace errors
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END







