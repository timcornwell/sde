C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calplot.f	1.3    11/7/90
C
      SUBROUTINE CALPLOT (DEVICE, NAME, SUB)
C
CD Plot antenna gains
C
C
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, DEVICE
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALPLOT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		GADD, TGADD, I, IANT, ORADD, NRADD, DATADD
      INTEGER		OFFSET, UADD, VADD, ISTAT, PGBEGIN, NDUMMY
      REAL		X, Y, XSCALE, YSCALE
      REAL		UMIN, UMAX, VMIN, VMAX, RMAX
      REAL		R
      COMPLEX		ANTGAIN
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*11	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL ARRSTAT (STRM3(NAME, SUB, 'UPOS'), ' ')
      CALL DATGETR (STRM3(NAME, SUB, 'UPOS'), 'ARRMAX', UMAX, 1, 
     1   NDUMMY)
      CALL DATGETR (STRM3(NAME, SUB, 'UPOS'), 'ARRMIN', UMIN, 1, 
     1   NDUMMY)
      CALL ARRSTAT (STRM3(NAME, SUB, 'VPOS'), ' ')
      CALL DATGETR (STRM3(NAME, SUB, 'VPOS'), 'ARRMAX', VMAX, 1, 
     1   NDUMMY)
      CALL DATGETR (STRM3(NAME, SUB, 'VPOS'), 'ARRMIN', VMIN, 1, 
     1   NDUMMY)
      IF (ERROR) GO TO 990
      UMAX = MAX(UMAX, UMIN)
      VMAX = MAX(VMAX, VMIN)
      IF ((UMAX.EQ.0.0).OR.(VMAX.EQ.0.0)) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Axes zero')
         GO TO 999
      END IF
      RMAX = 1.2*MAX(UMAX, VMAX)
      XSCALE = (2.0*RMAX)/20.0
      YSCALE = (2.0*RMAX)/20.0
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAIN'), NAX, NAXIS, 
     1   ATYPE, GADD)
      NANT = NAXIS(1)
      NUMINT = NAXIS(2)
      TGADD = DATADD(STRM3(NAME, SUB, 'GAINTIME'))
      ORADD = DATADD(STRM3(NAME, SUB, 'ORES'))
      NRADD = DATADD(STRM3(NAME, SUB, 'NRES'))
      UADD = DATADD(STRM3(NAME, SUB, 'UPOS'))
      VADD = DATADD(STRM3(NAME, SUB, 'VPOS'))
      IF (ERROR) GO TO 990
C
      IF (NUMINT.EQ.1) THEN
         ISTAT = PGBEGIN (0, DEVICE, 1, 1)
      ELSE 
         ISTAT = PGBEGIN (0, DEVICE, 3, 2)
      END IF
      IF (ISTAT.NE.1) THEN
         CALL ERRREPOR (ERROPEN, ROUTINE, 
     1      'Cannot open plot device ')
         GO TO 990
      END IF
C
      DO 10 I = 1, NUMINT
         CALL PGENV (-RMAX, RMAX, -RMAX, RMAX, 1, 0)
         DSTRING = STRTIMC(MEMR(TGADD+I-1))
         IF (ERROR) GO TO 990
         WRITE (MESSAGE, 1000) DSTRING , I
 1000    FORMAT ('Gains for time: ',A,', integration ',I6)
         CALL PGLABEL ('U position', 'V position', MESSAGE)
         WRITE (MESSAGE, 1050) MEMR(ORADD+I-1), MEMR(NRADD+I-1)
 1050    FORMAT ('Residual before = ',1PE12.4,', after = ',
     1      1PE12.4 , 'Jy')
         CALL PGMTEXT ('T', 1.0, 0.1, 0.0, MESSAGE)
         DO 20 IANT = 1, NANT
            OFFSET = (IANT-1) + NANT * (I - 1)
            ANTGAIN = MEMX(GADD + OFFSET)
            R = ABS(ANTGAIN)
            IF (R.GT.0.0) THEN
               X = MEMR(UADD + OFFSET)
               Y = MEMR(VADD + OFFSET)
               CALL PGMOVE (X,Y)
               CALL PGPOINT (1, X, Y, 17)
               X = X + XSCALE * AIMAG(ANTGAIN)
               Y = Y + YSCALE * REAL(ANTGAIN)
               CALL PGDRAW (X,Y)
            END IF
  20     CONTINUE
         X = -0.95*RMAX
         Y = -0.95*RMAX
         CALL PGMOVE (X,Y)
         CALL PGPOINT (1, X, Y, 17)
         Y = Y + YSCALE
         CALL PGDRAW (X,Y)
         IF (ERROR) GO TO 990
  10  CONTINUE
      CALL PGEND
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
