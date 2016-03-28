C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calcfplo.f	1.4    5/15/92
C
      SUBROUTINE CALCFPLO (DEVICE, NAME, SUB)
C
CD Plot antenna gain correlation function
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

      PARAMETER		(ROUTINE = 'CALCFPLO')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		CFADD, TGADD, I, IANT, ORADD, NRADD, DATADD
      INTEGER		UADD, VADD, ISTAT, PGBEGIN, NDUMMY
      INTEGER		IA1,IA2, ICF, NLAG, NCF, OFFSET1, OFFSET2
      REAL		X, Y, XSCALE, YSCALE
      REAL		UMIN, UMAX, VMIN, VMAX, RMAX
      REAL		R
      COMPLEX		CF
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*12	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
      CHARACTER*4	STRING
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
      XSCALE = (4.4*UMAX)/20.0
      YSCALE = (4.4*VMAX)/20.0
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAINCF'), NAX, NAXIS, 
     1   ATYPE, CFADD)
      NANT = NAXIS(1)
      NCF = NAXIS(3)
      UADD = DATADD(STRM3(NAME, SUB, 'UPOS'))
      VADD = DATADD(STRM3(NAME, SUB, 'VPOS'))
      IF (ERROR) GO TO 990
C
      ISTAT = PGBEGIN (0, DEVICE, 1, 1)
      IF (ISTAT.NE.1) THEN
         CALL ERRREPOR (ERROPEN, ROUTINE, 
     1      'Cannot open plot device ')
         GO TO 990
      END IF
C
      DO 10 ICF = 1, NCF
         IF (ICF.EQ.1) THEN
            I = 0
            STRING = 'Even'
         ELSE IF (ICF.EQ.2) THEN
            I = 1
            STRING = 'Even'
         ELSE IF (ICF.EQ.3) THEN
            I = 0
            STRING = 'Odd '
         ELSE IF (ICF.EQ.4) THEN
            I = 1
            STRING = 'Odd '
         END IF
C
         CALL PGENV (-2.2*UMAX, 2.2*UMAX, -2.2*VMAX, 2.2*VMAX, 1, 0)
         IF (ERROR) GO TO 990
         WRITE (MESSAGE, 1000) I, STRING
 1000    FORMAT ('Antenna Gain correlation function for lag ',I4,
     1      ' : ',A4,' samples only')
         CALL PGLABEL ('U position', 'V position', MESSAGE)
         DO 20 IA2 = 1, NANT
            DO 15 IA1 = 1, NANT
               CF  = MEMX(CFADD + (IA1-1) + NANT * (IA2 - 1) + 
     1            NANT**2 * (ICF - 1))
               R = ABS(CF)
               IF (R.GT.0.0) THEN
                  X = - (MEMR(UADD + IA2-1) - MEMR(UADD + IA1-1))
                  Y = MEMR(VADD + IA2-1) - MEMR(VADD + IA1-1)
                  CALL PGMOVE (X,Y)
                  CALL PGPOINT (1, X, Y, 17)
                  X = X + XSCALE * AIMAG(CF)
                  Y = Y + YSCALE * REAL(CF)
                  CALL PGDRAW (X,Y)
               END IF
  15        CONTINUE
  20     CONTINUE
         X = -0.95*2.2*UMAX
         Y = -0.95*2.2*VMAX
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
