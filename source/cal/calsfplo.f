C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsfplo.f	1.4    5/15/92
C
      SUBROUTINE CALSFPLO (STRING, DEVICE, NAME, SUB)
C
CD Plot antenna gain structure function
C
C
C	STRING	CH*(*)	input	String for plot labelling
C	DEVICE	CH*(*)	input	Name of device for plotting
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C Audit trail:
C	Fixed to allow arbitrary number of lags
C				T.J.Cornwell	Jan 12 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, DEVICE, STRING
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALSFPLO')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		SFADD, I, IANT, DATADD
      INTEGER		UADD, VADD, ISTAT, PGBEGIN, NDUMMY
      INTEGER		IA1,IA2, ILAG, NLAG
      REAL		X, Y, XSCALE, YSCALE
      REAL		UMIN, UMAX, VMIN, VMAX, RMAX
      REAL		SF
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*12	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
      CHARACTER*4	TSTRING
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
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAINSF'), NAX, NAXIS, 
     1   ATYPE, SFADD)
      NANT = NAXIS(1)
      NLAG = NAXIS(3)
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
      CALL CALSFPLP (MEMR(SFADD), NANT, NLAG, MEMR(UADD), MEMR(VADD),
     1   XSCALE, YSCALE, UMAX, VMAX, STRING)
C
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
