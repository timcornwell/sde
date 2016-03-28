C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gailist.f	1.1    12/26/91
C
       SUBROUTINE GAILIST (HANDLE, NAME, SUB, TIME, IANT, NIANT)
C
CD List complex antenna gains
C
C	HANDLE	CH*(*)	input	HANDLE of output stream
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. CAL/I or OBS/I
C	TIME	R(2)	input	Time range
C	IANT	I(*)	input   Array of antenna numbers
C	NIANT	I	input	Number of antennas in IANT
C
C Audit trail:
C       Cloned from VISLIST
C				D.S.Briggs	4 Sept 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, HANDLE
       INTEGER		IANT(*), NIANT
       REAL		TIME(2)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAILIST')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), GADD, TADD, NUMANT,
     $   		NUMINT
      REAL		TMIN, TMAX
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM3
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (.NOT.DATEXIST(STRM3(NAME, SUB, 'ANTGAIN'))) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE,
     $                  'No Antenna Gain information')
         GO TO 999
      END IF
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAIN'),
     $               NAX, NAXIS, ATYPE, GADD)
      IF ((NAX.NE.2).OR.(ATYPE.NE.'X')) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Malformed ANTGAIN array')
         GO TO 999
      END IF
      NUMANT = NAXIS(1)
      NUMINT = NAXIS(2)
C
      CALL DATGETAR (STRM3(NAME, SUB, 'GAINTIME'),
     $               NAX, NAXIS, ATYPE, TADD)
      IF ((NAX.NE.1).OR.(ATYPE.NE.'R').OR.(NAXIS(1).NE.NUMINT)) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Malformed GAINTIME array')
         GO TO 999
      END IF
C
      TMIN = TIME(1)
      TMAX = TIME(2)
      IF ((TMIN.EQ.0).AND.(TMAX.EQ.0)) THEN
         TMIN = -1.E10
         TMAX = 1.E10
      END IF
C
      CALL GAILISTP (HANDLE, MEMX(GADD), MEMR(TADD), NUMANT, NUMINT,
     $   TMIN, TMAX, IANT, NIANT)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
