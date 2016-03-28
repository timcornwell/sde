C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdtplot.f	1.1	 5/4/93
C
      SUBROUTINE VSDTPLOT (PDT, IANT, HAND, AMPDEV, PHADEV,
     $   STYLE, NPOINTS)
C
CD Plots D terms as a function of time
C
C	PDT	CH*(*)	in	Pol D Terms database
C	IANT	INT	in	Which antenna to plot
C	HAND	CH*1	in	R or L
C	AMPDEV	CH*(*)	in	Amplitude Display Device
C	PHADEV	CH*(*)	in	Phase Display Device
C	STYLE	INT	in	Graph marker style
C	NPOINTS	INT	in	Number of points to plot
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 16 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	PDT, AMPDEV, PHADEV
      INTEGER		IANT, STYLE, NPOINTS
      CHARACTER*1	HAND
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDTPLOT')
C
      CHARACTER*1	T
      INTEGER		I, NAX, NAXIS(SYSMXDIM), NANT, NUMINT
      INTEGER		DADD, TADD, AADD, PADD, ADADD
      DATA		NAXIS	/SYSMXDIM * 1/
      CHARACTER*3	STRINT
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (HAND .EQ. 'L' .OR. HAND .EQ. 'l') THEN
         CALL DATGETAR (STRM2(PDT,'DL'), NAX, NAXIS, T, DADD)
      ELSE
         CALL DATGETAR (STRM2(PDT,'DR'), NAX, NAXIS, T, DADD)
      ENDIF
      IF (T .NE. 'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Should have complex D term')
         GOTO 999
      ENDIF
      NANT = NAXIS(1)
      NUMINT = NAXIS(2)
      IF (NAX .EQ. 1 .OR. NUMINT .LE. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Only 1 time interval, stopping now')
         GOTO 999
      ENDIF
      CALL DATGETAR (STRM2(PDT,'DTIME'), NAX, NAXIS, T, TADD)
C
      CALL DATMAKAR (STRM2(PDT, 'ADTERM'), 1, NUMINT, 'X', ADADD)
      CALL DATMAKAR (STRM2(PDT, 'AMP'), 1, NUMINT, 'R', AADD)
      CALL DATMAKAR (STRM2(PDT, 'PHA'), 1, NUMINT, 'R', PADD)
C
      DO 100 I = 0, NUMINT-1
         MEMX(ADADD+I) = MEMX(DADD + (IANT-1) + NANT*I)
 100  CONTINUE
      CALL PIXX2AP (NUMINT, MEMX(ADADD), MEMR(AADD), MEMR(PADD))
C
      IF (NPOINTS  .LE. 0) NPOINTS = NUMINT
C
      IF (AMPDEV .NE. ' ') THEN
         CALL PIXPGGRF(NPOINTS, MEMR(TADD), MEMR(AADD),0., 0., 0., 0.,
     $      AMPDEV, 'Time, days', 'D'//HAND//' Amplitude',
     $      'D'//HAND//' for antenna '//STRINT(IANT),
     $      STYLE, 0)
      ENDIF
      IF (PHADEV .NE. ' ') THEN
         CALL PIXPGGRF(NPOINTS, MEMR(TADD), MEMR(PADD),0.,0., 0., 0.,
     $      PHADEV, 'Time, days', 'D'//HAND//' Phase, Degrees',
     $      'D'//HAND//' for antenna '//STRINT(IANT),
     $      STYLE, 0)
      ENDIF
C
      CALL DATDELET (STRM2(PDT, 'ADTERM'))
      CALL DATDELET (STRM2(PDT, 'AMP'))
      CALL DATDELET (STRM2(PDT, 'PHA'))
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
