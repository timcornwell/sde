C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pltscatt.f	1.2	 7/16/92
C
      SUBROUTINE PLTSCATT (X, Y, DEV, XLAB, YLAB, TITLE, STYLE, AXIS,
     $   XMIN, XMAX, YMIN, YMAX)
C
C Makes a scatter plot of Y vs X on device DEV
C
C	X, Y	CH*(*)	input	REAL ARRAYS to be graphed
C	XLAB	CH*(*)	input	LABELS
C	STYLE	INT	input	line=0; graph marker=1-31
C	AXIS	INT	input	PGPLOT  (0 = normal, 30 = log-log)
C	XMIN    REAL	input	Limits on X and Y
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Oct 6 1989
C	Add axis argument.
C				D.S.Briggs	July 15 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	X, Y, DEV, XLAB, YLAB, TITLE
      INTEGER		STYLE, AXIS
      REAL		XMIN, XMAX, YMIN, YMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PLTSCATT')
C
      INTEGER		XADD, YADD, DATADD, NAXIS(SYSMXDIM), NAX, I
      INTEGER		NPTS1, NPTS2, ADD
      CHARACTER*(SYSMXNAM)	STRM2, T1*1
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (X, NAX, NAXIS, T1, XADD)
      NPTS1 = 1
      DO 100 I = 1, NAX
         NPTS1 = NPTS1 * NAXIS(I)
 100  CONTINUE
      CALL DATGETAR (Y, NAX, NAXIS, T1, YADD)
      NPTS2 = 1
      DO 200 I = 1, NAX
         NPTS2 = NPTS2 * NAXIS(I)
 200  CONTINUE
C
      IF (NPTS1 .NE. NPTS2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different number of pnts')
         GOTO 990
      ENDIF
      IF (STYLE .EQ. 0) STYLE = 1
      CALL PIXPGGRF (NPTS1, MEMR(XADD), MEMR(YADD), XMIN, XMAX, YMIN,
     $   YMAX, DEV, XLAB, YLAB, TITLE, STYLE, AXIS)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
