C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrpggrf.f	1.3	 7/16/92
C
      SUBROUTINE ARRPGGRF (NPTS, X, Y, DEV, XLAB, YLAB, LABEL, STYLE,
     $   AXIS, XMIN, XMAX, YMIN, YMAX)
C
C Makes a graph of Y vs X on device DEV
C
C	NPTS	INT	input	Number of points to graph
C	X, Y	CH*(*)	input	REAL ARRAYS to be graphed
C	XLAB	CH*(*)	input	LABELS
C	STYLE	INT	input	line=0; graph marker=1-31
C	AXIS	INT	input	PGPLOT  (0 = normal, 30 = log-log)
C	XMIN    REAL	input	Limits on X and Y
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C	Add AXIS argument
C				D.S.Briggs	June 15 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	X, Y, DEV, XLAB, YLAB, LABEL
      INTEGER		STYLE, NPTS, AXIS
      REAL		XMIN, XMAX, YMIN, YMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRPGGRF')
C
      INTEGER		XADD, YADD, DATADD
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      XADD =  DATADD(X)
      YADD =  DATADD(Y)
C
      CALL PIXPGGRF (NPTS, MEMR(XADD), MEMR(YADD), XMIN, XMAX, YMIN,
     $   YMAX, DEV, XLAB, YLAB, LABEL, STYLE, AXIS)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
