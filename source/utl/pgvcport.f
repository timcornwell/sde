C*PGVCPORT -- set viewport in terms of character sizes
C+
      SUBROUTINE PGVCPORT(LEFT, RIGHT, BOT, TOP)
C
C PGSTAND is equivalent to PGVCPORT (4., 4., 4., 4.)
C
C--
C Local routine by DSB  (23-Nov-93)
C-----------------------------------------------------------------------
#include "pgplot.inc"
      REAL	LEFT, RIGHT, BOT, TOP
      REAL     XLEFT, XRIGHT, YBOT, YTOP
C
      IF (PGOPEN.EQ.0) RETURN
C
      XLEFT  = LEFT*YSP/XPERIN
      XRIGHT = XLEFT + (XSZ-(LEFT+RIGHT)*YSP)/XPERIN
      YBOT   = BOT*YSP/YPERIN
      YTOP   = YBOT + (YSZ-(TOP+BOT)*YSP)/YPERIN
      CALL PGVSIZE(XLEFT, XRIGHT, YBOT, YTOP)
      END
