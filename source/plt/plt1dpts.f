C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)plt1dpts.f	1.2    11/7/90
C
        SUBROUTINE PLT1DPTS(POINTS, ADDR, PLOTINFO, SYMBOL,
     $     PTINCR, REVERSE, SQUARE, XMIN, XMAX, YMIN, YMAX, 
     $     RVAL, RPIX, DELT)
C
CD Plot points using the coordinates of two arrays
C
CS Arguments: CALL plt1dpts(points, addr, plotinfo, symbol, ptincr,
CS     1   reverse, square, autoscl, xmin, xmax, ymin, ymax)
CS
CS      points          INT input       Number of points to plot
CS      addr            INT input       Memory Address of data
CS      plotinfo        CH*(*)  input   Plot information
CS      symbol          INT     input   Symbol
CS      ptincr          INT     input   Increment
CS      reverse         LOGICAL input   Plot reversed points as well
CS      square          LOGICAL input   Make axes square?
CS      xmin, etc       REAL    input   Required minimum
CS      rval            REAL    input   Reference Value
CS      rpix            REAL    input   Reference Pixel
CS      delt            REAL    input   Co-ord increment
CS
CD Audit trail:
C      Cloned from plt2dpts
C                                      R.G. Marson 17 Sep 1990
C
C--------------------------------------------------------------------
#include        "stdinc.h"
C
C       Declarations of inputs
C
        INTEGER         POINTS, SYMBOL, PTINCR
        INTEGER         ADDR
        CHARACTER *(*)  PLOTINFO(1)
        LOGICAL         REVERSE, SQUARE
        REAL            XMIN, XMAX, YMIN, YMAX, RVAL, RPIX, DELT
C
C       Local variables
C
        REAL            XR
        REAL            YR
        REAL            DELTA, TEMP
C
C       Plot information mapped variables
C       
        INTEGER         I
        CHARACTER *(80) DEVNAME
        CHARACTER *(80) PLOTNAME
        CHARACTER *(80) XLABEL
        CHARACTER *(80) YLABEL
        CHARACTER *1    TYPE


        REAL            ADD2REAL
C
C       Extract various bits of information from plotinfo
C
        PLOTNAME = PLOTINFO(1)
        XLABEL = PLOTINFO(2)
        YLABEL = PLOTINFO(3)
        DEVNAME = PLOTINFO(4)
        TYPE = PLOTINFO(5)
C
C       Set things up find min-max etc.
C
        IF (YMIN.GT.YMAX) THEN
           TEMP = YMIN
           YMAX = YMIN
           YMIN = TEMP
        ELSE IF ((YMIN.EQ.0.0).AND.(YMAX.EQ.0.0)) THEN
           YMIN = ADD2REAL(ADDR, TYPE)
           YMAX = ADD2REAL(ADDR, TYPE)
           DO 10 I = 1, POINTS - 1, PTINCR
              YR = ADD2REAL(ADDR + I, TYPE)
              YMIN = MIN(YR, YMIN)
              YMAX = MAX(YR, YMAX)
 10        CONTINUE
        END IF
C
C       Expand min-max's by 5% for a nice fit
C
        DELTA = YMAX - YMIN
        IF (DELTA.NE.0.0) THEN
           YMAX = YMAX + 0.05 * DELTA
           YMIN = YMIN - 0.05 * DELTA
        ELSE IF (YMAX.NE.0.0) THEN
           YMAX = YMAX + 0.05 * YMAX
           YMIN = YMIN - 0.05 * YMIN
        ELSE
           YMAX = 1.0
           YMIN = 0.0
        END IF
C
C Setup xmin and xmax
C
        IF (XMIN.GT.XMAX) THEN
           TEMP = XMIN
           XMAX = XMIN
           XMIN = TEMP
        ELSE IF (XMIN.EQ.XMAX) THEN
           IF (XMIN.EQ.0.0) THEN
              XMIN = 1.0
              XMAX = FLOAT(POINTS)
              DELT = 1.0
              RPIX = 1.0
              RVAL = 1.0
           ELSE
              XMIN = XMIN - 0.05 * XMIN
              XMAX = XMAX + 0.05 * XMAX
           END IF
        END IF
        IF (DELT.EQ.0.0) THEN
           CALL MSGPUT('W', 'Pixel increment set to 1.0')
           DELT = 1.0
        END IF
C
C       Start the plotting now
C
        CALL PGBEGIN(0, DEVNAME, 1,1)
        CALL PGSCI(3)
        IF (SQUARE) THEN
           CALL PGENV(XMIN, XMAX, YMIN, YMAX,1,1)
        ELSE
           CALL PGENV(XMIN, XMAX, YMIN, YMAX,0,1)
        END IF
        CALL PGLABEL(XLABEL,YLABEL,PLOTNAME)
        CALL PGSCI(5)
C
C       do the dirty work point by %$#^%$#^% point
C
        IF(REVERSE) THEN
           XR = -((1 - RPIX) * DELT + RVAL)
           YR = ADD2REAL(ADDR+POINTS-1, TYPE)
           CALL PGMOVE(XR, YR)
           CALL PGPOINT(1, XR, YR, SYMBOL)
           DO 21 I=1, POINTS-1, PTINCR
              XR = (I+1 - RPIX) * DELT + RVAL
              YR = ADD2REAL(ADDR + I, TYPE)
              CALL PGDRAW(XR, YR)
 21        CONTINUE
           XR = -(I+1 - RPIX) * DELT + RVAL
           CALL PGPOINT(1,XR,YR,SYMBOL)
        ELSE 
           XR = (1 - RPIX) * DELT + RVAL
           YR = ADD2REAL(ADDR, TYPE)
           CALL PGMOVE(XR, YR)
           CALL PGPOINT(1, XR, YR, SYMBOL)
        END IF
        DO 20 I=1, POINTS-1, PTINCR
           XR = (I+1 - RPIX) * DELT + RVAL
           YR = ADD2REAL(ADDR + I, TYPE)
           CALL PGDRAW(XR, YR)
 20     CONTINUE
        CALL PGIDEN
C
C       Wait for a user response
C
C       CALL pgcurse(xr,yr, type)
C
C       All done
C
        CALL PGEND

        RETURN
100     FORMAT(I2)
        END
