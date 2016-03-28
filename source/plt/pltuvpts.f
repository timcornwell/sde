C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pltuvpts.f	1.9	 5/20/94
C
      SUBROUTINE PLTUVPTS(NVIS, UU, VV, WT, DEVNAME, NSKIP, AUTOS,
     $   XLIM, YLIM)
C
CD Plot visibility points.
C
C
C      NVIS    INT	input	Number of elements
C      NSKIP   INT              skip every NSKIP point
C      UU, etc	DOUBLE	input	Antenna positions
C	DEVNAME	CHAR(*)	input	Device name for plotting
C
C Audit trail:
C      Allow user to define the coordinate (XMAX,XMIN,YMAX,YMIN)
C      Allow user select to plot part of the data
C      J. Ge                           Feb 08 1991
C	Changed to input scaling from higher up
C				T.J. Cornwell April 18 1991
C	Added some comments, no change in code
C				M.A.Holdaway	Nov 18 1991
C	Now prints out the Device name in the title line
C				M.A. Holdaway	May 20 1994
C--------------------------------------------------------------------
#include        "stdinc.h"
C
        INTEGER         NVIS, NSKIP
        CHARACTER *(*)  DEVNAME
        LOGICAL		AUTOS
        REAL	UU(*), VV(*), WT(*), XLIM(*), YLIM(*)
C
C Local variables
C
        REAL            XR, XMIN, XMAX
        REAL            YR, YMIN, YMAX
        REAL            DELTA
C
C Plot information mapped variables
C       
        INTEGER         I
        CHARACTER *(80) PLOTNAME
        CHARACTER *(80) XLABEL
        CHARACTER *(80) YLABEL
C=====================================================================
C
        IF(ERROR) GO TO 999
C
        PLOTNAME = 'UV sampling for '//DEVNAME
        XLABEL = 'U (wavelengths)'
        YLABEL = 'V (wavelengths)'
C
C       Set things up find min-max etc.
C
        CALL PGBEGIN(0, DEVNAME, 1,1)
C
        IF(AUTOS) THEN
           XR = UU(1)
           YR = VV(1)
           XMIN = XR
           XMAX = YR
           YMIN = XR
           YMAX = YR
           DO 10 I=2, NVIS
              IF(WT(I).GT.0.0) THEN
                 XR = UU(I)
                 YR = VV(I)
                 IF(XMIN .GE. XR)XMIN = XR
                 IF(XMAX .LT. XR)XMAX = XR
                 IF(YMIN .GE. YR)YMIN = YR
                 IF(YMAX .LT. YR)YMAX = YR
                 XR = -UU(I)
                 YR = -VV(I)
                 IF(XMIN .GE. XR)XMIN = XR
                 IF(XMAX .LT. XR)XMAX = XR
                 IF(YMIN .GE. YR)YMIN = YR
                 IF(YMAX .LT. YR)YMAX = YR
              END IF
 10        CONTINUE
C
           XMAX = MAX(XMAX, YMAX)
           YMAX = XMAX
           XMIN = MIN(XMIN, YMIN)
           YMIN = XMIN
           DELTA = XMAX - XMIN
           XMAX = XMAX + 0.05*DELTA
           XMIN = XMIN - 0.05*DELTA
C
           DELTA = YMAX - YMIN
           YMAX = YMAX + 0.05*DELTA
           YMIN = YMIN - 0.05*DELTA
        ELSE
           XMIN=XLIM(1)
           XMAX=XLIM(2)
           YMIN=YLIM(1)
           YMAX=YLIM(2)
        END IF
C
C       Start the plotting now
C
        CALL PGENV(XMIN, XMAX, YMIN, YMAX,1,1)
        CALL PGLABEL(XLABEL,YLABEL,PLOTNAME)
C
C       do the dirty work point by %$#^%$#^% point
C
        DO 20 I=1, NVIS, NSKIP
           IF(WT(I).GT.0.0) THEN
              XR = UU(I)
              YR = VV(I)
              CALL PGPOINT(1,XR,YR,-1)
              CALL PGPOINT(1,-XR,-YR,-1)
C
C Hey!, if -1 is too small for something, try SYMBOL = 210
C It worked for me.		-MH
C
C              CALL PGPOINT(1,XR,YR,210)
C              CALL PGPOINT(1,-XR,-YR,210)
           END IF
 20     CONTINUE
C
       CALL PGIDEN
       CALL PGEND

 999   CONTINUE
       END
