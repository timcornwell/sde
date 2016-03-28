C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arr2dplt.f	1.8    11/15/94
C
      SUBROUTINE ARR2DPLT (XNAME, YNAME, WNAME, PLOTOPTS)
C
CD Make a scatter plot using two arrays for coordinates
C
C	XNAME		CH*(*)	input	Directory name of x array
C	YNAME		CH*(*)	input	Directory name of y array
C	WNAME		CH*(*)	input	Directory name of weights array
C	PLOTOPTS	CH*(*)	input	Directory name of plot options
C					including the following:
C	PLOTOPTS/PLTLABEL	C	The title of the plot
C	PLOTOPTS/XLABEL		C	The x-axis label
C	PLOTOPTS/YLABEL		C	The y-axis label
C	PLOTOPTS/XCONV		C	The x-axis complex conversion mode
C	PLOTOPTS/YCONV		C	The y-axis complex conversion mode
C	PLOTOPTS/DEV		C	The plot device
C	PLOTOPTS/PTTYPE		I	The pgplot point style
C	PLOTOPTS/PTSKIP		I	The point plotting increment
C	PLOTOPTS/REVERSE	L	Plot reversed points as well
C	PLOTOPTS/AUTOSCL 	L	Scale automatically
C	PLOTOPTS/SQUARE 	L	Make plot square
C	PLOTOPTS/ID		L	Add plot ID?
C	PLOTOPTS/LEFT		L	Label Y axis on Left?
C	PLOTOPTS/CHEIGHT	R	Character height  (~.5 - ~1.5 is good)
C	PLOTOPTS/LWIDTH		I	Line width
C       PLOTOPTS/XLIMITS        R(2)    Min, Max
C       PLOTOPTS/YLIMITS        R(2)    Min, Max
C	PLOTOPTS/XTICK		R	X major tick interval
C	PLOTOPTS/YTICK		R	Y major tick interval
C	PLOTOPTS/XNSUB		I	number of X sub intervals
C	PLOTOPTS/YNSUB		I	number of Y sub intervals
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				W. Young, March 10, 1989
C       Added reverse
C                              T.J. Cornwell April 9, 1989
C       Added AUTOSCL, SQUARE and LIMITS
C                              T.J. Cornwell April 17, 1989
C       Allow independent autoscaling in X and Y.  Scaling logic
C       cleaned up. In previous version, AUTOSCL was being overridden
C       by the LIMITS options, and was effectively ignored.  Now, it
C       taken as an enable.
C                               D.S. Briggs   August 31, 1991
C
C	Added weight argument
C				D.S.Briggs	Jul 23 1992
C	Added ID, Left, CHeight & LWidth options, Annot
C				D.S.Briggs      July 22 1994
C	Added XCONV & YCONV
C				D.S.Briggs	July 28 1994
C	Added XTICK, XNSUB, YTICK, YNSUB
C				D.S.Briggs	Nov 15 1994
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	XNAME, YNAME, WNAME, PLOTOPTS
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARR2DPLT')
C
C Local variables
C
      CHARACTER*80	PLOTINFO(10)
      INTEGER		NUMXPTS, NUMYPTS, NUMWPTS, NUMPTS, LWIDTH
      CHARACTER 	ATYPE*1
      REAL              XLIMITS(2), YLIMITS(2), CHEIGHT, XTICK, YTICK
      INTEGER		XNSUB, YNSUB
      INTEGER 		XNAX, XNAXIS(SYSMXDIM)
      INTEGER 		YNAX, YNAXIS(SYSMXDIM)
      INTEGER 		WNAX, WNAXIS(SYSMXDIM)
      INTEGER 		XADDR, YADDR, WADDR, NDUMMY, SYMBOL, INCR
      LOGICAL		REVERSE, SQUARE, AUTOSCL, ID, LEFT,
     $   		XAUTOSCL, YAUTOSCL
C
      LOGICAL		DATEXIST
      CHARACTER		STRM2*(SYSMXNAM)
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get x-array attributes
C
      CALL DATGETAR (XNAME, XNAX, XNAXIS, ATYPE, XADDR)
      IF(XNAX .EQ. 1)THEN
         NUMXPTS = XNAXIS(1)
         PLOTINFO(5) = ATYPE
      ELSE
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'X-AXIS NOT ONE DIMENSIONAL')
         GOTO 999
      END IF
C
C Get y-array attributes
C
      CALL DATGETAR (YNAME, YNAX, YNAXIS, ATYPE, YADDR)
      IF(YNAX .EQ. 1)THEN
         PLOTINFO(6) = ATYPE
         NUMYPTS = YNAXIS(1)
      ELSE
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 'Y-AXIS NOT ONE DIMENSIONAL')
         GOTO 999
      END IF
C
C Get weight array attributes, or make one
C
      IF (WNAME.NE.' ') THEN
         CALL DATGETAR (WNAME, WNAX, WNAXIS, ATYPE, WADDR)
         IF(YNAX .EQ. 1)THEN
            NUMWPTS = YNAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE,
     $         'WEIGHT ARRAY NOT ONE DIMENSIONAL')
            GOTO 999
         END IF
      ELSE
         ATYPE = 'R'
         NUMWPTS = NUMXPTS
         CALL DATMAKAR ('WeightScratch', 1, NUMWPTS, ATYPE, WADDR)
         CALL ARRSETCO ('WeightScratch', 0.0, 1.0)
      END IF
      PLOTINFO(7) = ATYPE
C
C Get Plot label Goodies
C
      IF (DATEXIST(STRM2(PLOTOPTS, 'PLTLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'PLTLABEL', PLOTINFO(1), 1, NDUMMY)
      ELSE
         PLOTINFO(1) = 'SDE 2-D SCATTER PLOT'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'XLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'XLABEL', PLOTINFO(2), 1, NDUMMY)
      ELSE
         PLOTINFO(2) = XNAME
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'YLABEL', PLOTINFO(3), 1, NDUMMY)
      ELSE
         PLOTINFO(3) = YNAME
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'XCONV'))) THEN
         CALL DATGETC (PLOTOPTS, 'XCONV', PLOTINFO(9), 1, NDUMMY)
      ELSE
         PLOTINFO(9) = 'AMP'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YCONV'))) THEN
         CALL DATGETC (PLOTOPTS, 'YCONV', PLOTINFO(10), 1, NDUMMY)
      ELSE
         PLOTINFO(9) = 'AMP'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'DEV'))) THEN
         CALL DATGETC (PLOTOPTS, 'DEV', PLOTINFO(4), 1, NDUMMY)
      ELSE
         PLOTINFO(4) = '?'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'PTTYPE'))) THEN
         CALL DATGETI (PLOTOPTS, 'PTTYPE', SYMBOL, 1, NDUMMY)
      ELSE
         SYMBOL = 1
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'PTSKIP'))) THEN
         CALL DATGETI (PLOTOPTS, 'PTSKIP', INCR, 1, NDUMMY)
      ELSE
         INCR = 1
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'LWIDTH'))) THEN
         CALL DATGETI (PLOTOPTS, 'LWIDTH', LWIDTH, 1, NDUMMY)
      ELSE
         LWIDTH = 1
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'CHEIGHT'))) THEN
         CALL DATGETR (PLOTOPTS, 'CHEIGHT', CHEIGHT, 1, NDUMMY)
      ELSE
         CHEIGHT = 1.0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'ANNOT'))) THEN
         CALL DATGETC (PLOTOPTS, 'ANNOT', PLOTINFO(8), 1, NDUMMY)
      ELSE
         PLOTINFO(8) = ' '
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'REVERSE'))) THEN
         CALL DATGETL (PLOTOPTS, 'REVERSE', REVERSE, 1, NDUMMY)
      ELSE
         REVERSE = .FALSE.
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'SQUARE'))) THEN
         CALL DATGETL (PLOTOPTS, 'SQUARE', SQUARE, 1, NDUMMY)
      ELSE
         SQUARE = .FALSE.
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'ID'))) THEN
         CALL DATGETL (PLOTOPTS, 'ID', ID, 1, NDUMMY)
      ELSE
         ID = .TRUE.
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'LEFT'))) THEN
         CALL DATGETL (PLOTOPTS, 'LEFT', LEFT, 1, NDUMMY)
      ELSE
         LEFT = .TRUE.
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'AUTOSCL'))) THEN
         CALL DATGETL (PLOTOPTS, 'AUTOSCL', AUTOSCL, 1, NDUMMY)
      ELSE
         AUTOSCL = .TRUE.
      END IF
      XAUTOSCL = AUTOSCL
      YAUTOSCL = AUTOSCL
      IF (DATEXIST(STRM2(PLOTOPTS, 'XLIMITS'))) THEN
         CALL DATGETR (PLOTOPTS, 'XLIMITS', XLIMITS, 2, NDUMMY)
         IF (XLIMITS(1).NE.XLIMITS(2)) XAUTOSCL = .FALSE.
      ELSE
         XLIMITS(1) = 0.0
         XLIMITS(2) = 0.0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YLIMITS'))) THEN
         CALL DATGETR (PLOTOPTS, 'YLIMITS', YLIMITS, 2, NDUMMY)
         IF (YLIMITS(1).NE.YLIMITS(2)) YAUTOSCL = .FALSE.
      ELSE
         YLIMITS(1) = 0.0
         YLIMITS(2) = 0.0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'XTICK'))) THEN
         CALL DATGETR (PLOTOPTS, 'XTICK', XTICK, 1, NDUMMY)
      ELSE
         XTICK = 0.0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YTICK'))) THEN
         CALL DATGETR (PLOTOPTS, 'YTICK', YTICK, 1, NDUMMY)
      ELSE
         YTICK = 0.0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'XNSUB'))) THEN
         CALL DATGETI (PLOTOPTS, 'XNSUB', XNSUB, 1, NDUMMY)
      ELSE
         XNSUB = 0
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YNSUB'))) THEN
         CALL DATGETI (PLOTOPTS, 'YNSUB', YNSUB, 1, NDUMMY)
      ELSE
         YNSUB = 0
      END IF
C
C  Do your thing
C
      IF (NUMXPTS .LT. NUMYPTS)THEN
          NUMPTS = NUMXPTS
      ELSE
          NUMPTS = NUMYPTS
          IF( NUMXPTS .NE. NUMYPTS)THEN
             CALL ERRREPOR(ERRTRUNC, ROUTINE, 
     &          'ARRAYS HAVE UNEQUAL SIZES.')
          END IF
      END IF
      IF (NUMWPTS.LT.NUMPTS)
     $   CALL ERRREPOR(ERRTRUNC, ROUTINE, 'Not enough weights')
C
      CALL PLT2DPTS(NUMPTS, XADDR, YADDR, WADDR, PLOTINFO, SYMBOL,
     $   INCR, REVERSE, SQUARE, XAUTOSCL, XLIMITS(1), XLIMITS(2),
     $   XTICK, XNSUB, YAUTOSCL, YLIMITS(1), YLIMITS(2), YTICK, YNSUB,
     $   ID, LEFT, CHEIGHT, LWIDTH)
C
      IF (WNAME.EQ.' ') CALL DATDELET ('WeightScratch')
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
