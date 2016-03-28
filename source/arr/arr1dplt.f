C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arr1dplt.f	1.5    7/21/93
C
      SUBROUTINE ARR1DPLT (NAME, PLOTOPTS)
C
CD   Make a scatter plot using two arrays for coordinates
C
C	name		CH*(*)	input	Directory name of array
C	plotopts	CH*(*)	input	Directory name of plot options
C					including the following:
C	plotopts/pltlabel	C	The title of the plot
C	plotopts/xlabel		C	The x-axis label
C	plotopts/ylabel		C	The y-axis label
C	plotopts/dev		C	The plot device
C	plotopts/pttype		I	The pgplot point style
C	plotopts/ptskip		I	The point plotting increment
C	plotopts/reverse	L	Plot reversed points as well
C	plotopts/square 	L	Make plot square
C      plotopts/xlimits        R(2)    Min, Max
C      plotopts/ylimits        R(2)    Min, Max
C
C Audit trail:
C      Cloned from arr2dplt.f
C                              R.G. Marson 18 Sep 1990
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, PLOTOPTS
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARR1DPLT')
C
C Local variables
C
      CHARACTER*80	PLOTINFO(6)
      INTEGER           NUMPTS
      CHARACTER*(1) 	ATYPE
      CHARACTER		STRM2*(SYSMXNAM)
      CHARACTER*8       CTYPE(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              DELT(SYSMXDIM)
      REAL              RPIX(SYSMXDIM)
      REAL              ROTA(SYSMXDIM)
      REAL              XLIMITS(2), YLIMITS(2)
      INTEGER 		NAX, NAXIS(SYSMXDIM)
      INTEGER 		ADDR, NDUMMY, SYMBOL, INCR, I
      LOGICAL		DATEXIST, REVERSE, SQUARE, AUTOSCL
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADDR)
      DO I = 1, NAX 
         IF (NAXIS(I) .EQ. 1) NAX = NAX - 1
      END DO
      IF(NAX .EQ. 1)THEN
         PLOTINFO(5) = ATYPE
      ELSE
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $        'Plot Array not one dimensional')
         GOTO 999
      END IF
C
C Get Co-ords
C
      CALL CRDGET(NAME, NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      DO I = 1, NAX
         IF (NAXIS(I) .NE. 1) THEN
            RVAL(1) = RVAL(I)
            RPIX(1) = RPIX(I)
            DELT(1) = DELT(I)
            NUMPTS = NAXIS(I)
            CTYPE(1) = CTYPE(I)
         END IF
      END DO
C
C Get Plot label Goodies
C
      IF (DATEXIST(STRM2(PLOTOPTS, 'PLTLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'PLTLABEL', PLOTINFO(1), 1, NDUMMY)
      ELSE
         PLOTINFO(1) = 'SDE 1-D PLOT'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'XLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'XLABEL', PLOTINFO(2), 1, NDUMMY)
      ELSE IF (CTYPE(1).NE.' ') THEN
         PLOTINFO(2) = CTYPE(1)
      ELSE
         PLOTINFO(2) = 'X axis'
      END IF
      IF (DATEXIST(STRM2(PLOTOPTS, 'YLABEL'))) THEN
         CALL DATGETC (PLOTOPTS, 'YLABEL', PLOTINFO(3), 1, NDUMMY)
      ELSE IF (DATEXIST(STRM2(NAME, 'BUNIT'))) THEN
         CALL DATGETC (NAME, 'BUNIT', PLOTINFO(3), 1, NDUMMY)
      ELSE
         PLOTINFO(3) = 'Y axis'
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
      IF (DATEXIST(STRM2(PLOTOPTS, 'AUTOSCL'))) THEN
         CALL DATGETL (PLOTOPTS, 'AUTOSCL', AUTOSCL, 1, NDUMMY)
      END IF
      IF (.NOT.AUTOSCL) THEN
         IF (DATEXIST(STRM2(PLOTOPTS, 'XLIMITS'))) THEN
            CALL DATGETR (PLOTOPTS, 'XLIMITS', XLIMITS, 2, NDUMMY)
         ELSE
            XLIMITS(1) = (1.0 - RPIX(1)) * DELT(1) + RVAL(1)
            XLIMITS(2) = (NUMPTS - RPIX(1)) * DELT(1) + RVAL(1)
         END IF
         IF (DATEXIST(STRM2(PLOTOPTS, 'YLIMITS'))) THEN
            CALL DATGETR (PLOTOPTS, 'YLIMITS', YLIMITS, 2, NDUMMY)
         ELSE
            YLIMITS(1) = 0.0
            YLIMITS(2) = 0.0
         END IF
      ELSE
         XLIMITS(1) = (1.0 - RPIX(1)) * DELT(1) + RVAL(1)
         XLIMITS(2) = (NUMPTS - RPIX(1)) * DELT(1) + RVAL(1)
         YLIMITS(1) = 0.0
         YLIMITS(2) = 0.0
      END IF
C
C  Do your thing
C
      CALL PLT1DPTS(NUMPTS, ADDR, PLOTINFO, SYMBOL, INCR,
     $   REVERSE, SQUARE, XLIMITS(1), XLIMITS(2), YLIMITS(1),
     $   YLIMITS(2), RVAL(1), RPIX(1), DELT(1))
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
