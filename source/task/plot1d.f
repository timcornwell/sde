C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)plot1d.f	1.3    7/19/93
C
      SUBROUTINE SDEMAIN
C
CD Program to plot a 1D image. This invokes pgplot to plot on a number 
CD of devices
C
CH @(#)plot1d.f	1.1    9/22/90
C
CS Arguments: CALL SDEMAIN
CD Audit trail:
C       Cloned from imgtv
C                                     R.G. Marson   Sep 19 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PLOT1D')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM) XLABEL, YLABEL, PLOTLABEL, DEVICE, INFILE

      REAL		XLIMITS(2) , YLIMITS(2)
C
C==================================================================
C
      CALL MSGWELCO ('I plot things (1D)')
 100  CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Image
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
      IF (ERROR) GO TO 999
      CALL DATCREAT('Plotopts')

      CALL USRGETR('xlimits', XLIMITS, 2, NDUMMY)
      IF (XLIMITS(1).NE.0.0.OR.XLIMITS(2).NE.0.0) THEN
         CALL DATPUTR('Plotopts', 'XLIMITS', XLIMITS, 2)
      END IF

      CALL USRGETR('ylimits', YLIMITS, 2, NDUMMY)
      IF (YLIMITS(1).NE.0.0.OR.YLIMITS(2).NE.0.0) THEN
         CALL DATPUTR('Plotopts', 'YLIMITS', YLIMITS, 2)
      END IF
	
      CALL USRGETC('xlabel', XLABEL, 1, NDUMMY)
      IF (XLABEL.NE. ' ') THEN
         CALL DATPUTC('Plotopts', 'XLABEL', XLABEL, 1)
      END IF

      CALL USRGETC('ylabel', YLABEL, 1, NDUMMY)
      IF (ylabel.NE. ' ') THEN
         CALL DATPUTC('Plotopts', 'YLABEL', YLABEL, 1)
      END IF

      CALL USRGETC('plotlabel', PLOTLABEL, 1, NDUMMY)
      IF (PLOTLABEL.NE. ' ') THEN
         CALL DATPUTC('Plotopts', 'PLTLABEL', PLOTLABEL, 1)
      ELSE
         CALL DATPUTC('Plotopts', 'PLTLABEL', INFILE, 1)
      END IF
      
      CALL USRGETC('device', DEVICE, 1, NDUMMY)
      IF (DEVICE.NE. ' ') THEN
         CALL DATPUTC('Plotopts', 'DEV', DEVICE, 1)
      END IF
      
      CALL ARR1DPLT('Image', 'Plotopts')
C     
 999  CONTINUE
      END
