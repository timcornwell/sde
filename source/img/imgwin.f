C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgwin.f	1.1    12/7/90
C
      SUBROUTINE IMGWIN (INIMAGE, WINDOW, BLANK, OUTIMAGE)
C
CD Given a image and a window blank out everything outside the window
C
C Given a input image this routine will produce an identical output
C image except that everything outside the WINDOW will be set to the
C BLANK value
C
C	INIMAGE 	CH*(*)	input	Directory entry of input image
C	WINDOW  	CH*(*)	input	Directory entry of window
C	BLANK   	REAL	input	Blanking value
C	OUTIMAGE 	CH*(*)	input	Directory entry of output image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 15 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      CHARACTER*(*)	INIMAGE, WINDOW, OUTIMAGE
      REAL              BLANK
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGWIN')
C
C Function Declarations
C
      LOGICAL           DATEXIST
C
C Local Variables
C
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*1       ARRTYPE
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER           NAX, NAXIS(SYSMXDIM), INADD, OUTADD
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get input image 
C
      CALL CRDGET (INIMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL DATGETAR (INIMAGE, NAX, NAXIS, ARRTYPE, INADD)
      IF (ERROR) GOTO 990
C
C Cut out the window bit
C
      CALL ARRSUBSE (INIMAGE, 'IMGWINTEMP', WINDOW)
C
C Create output array with coordinates
C
      IF (INIMAGE.NE.OUTIMAGE) THEN
         IF (DATEXIST(OUTIMAGE)) CALL DATDELET(OUTIMAGE)
         CALL DATMAKAR (OUTIMAGE, NAX, NAXIS, ARRTYPE, OUTADD)
         CALL HEDCOPY (INIMAGE, OUTIMAGE)
         CALL CRDPUT (OUTIMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     $        ROTA)
      END IF
      CALL ARRSETCO (OUTIMAGE, 0.0, BLANK)
C
C Insert the tempory array back in
C
      CALL ARRINSER('IMGWINTEMP', OUTIMAGE, WINDOW)
      CALL DATDELET('IMGWINTEMP')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
