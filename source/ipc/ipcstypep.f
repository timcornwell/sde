C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcstypep.f	1.4    1/28/93
C
      SUBROUTINE IPCSTYPEP (HANDLE, PHOTONS, XCOORD, YCOORD, 
     $     FRAME, NPHOTONS)
C
CD List ipcs data
C
C	handle	CH*(*)	input	Handle of output file for listing
C	photons	CMPLX	input	input photons
C	xcoord	REAL(*)	input	x coordinates
C	ycoord	REAL(*)	input	y coordinates
C	frame	REAL(*)	input	frame numbers
C	nphotons INT	input	Number of photons
C Audit trail:
C	Cloned from vislistp
C				R.G. Marson     Dec 29 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NPHOTONS
      REAL		PHOTONS(*), XCOORD(*), YCOORD(*), FRAME(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSTYPEP')
C
      INTEGER		IPH, X, Y, FR, PH
      CHARACTER*31	LINE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Write heading
C
      MESSAGE = ' Frame  X-coord  Y-coord  Photons'
      CALL TXTWRITE (HANDLE, MESSAGE)
C
C Loop over data
C
      DO 100 IPH = 1, NPHOTONS
         X = NINT(XCOORD(IPH)) + 1
         Y = NINT(YCOORD(IPH)) + 1
         FR = NINT(FRAME(IPH))
         PH = NINT(PHOTONS(IPH))
         WRITE (LINE, 1000) FR, X, Y, PH
 1000    FORMAT (I6, 4X,I3, 6X,I3, 3X, I6)
         CALL TXTWRITE (HANDLE, LINE)
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
