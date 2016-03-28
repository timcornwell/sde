C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgtvd.f	1.5	 7/20/92
C
      SUBROUTINE IMGTVD (IMAGE, PMIN, PMAX)
C
CD Display an image on a tv. This version assumes X11 with color TV
C display. Since the X TV exits with the program exit the routine 
C also writes a standard FITS
C file to the file $SDETV which can then be read using saoimage.
C PMIN and PMAX are obsolete.
C
C	IMAGE	CH*(*)	input	Name of image
C	PMIN	REAL	input	Minimum value to be displayed
C	PMAX	REAL	input	Maximum value to be displayed
C Audit trail:
C	Changed to IEEE Fits file
C				T.J. Cornwell	Feb 2  1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMAGE
      REAL		PMAX, PMIN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGTVD')
C
      CHARACTER*(SYSMXNAM)	TVFILE
C
      INTEGER 		IAX, NAX, NDUMMY, NREAL, BLC(SYSMXDIM), 
     1   		TRC(SYSMXDIM), NAXIS (SYSMXDIM), STRLEN,
     2                  NX,NY,NZ,AADD, DATADD
      REAL		AVE, RMS, DMAX, DMIN, DATFGETR, SUM
      REAL		DISP
      CHARACTER*(SYSMXNAM) 	IMGUNITS,
     3                          DISPLY
C
      DATA BLC /SYSMXDIM*1/
      DATA TRC /SYSMXDIM*1/
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL SYSGTENV ('SDETV', TVFILE)
      IF(TVFILE.EQ.' ') GO TO 999
C
      SYSMSG = .FALSE.
      CALL FILIMGPU (IMAGE, TVFILE, ' ')
      SYSMSG = .TRUE.
C
#if DO_X11
C
      CALL DATGETI (IMAGE//'/ARRAY', 'NAXIS', NAXIS, SYSMXDIM,
     1   NAX)
C
C Find number of real axes
C
      NREAL = NAX
   1  IF (NAXIS(NREAL).EQ.1) THEN
         NREAL = NREAL - 1
         GO TO 1
      END IF
C
      IF (NREAL.LE.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No real axes!')
         GO TO 999
      END IF
C      DMAX = DATFGETR(IMAGE, 'ARRMAX')
C      DMIN = DATFGETR(IMAGE, 'ARRMIN')
C      IF ((DMAX.NE.0.0) .OR. (DMIN .NE. 0.0)) THEN
C
C Validate BLC and TRC
C
      DO 10 IAX = 1, NREAL
         BLC(IAX) = 1
         TRC(IAX) = NAXIS(IAX)
  10  CONTINUE
      NX = TRC(1) - BLC(1) + 1
      NY = TRC(2) - BLC(2) + 1
      NZ = 1
      DO 15 IAX = NREAL+1, SYSMXDIM
         BLC (IAX) = 1
	 TRC (IAX) = 1
 15   CONTINUE
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
C      
C Call main routine to work on the ''Image'' array.
C
      CALL ARRSTAT (IMAGE, 'Window')
C      ENDIF
C
      AADD = DATADD(IMAGE)
C
      IF (ERROR) THEN
         GO TO 999
      END IF
C
C Write results to the message file
C
C
C IMGUNITS may not exist
C
      IF (ERROR) GO TO 999
      IMGUNITS = ' '
      CALL DATGETC (IMAGE, 'BUNIT', IMGUNITS, 1, NDUMMY)
      CALL ERRCANCE
C
C Get statistics from database
C
      AVE = DATFGETR(IMAGE, 'ARRAVE')
      RMS = DATFGETR(IMAGE, 'ARRRMS')
      DISP = DATFGETR(IMAGE, 'ARRDISP')
      DMAX = DATFGETR(IMAGE, 'ARRMAX')
      DMIN = DATFGETR(IMAGE, 'ARRMIN')
      SUM = DATFGETR(IMAGE, 'ARRSUM')
C
      CALL MSGPUT ('SdeTV display', 'I')
      WRITE (MESSAGE, 1300) DMAX, IMGUNITS(1:STRLEN(IMGUNITS))
 1300 FORMAT ('   Maximum = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1400) DMIN, IMGUNITS(1:STRLEN(IMGUNITS))
 1400 FORMAT ('   Minimum = ',1PE11.3,' ',A)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL SDETV(DISPLY,NX,NY,NZ,MEMR(AADD),DMAX,DMIN)
#endif
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      RETURN
      END
