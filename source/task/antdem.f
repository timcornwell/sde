C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)antdem.f	1.2	 5/20/94
C
      SUBROUTINE SDEMAIN
C
CD Program for interactive improvement of antenna locations
C
C Audit trail:
C	Original version
C					T.J. Cornwell Sept 26 1989
C	Changed call to VISCAT
C					T.J. Cornwell Nov 27 1990
C	Fixed edit option
C					T.J. Cornwell Jan 28 1991
C	Added uvdiff option
C					T.J. Cornwell Jan 31 1991
C	Added shift and center options
C					T.J. Cornwell Feb 6 1991
C	Merged all interactive options and cleaned up. Added scaling
C					T.J. Cornwell April 18 1991
C
C	An odd history:  In 1991, this task was taken over by Jing Ping Ge
C	and Digital Elevation Models were read in for MMA configuration
C	studies.  I have come back and cleaned up the code a bit and
C	made a few more things work.  This program is similar in capabilities
C	to ANT, but almost everything in this code is directed towards
C	LOCAL geomoetry rather than GLOBAL geometry:  when you SHIFT
C	or CENTER, its all done LOCALLY.
C					M.A. Holdaway	March 26 1993
C	Removed UVHARD and ANHARD options, which caused the program to fail
C					M.A. Holdaway	May 20 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ANTDEM')
C
C Go commands
C
      INTEGER		NPOSS
      PARAMETER		(NPOSS=17)
      CHARACTER*(SYSMXNAM)	GC
      CHARACTER*8	MNMAT, POSSGC(NPOSS)
C
C Variables
C
      INTEGER		        NDUMMY, STAT, SYSSYSTM,
     $				NSKIP
      LOGICAL			AUTOC, DATEXIST, AUTOS, POPEN, PNUMB
      REAL			HALIMITS(2), DEC, ELMIN, INTTIME,
     $   			HAMIN, HAMAX, FREQ, STRETCH(3), CC(3),
     $				C(3), XLIM(2), YLIM(2)
      DOUBLE PRECISION		LONG, LAT
      CHARACTER*(SYSMXNAM)	ANTFILE, NANTFILE, UVPLFILE, 
     $   			TANTFILE, COORD, 
     $				STRTMP
C
      INTEGER		NC, BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL		PLEV, CLEV, PIXR(2)
      CHARACTER*(SYSMXNAM) 	DEVICE, PLOT, INFILE, OINFILE, IMAGE
      CHARACTER*(SYSMXNAM)	TITLE
C
C Possible go commands
C
      DATA	POSSGC	/'help', 'edit', 'get', 'save', 'list', 
     $             'stretch', 'uvplt', 'anplt', 
     $             'center', 'shift', 'uvdiff', 'uvinit', 'uvaccum', 
     $             'uvlist', 'implot','elvation', 'quit'/
C
C Name of temporary antenna file for editing
C
      DATA	TANTFILE	/'/tmp/Antfile'/
C==================================================================
C
C Start of loop over go commands
C
      CALL MSGWELCO ('I allow interactive array building')
 1    CONTINUE
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC('Nantfile', NANTFILE, 1, NDUMMY)
      CALL USRGETC ('Device', DEVICE, 1, NDUMMY)
      CALL USRGETC('UVPlotfile', UVPLFILE, 1, NDUMMY)
      CALL USRGETR('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETR('Stretch', STRETCH, 3, NDUMMY)
      CALL USRGETR('Center', C, 3, NDUMMY)
      CALL USRGETR('Dec', DEC, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR('INTtime', INTTIME, 1, NDUMMY)
      CALL USRGETL ('AutoC', AUTOC, 1, NDUMMY)
      CALL USRGETL ('AutoS', AUTOS, 1, NDUMMY)
      CALL USRGETR('Xlimits', XLIM, 2, NDUMMY)
      CALL USRGETR('Ylimits', YLIM, 2, NDUMMY)
      CALL USRGETI('Nskip', NSKIP, 1, NDUMMY)
      CALL USRGETC('Coordinate', COORD, 1, NDUMMY)
C
      CALL USRGETI ('Ncont', NC, 1,NDUMMY)
      CALL USRGETR ('Plev', PLEV, 1, NDUMMY)
      CALL USRGETR ('Clev', CLEV, 1, NDUMMY)
      CALL USRGETR ('Pixrange', PIXR, 2, NDUMMY)
      CALL USRGETC ('Plot', PLOT, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETL ('PlotNumb', PNUMB, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL ERRCANCE
      HAMIN = MIN (HALIMITS(1), HALIMITS(2))
      HAMAX = MAX (HALIMITS(1), HALIMITS(2))
      IF(ERROR) GO TO 990
C
C Assemble plot commands
C
C
C
C Minmatch go command
C
      CALL USRGETGO (GC)
      CALL STRMNMAT (GC, POSSGC, NPOSS, MNMAT)
      IF(ERROR) GO TO 990
      IF (MNMAT.EQ.' ') THEN
         GC='help'
      ELSE
         GC = MNMAT
      END IF
C
C Go through all the things we can do
C
      IF (GC.EQ.'help') THEN
         CALL ANTHELP
C
C Get antenna file
C
      ELSEIF (GC.EQ.'get') THEN
         IF (ANTFILE.NE.' ') THEN
            CALL MSGPUT ('Getting antenna file from '//ANTFILE, 'I')
            IF(DATEXIST('Antfile')) THEN
               CALL DATDELET ('Antfile')
            END IF
            CALL FILGETAN ('Antfile', ANTFILE)
            CALL DATGETD ('Antfile', 'SITELAT', LAT, 1, NDUMMY)
            CALL DATGETD ('Antfile', 'SITELONG', LONG, 1, NDUMMY)
         ELSE
            CALL MSGPUT ('Need to specify Antfile', 'E')
            GO TO 1
         END IF
C
C Save antenna file
C
      ELSE IF (GC.EQ.'save') THEN
         CALL MSGPUT ('Putting antenna file to '//NANTFILE, 'I')
         CALL FILPUTAN ('Antfile', NANTFILE, COORD)
C
C List antenna file
C
      ELSEIF (GC.EQ.'list') THEN
         IF (.NOT.DATEXIST('Antfile')) THEN
            CALL FILGETAN ('Antfile', ANTFILE)
         END IF
         CALL MSGPUT ('Listing antenna file '//ANTFILE, 'I')
         CALL FILPUTAN ('Antfile', '/dev/tty', COORD)
C
C Edit antenna file
C
      ELSEIF (GC.EQ.'edit') THEN
         CALL MSGPUT ('Editing antenna file', 'I')
         IF (.NOT.DATEXIST('Antfile')) THEN
            CALL FILGETAN ('Antfile', ANTFILE)
         END IF
         CALL FILPUTAN ('Antfile', TANTFILE, COORD)
         CALL DATGETC ('SYI', 'Editor', STRTMP, 1, NDUMMY)
         STAT = SYSSYSTM(STRTMP//' '//TANTFILE)
         CALL DATDELET ('Antfile')
         CALL FILGETAN ('Antfile', TANTFILE)
         STAT = SYSSYSTM ('/bin/rm '//TANTFILE)
C
C Stretch antenna locations
C
      ELSE IF (GC.EQ.'stretch') THEN
         CALL MSGPUT ('Stretching array', 'I')
         CALL ARRSCALE ('Antfile/LX', STRETCH(1), 0.0, 'Antfile/LX')
         CALL ARRSCALE ('Antfile/LY', STRETCH(2), 0.0, 'Antfile/LY')
         CALL ARRSCALE ('Antfile/LZ', STRETCH(3), 0.0, 'Antfile/LZ')
C
C Center antenna locations
C
      ELSE IF (GC.EQ.'center') THEN
         CALL MSGPUT ('Centering array in local coords', 'I')
         CALL ARRSTAT ('Antfile/RX', ' ')
         CALL ARRSTAT ('Antfile/RY', ' ')
         CALL ARRSTAT ('Antfile/RZ', ' ')
         CALL DATGETR ('Antfile/RX', 'ARRAVE', CC(1), 1, NDUMMY)
         CALL DATGETR ('Antfile/RY', 'ARRAVE', CC(2), 1, NDUMMY)
         CALL DATGETR ('Antfile/RZ', 'ARRAVE', CC(3), 1, NDUMMY)
         CALL ARRSCALE ('Antfile/RX', 1.0, -CC(1), 'Antfile/RX')
         CALL ARRSCALE ('Antfile/RY', 1.0, -CC(2), 'Antfile/RY')
         CALL ARRSCALE ('Antfile/RZ', 1.0, -CC(3), 'Antfile/RZ')
         CALL ARRL2G ('Antfile')
C
C Shift antenna locations
C
      ELSE IF (GC.EQ.'shift') THEN
         CALL MSGPUT ('Shifting array in local coords', 'I')
         CALL ARRSTAT ('Antfile/RX', ' ')
         CALL ARRSTAT ('Antfile/RY', ' ')
         CALL ARRSTAT ('Antfile/RZ', ' ')
         CALL DATGETR ('Antfile/RX', 'ARRAVE', CC(1), 1, NDUMMY)
         CALL DATGETR ('Antfile/RY', 'ARRAVE', CC(2), 1, NDUMMY)
         CALL DATGETR ('Antfile/RZ', 'ARRAVE', CC(3), 1, NDUMMY)
         CALL ARRSCALE ('Antfile/RX', 1.0, C(1)-CC(1), 'Antfile/RX')
         CALL ARRSCALE ('Antfile/RY', 1.0, C(2)-CC(2), 'Antfile/RY')
         CALL ARRSCALE ('Antfile/RZ', 1.0, C(3)-CC(3), 'Antfile/RZ')
         CALL ARRL2G ('Antfile')
C
C Imgplot here
C
      ELSE IF(GC.EQ.'implot') THEN
         IF (INFILE.NE.OINFILE) THEN
            CALL FILIMGGE ('Image', INFILE, ' ')
         ENDIF
C
         OINFILE=INFILE
         IF (ERROR) GO TO 999
         IMAGE = 'Image'
         TITLE = INFILE(1:20)//NANTFILE(1:20)
         CALL IMGCONTR (IMAGE, TITLE, DEVICE, PLOT, PIXR,
     $      BLC, TRC, CLEV, 
     $      PLEV, NC, .FALSE.)
         POPEN = .TRUE.
         CALL ARRDEMAN ('Image', 'Antfile')
C
C Plot antenna locations
C
      ELSE IF(GC.EQ.'anplt') THEN
         IF(.NOT.DATEXIST('Antfile')) THEN
            CALL MSGPUT ('No antenna locations', 'I')
         ELSE
            CALL ARRANPLT ('Antfile', DEVICE, AUTOS, XLIM, YLIM, 
     $         POPEN, PNUMB)
            IF (POPEN) POPEN = .FALSE.
         END IF
C
C Quit
C
      ELSE IF (GC .EQ. 'quit') THEN
         GO TO 999
      ELSE
C
C Unknown!
C
         CALL MSGPUT ('Unknown command', 'W')
         CALL ANTHELP
      END IF
C
C Live dangerously
C
      IF(ERROR) THEN
         CALL MSGPUT ('Error found: continuing anyway', 'W')
         CALL ERRFLUSH
         CALL ERRCANCE
      END IF
      GO TO 1
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE ANTHELP
C
C Print help info for ant task
C
C
C
C Audit trail:
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'ANTHELP')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Possible commands:', 'I')
      CALL MSGPUT ('   get       - get from antfile', 'I')
      CALL MSGPUT ('   save      - save to Nantfile', 'I')
      CALL MSGPUT ('   list      - list antennas', 'I')
      CALL MSGPUT ('   edit      - edit antennas', 'I')
      CALL MSGPUT ('   stretch   - stretch array', 'I')
      CALL MSGPUT ('   center    - shift array center to zero', 'I')
      CALL MSGPUT ('   shift     - shift center to specified point',
     $   'I')
      CALL MSGPUT ('   anplt     - plot antennas', 'I')
      CALL MSGPUT ('   uvplt     - plot uv plane', 'I')
      CALL MSGPUT ('   uvinit    - initialize uv coverage',
     $      'I')
      CALL MSGPUT ('   uvaccum   - accumulate uv coverage',
     $      'I')
      CALL MSGPUT ('   uvdiff    - difference uv coverage',
     $      'I')
      CALL MSGPUT ('   implot    - plot the contour', 'I')
      CALL MSGPUT ('   elvation  - get elvation from dem image', 'I')
      CALL MSGPUT ('   quit      - quit program', 'I')
C
  999 CONTINUE
      END
