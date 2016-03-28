C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ant.f	1.12	 5/20/94
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
C	Added Starting TIME to SIMUV call
C					M.A.Holdaway  May 12 1991
C	Added POPEN, PNUMB to make ARRANPLT compatible with JING PING
C	code in ANTDEM
C					M.A.Holdaway	March 26 1993
C	Had the logic of POPEN wrong
C					M.A.Holdaway	April 2 1993
C	Removed ANHARD, UVHARD options, which were making the
C	program fail.
C					M.A.Holdaway	May 20 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ANT')
C
C Go commands
C
      INTEGER		NPOSS
      PARAMETER		(NPOSS=15)
      CHARACTER*(SYSMXNAM)	GC
      CHARACTER*8	MNMAT, POSSGC(NPOSS)
C
C Variables
C
      INTEGER		        NDUMMY, GETPID, STAT, SYSSYSTM,
     $				NSEL, NSKIP
      LOGICAL			AUTOC, DATEXIST, AUTOS, POPEN, PNUMB
      LOGICAL			WCENTER
      REAL			HALIMITS(2), DEC, ELMIN, INTTIME,
     $   			HAMIN, HAMAX, FREQ, STRETCH(3), CC(3),
     $				UVLIMITS(2), TIMERANG(2), C(3), CG(3),
     $				XLIM(2), YLIM(2), TIME
      DOUBLE PRECISION		LONG, LAT, DC(3), DCG(3)
      CHARACTER*(SYSMXNAM)	ANTFILE, NANTFILE, UVPLFILE, ANPLFILE,
     $   			TANTFILE, COORD, 
     $				STRTMP
      CHARACTER*6		STRINT
C
C Possible go commands
C
      DATA	POSSGC	/'help', 'edit', 'get', 'save', 'list', 
     $             'stretch', 'uvplt', 'anplt', 
     $             'center', 'shift', 'uvdiff', 'uvinit', 'uvaccum', 
     $             'uvlist', 'quit'/
C
C Name of temporary antenna file for editing
C
      DATA	TANTFILE	/'/tmp/Antfile'/
      DATA	POPEN		/.FALSE./
C POPEN: (Is Plot File Already Open?)
      DATA	PNUMB		/.TRUE./
C==================================================================
C
C Various initializations
C
      CALL STRAPPEN (TANTFILE, STRINT (GETPID(NDUMMY)))
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
      CALL USRGETC('UVPlotfile', UVPLFILE, 1, NDUMMY)
      CALL USRGETC('AnPlotfile', ANPLFILE, 1, NDUMMY)
      CALL USRGETR('HAlimits', HALIMITS, 2, NDUMMY)
      CALL USRGETR('Stretch', STRETCH, 3, NDUMMY)
      CALL USRGETR('Center', C, 3, NDUMMY)
      CALL USRGETL('CenterOnWrite', WCENTER, 1, NDUMMY)
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
      HAMIN = MIN (HALIMITS(1), HALIMITS(2))
      HAMAX = MAX (HALIMITS(1), HALIMITS(2))
      TIME = 0.0
      IF(ERROR) GO TO 990
C
C Assemble plot commands
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
         ELSE
            CALL MSGPUT ('Need to specify Antfile', 'E')
            GO TO 1
         END IF
C
C Save antenna file
C
      ELSE IF (GC.EQ.'save') THEN
         CALL MSGPUT ('Putting antenna file to '//NANTFILE, 'I')
         CALL DATPUTL ('Antfile', 'CenterOnWrite', WCENTER, 1)
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
         CALL MSGPUT ('Centering array', 'I')
         CALL ARRSTAT ('Antfile/LX', ' ')
         CALL ARRSTAT ('Antfile/LY', ' ')
         CALL ARRSTAT ('Antfile/LZ', ' ')
         CALL DATGETR ('Antfile/LX', 'ARRAVE', CC(1), 1, NDUMMY)
         CALL DATGETR ('Antfile/LY', 'ARRAVE', CC(2), 1, NDUMMY)
         CALL DATGETR ('Antfile/LZ', 'ARRAVE', CC(3), 1, NDUMMY)
         CALL ARRSCALE ('Antfile/LX', 1.0, -CC(1), 'Antfile/LX')
         CALL ARRSCALE ('Antfile/LY', 1.0, -CC(2), 'Antfile/LY')
         CALL ARRSCALE ('Antfile/LZ', 1.0, -CC(3), 'Antfile/LZ')
C
C Shift antenna locations
C
      ELSE IF (GC.EQ.'shift') THEN
         CALL ARRSTAT ('Antfile/LX', ' ')
         CALL ARRSTAT ('Antfile/LY', ' ')
         CALL ARRSTAT ('Antfile/LZ', ' ')
         CALL DATGETR ('Antfile/LX', 'ARRAVE', CC(1), 1, NDUMMY)
         CALL DATGETR ('Antfile/LY', 'ARRAVE', CC(2), 1, NDUMMY)
         CALL DATGETR ('Antfile/LZ', 'ARRAVE', CC(3), 1, NDUMMY)
         IF (COORD(1:1).EQ.'L') THEN
            CALL MSGPUT ('Shifting array in local coordinates', 'I')
            CALL DATGETD ('Antfile', 'SITELAT', LAT, 1, NDUMMY)
            LONG=0.0D0
            DC(1) = C(1)
            DC(2) = C(2)
            DC(3) = C(3)
            CALL UTLL2G (DC, LONG, LAT, DCG)
            CG(1) = DCG(1)
            CG(2) = DCG(2)
            CG(3) = DCG(3)
            CALL ARRSCALE ('Antfile/LX', 1.0, CG(1)-CC(1), 'Antfile/LX')
            CALL ARRSCALE ('Antfile/LY', 1.0, CG(2)-CC(2), 'Antfile/LY')
            CALL ARRSCALE ('Antfile/LZ', 1.0, CG(3)-CC(3), 'Antfile/LZ')
         ELSE
            CALL MSGPUT ('Shifting array in geocentric coordinates',
     $         'I')
            CALL ARRSCALE ('Antfile/LX', 1.0, C(1)-CC(1), 'Antfile/LX')
            CALL ARRSCALE ('Antfile/LY', 1.0, C(2)-CC(2), 'Antfile/LY')
            CALL ARRSCALE ('Antfile/LZ', 1.0, C(3)-CC(3), 'Antfile/LZ')
         END IF
C
C Initialize uv data
C
      ELSE IF (GC.EQ.'uvinit') THEN
         IF(DATEXIST('OVis')) CALL DATDELET ('OVis')
         IF(DATEXIST('Vis')) CALL DATRENAM ('Vis', 'OVis')
C
C Accumulate uv data
C
      ELSE IF (GC.EQ.'uvaccum') THEN
         IF(DATEXIST('Vis'))  THEN
            CALL MSGPUT ('Adding to existing uv data', 'I')
            CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HAMIN), DBLE(HAMAX), 
     1         DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $         AUTOC, 1.0,
     $         'IVis')
            CALL VISCAT ('Vis', 'IVis', 'NVis', 'OBS', 'I')
            CALL DATDELET ('IVis')
            CALL DATDELET ('Vis')
            CALL DATRENAM ('NVis', 'Vis')
         ELSE
            CALL MSGPUT ('Making initial uv data', 'I')
            CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HAMIN), DBLE(HAMAX), 
     1         DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $         AUTOC, 1.0, 'Vis')
         END IF
C
C Plot uv data
C
      ELSE IF (GC.EQ.'uvplt') THEN
         IF(.NOT.DATEXIST('Vis')) THEN
            CALL MSGPUT ('No uv data', 'I')
         ELSE
            CALL ARRUVPLT ('Vis', 'OBS/I', UVPLFILE, NSKIP, AUTOS,
     $         XLIM, YLIM)
         END IF
C
C List uv data
C
      ELSE IF (GC.EQ.'uvlist') THEN
         IF(.NOT.DATEXIST('Vis')) THEN
            CALL MSGPUT ('No uv data', 'I')
         ELSE
            CALL TXTOPEN ('Listing', '/dev/tty', 'WRITE')
            CALL VISLISTT ('Listing', 'Vis', 'OBS/I', 0, .TRUE.)
            CALL TXTCLOSE ('Listing')
         END IF
C
C Plot antenna locations
C
      ELSE IF(GC.EQ.'anplt') THEN
         IF(.NOT.DATEXIST('Antfile')) THEN
            CALL MSGPUT ('No antenna locations', 'I')
         ELSE
            CALL ARRANPLT ('Antfile', ANPLFILE, AUTOS, XLIM, YLIM,
     $         POPEN, PNUMB)
         END IF
C
C Find points which moved
C
      ELSE IF (GC.EQ.'uvdiff') THEN
         IF(.NOT.DATEXIST('Vis')) THEN
            CALL MSGPUT ('No uv data', 'I')
         ELSEIF(.NOT.DATEXIST('OVis')) THEN
            CALL MSGPUT ('No previous uv data', 'I')
         ELSE
            TIMERANG(1)=0.0
            TIMERANG(2)=0.0
            UVLIMITS(1)=1.0
            UVLIMITS(2)=1.0E10
            CALL ARRSUBTR ('Vis/UU', 'OVis/UU', 'OVis/UU')
            CALL ARRSUBTR ('Vis/VV', 'OVis/VV', 'OVis/VV')
            CALL VISSEL ('OVis', 'OBS/I', TIMERANG, UVLIMITS, NSEL)
            CALL ARRCOPY ('OVis/OBS/I/WT', 'Vis/OBS/I/WT')
            WRITE (MESSAGE, 1200) NSEL
 1200       FORMAT (I7,' visibility points changed')
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
C Quit
C
      ELSE IF (GC.EQ.'quit') THEN
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
      CALL MSGPUT ('   quit      - quit program', 'I')
C
  999 CONTINUE
      END
