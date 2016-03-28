C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pltanpts.f	1.16 1/12/95
C
      SUBROUTINE PLTANPTS(NANT, LX, LY, LZ, DM, DEVNAME,
     1    SITELAT, AUTOS, XLIM, YLIM, POPEN, PNUMB)
C
C Plot antenna positions in local coordinates and allow editting
C of positions. Antennas are flagged by setting the diameter to be
C less than zero.
C
C
C      NANT    INT	input	Number of elements
C      LX, etc	DOUBLE	input	Antenna positions
C	DEVNAME	CHAR(*)	input	Device name for plotting
C	SITELAT	DOUBLE	input	Site latitude (radians)
C	AUTOS	L	input	Do autoscaling
C	XLIM	R(*)	input	X limits of the graph
C	YLIM	R(*)	input	Y Limits of the graph
C	POPEN	L	input	IS plot file ALREADY OPEN?
C	PNUMB	L	input	Plot Numbers?
C
C Audit trail:
C	Switched X and Y in PGENV to match other calls
C				T.J. Cornwell Jan 28 1991
C	Fixed initialization of XMAX, YMAX, removed squaring of
C	plot
C				T.J. Cornwell Jan 30 1991
C       Add in blow up window using the cursor
C                               J. Ge Feb 07 1991
c       Plot the deleted antenna in different symbol
C                               J. Ge Feb 22 1991
C	Merged all interactive options and cleaned up
C				T.J. Cornwell April 18 1991
C       Plot the antenna numbers
C                               J. Ge May 7 1991
C	Added POPEN, PNUMB, prepared to merge with standard SDE PLTANPTS
C				M.A. Holdaway	March 16 1993
C	Changed comment of POPEN, changed no code
C				M.A. Holdaway	April 3 1993
C	Now prints out the Device name in the title line
C				M.A. Holdaway	May 20 1994
C	Change "CALL PGADVANCE" -> "CALL PGPAGE" for pgplot.
C				M. Stupar	Jan 5 1995
C--------------------------------------------------------------------
#include        "stdinc.h"
C
        INTEGER         NANT
        CHARACTER *(*)  DEVNAME
        LOGICAL		AUTOS, POPEN, PNUMB
        DOUBLE PRECISION	LX(*), LY(*), LZ(*), DM(*)
        DOUBLE PRECISION	SITELAT
        REAL		XLIM(*), YLIM(*)
C
C Local variables
C
        REAL            XMIN, XMAX
        REAL            YMIN, YMAX
        REAL            DELTA
C
C Plot information mapped variables
C       
        INTEGER         I,J,NC
        CHARACTER *(80) STRING,TEST1
        CHARACTER *(80) PLOTNAME
        CHARACTER *(80) XLABEL
        CHARACTER *(80) YLABEL
        CHARACTER *1    XTYPE
C
        INTEGER		ISEL
        DOUBLE PRECISION	EARTH(3), LOCAL(3), LONG
        REAL		DIST, DISTSEL, XN, YN
C=====================================================================
C
        IF(ERROR) GO TO 999
C
        PLOTNAME = 'Antenna locations for '//DEVNAME
        XLABEL = 'East (meters)'
        YLABEL = 'North (meters)'
C 
C       Set things up find min-max etc.
C
        IF (.NOT.POPEN) THEN
           CALL PGBEGIN(0, DEVNAME, 1,1)
        ENDIF
 1      CONTINUE
        CALL PGASK (.FALSE.)
        EARTH(1)=LX(1)
        EARTH(2)=LY(1)
        EARTH(3)=LZ(1)
        CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
        XMIN = LOCAL(1)
        XMAX = LOCAL(1)
        YMIN = LOCAL(2)
        YMAX = LOCAL(2)
        DO 10 I=2,NANT
           IF(DM(I).GT.0.0) THEN
              EARTH(1)=LX(I)
              EARTH(2)=LY(I)
              EARTH(3)=LZ(I)
              CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
              IF(XMIN .GE. LOCAL(1))XMIN = LOCAL(1)
              IF(XMAX .LT. LOCAL(1))XMAX = LOCAL(1)
              IF(YMIN .GE. LOCAL(2))YMIN = LOCAL(2)
              IF(YMAX .LT. LOCAL(2))YMAX = LOCAL(2)
           END IF
 10     CONTINUE
C
C       Expand min-max's by 5% for a nice fit
C
        DELTA = XMAX - XMIN
        XMAX = XMAX + 0.05*DELTA
        XMIN = XMIN - 0.05*DELTA
C
        DELTA = YMAX - YMIN
        YMAX = YMAX + 0.05*DELTA
        YMIN = YMIN - 0.05*DELTA
C
        IF(.NOT.AUTOS) THEN
           XMIN=XLIM(1)
           XMAX=XLIM(2)
           YMIN=YLIM(1)
           YMAX=YLIM(2)
        ENDIF
C
C       Start the plotting now
C
 11     CONTINUE
        IF (.NOT.POPEN) THEN
           CALL PGENV(XMIN, XMAX, YMIN, YMAX, 1, 1)
           CALL PGLABEL(XLABEL,YLABEL,PLOTNAME)
        ENDIF
C
C       do the dirty work point by %$#^%$#^% point
C
        DO 20 I=1, NANT
C
C To get multiple arrays on same image with same numbers....
C
C           IF (I .GT. 40) THEN
C              J = I - 40
C              CALL PGNUMB(J,0,0,STRING,NC)
C              TEST1 = STRING(1:NC)
C           ELSE
           CALL PGNUMB(I,0,0,STRING,NC)
           TEST1 = STRING(1:NC)
C           ENDIF
           IF(DM(I).GT.0.0) THEN
              EARTH(1)=LX(I)
              EARTH(2)=LY(I)
              EARTH(3)=LZ(I)
              CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
              CALL PGPOINT(1,SNGL(LOCAL(1)),SNGL(LOCAL(2)),17)
           ELSE
              EARTH(1)=LX(I)
              EARTH(2)=LY(I)
              EARTH(3)=LZ(I)
              CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
              CALL PGPOINT(1,SNGL(LOCAL(1)),SNGL(LOCAL(2)),2)
           END IF
           IF (PNUMB) THEN
              CALL PGTEXT(SNGL(LOCAL(1)),SNGL(LOCAL(2)),TEST1)
           ENDIF
 20     CONTINUE
        CALL PGIDEN
c
c React
c
       CALL MSGPUT ('Action required?: Rescale, Move, Delete, Quit',
     $     'I')
       CALL MSGPUT (
     $    'Hit Q or <return> to quit now, else R or M or D', 'I')
       CALL PGCURSE(XN, YN, XTYPE)
       IF((XTYPE.EQ.'R').OR.(XTYPE.EQ.'r')) THEN
          CALL MSGPUT ('Rescaling plot', 'I')
          XMIN = XN
          YMIN = YN
          WRITE (MESSAGE, 1000) ISEL
 1001     FORMAT ('Select the top right position')
          CALL MSGPUT ('Move cursor to top right position', 'I')
          CALL PGUPDT
          CALL PGCURSE(XN, YN, XTYPE)
          XMAX = XN
          YMAX = YN
          IF (.NOT.POPEN) CALL PGPAGE
          GO TO 11
       ELSEIF((XTYPE.EQ.'M').OR.(XTYPE.EQ.'m').OR.
     $        (XTYPE.EQ.'D').OR.(XTYPE.EQ.'d')) THEN
          DISTSEL = 1E20
          DO 30 I = 1, NANT
             IF(DM(I).GT.0.0) THEN
                EARTH(1)=LX(I)
                EARTH(2)=LY(I)
                EARTH(3)=LZ(I)
                CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
                DIST = (LOCAL(1)-XN)**2 + (LOCAL(2)-YN)**2
                IF(DISTSEL.GT.DIST) THEN
                   ISEL = I
                   DISTSEL = DIST
                END IF
             END IF
 30       CONTINUE
          IF ((ISEL.LT.1).OR.(ISEL.GT.NANT)) THEN
             CALL MSGPUT ('Illegal antenna selected', 'W')
             GO TO 3
          END IF
          IF ((XTYPE.EQ.'M').OR.(XTYPE.EQ.'m')) THEN
             WRITE (MESSAGE, 1000) ISEL
 1000        FORMAT ('Moving antenna ', I3, ' to new position')
             CALL MSGPUT (MESSAGE, 'I')
             CALL MSGPUT ('Move cursor to new position', 'I')
             CALL PGUPDT
             CALL PGCURSE(XN, YN, XTYPE)
             CALL MSGPUT ('Moving...', 'I')
             EARTH(1)=LX(ISEL)
             EARTH(2)=LY(ISEL)
             EARTH(3)=LZ(ISEL)
             CALL UTLG2L(EARTH, LONG, SITELAT, LOCAL)
             LOCAL(1)=XN
             LOCAL(2)=YN
             CALL UTLL2G(LOCAL, LONG, SITELAT, EARTH)
             LX(ISEL) = EARTH(1)
             LY(ISEL) = EARTH(2)
             LZ(ISEL) = EARTH(3)
          ELSE IF ((XTYPE.EQ.'D').OR.(XTYPE.EQ.'d')) THEN
             WRITE (MESSAGE, 1100) ISEL
 1100        FORMAT ('Deleting antenna ', I3)
             CALL MSGPUT (MESSAGE, 'I')
             DM(ISEL) = -1.0
          END IF
       ELSE
          CALL MSGPUT ('Done', 'I')
          GO TO 3
       END IF
C
       CALL PGUPDT
C
C Stop this plot and go back for another
C
       IF (.NOT.POPEN) CALL PGPAGE
       GO TO 1
C
C       All done
C
 3     CONTINUE
       CALL PGEND

 999   CONTINUE
       END
