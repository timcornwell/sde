C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsaoms.f	1.10    2/28/95
C
C Audit trail:
      SUBROUTINE IMGSAOMS (TCURFILE, TIMGTMPL, MASK, FILLVAL, BACKVAL,
     $   EXCLVAL)
C
CD Create mask from SAOImage cursor files.
C
C	TCURFILE CH*(*)	input	Filename of cursor file
C	TIMGTMPL CH*(*)	input	Name/Filename of image template
C	MASK	 CH*(*)	input	Name of output mask
C	FILLVAL	 REAL	input	Fill value in mask
C	BACKVAL	 REAL	input	Background value in mask
C	EXCLVAL	 REAL	input	Excluded region value in mask
C
C	If CURFILE is blank, the logical name SDECURSOR will be evaluated
C	for a name.  If this is not found, 'saoimage.reg' is used.  Likewise
C	the database is searched for an existing image of name IMGTMPL.
C	If this is not found, IMGTMPL is interpreted as a filename.  If
C	it is blank, the name in the cursor file will be used.
C
C	SAOImage cursor files only define a region on the sky in conjunction
C	with a coordinate system, normally found in an image.  We attempt
C	to read the image whose name is found in the cursor file header.
C	If it is not found, a warning message is printed, and the coordinate
C	system is taken from the template image.  Note that if the template
C	and original images are of different sizes, this might produce an
C	erroneous mask.  If neither image has a coordinate system, then
C	the center of the cursor reference image will be lined up on the
C	center of the template image.
C
C Audit trail:
C	Original version:
C	Broken out of MASK.F
C				D.S.Briggs	April 2 1992
C	If MASK already exists, do not reset to zero initially.  In this
C	case, the IMGTMPL input is not used.
C				D.S.Briggs	May 7 1992
C	If the image template is complex, use the modulus as the effective
C	template.
C				D.S.Briggs	Aug 22 1992
C	Bugfix in obscure combination of parameters.
C				D.S.Briggs	Aug 27 1992
C	Change template strategy -- original cursor diskfile takes
C	priority over the template.  Astronomical coordinate systems used
C	throughout, and transformed properly when present.  Boxes translated
C	to polygons prior to transformation.  Ellipses sampled at 8 points,
C	translated individually, and refitted in the target system.
C				D.S.Briggs	26 June 1993
C	Bugfix in polygon code
C				D.S.Briggs	18 May 1994
C	Clean up coordinate systems if necessary for 1-D images.
C				D.S.Briggs	June 10 1994
C	Added check for ERROR in main loop and explicit blank line filter.
C				D.S.Briggs	Nov 20 1994
C	Allow continuation in polygon lines.  (End line in a comma)
C				D.S.Briggs	Feb 27 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSAOMS')
C
      CHARACTER*(*)	TCURFILE, TIMGTMPL, MASK
      REAL		BACKVAL, FILLVAL, EXCLVAL
C
      CHARACTER*(SYSMXNAM)	CURFILE, IMGTMPL, CREFFILE
      CHARACTER		LINE*1000, AEXCLD*1, NAEXCLD*1
      INTEGER		IST, CURST, CUREND, NUMST, NUMEND,
     $   		I, J, K, L, NCHAR, NPTS, POLYST
      LOGICAL		EOF, DOEXCLD, LINEERR, REFCRD, MAPCRD
      REAL		CURVAL, NCURVAL, PX, PY, PXR, PYR, ROT, PYRS,
     $   		PIXEL(SYSMXDIM), TPX, TPY, TPXR, TPYR, TROT,
     $   		DX1, DX2, DY1, DY2, R, PHI, A, B, C, D, E, F
      DOUBLE PRECISION	WORLD(SYSMXDIM)
C
      INTEGER		MAXPOLY
      PARAMETER		(MAXPOLY = 500)
      REAL		XPOL(MAXPOLY), YPOL(MAXPOLY),
     $   		POLY(2,MAXPOLY), POLY1(2*MAXPOLY),
     $   		WORK(MAXPOLY,4)
      EQUIVALENCE	(WORK, POLY, POLY1)
C
      INTEGER		NAX, NAXIS(SYSMXDIM), MADD
      CHARACTER		ATYPE*1, CURTYPE*30
C
      REAL		MARKER
      PARAMETER		(MARKER = -100000.0)
      REAL		PI, D2R
      PARAMETER		(PI=3.14159265359)
      PARAMETER		(D2R=PI/180.0)
C
      INTEGER		STRLEN
      LOGICAL		DATEXIST, CRDEXIST, FILEXIST
C==================================================================
      IF (ERROR) GO TO 999
C
      CURFILE = TCURFILE
      IMGTMPL = TIMGTMPL
C
C Find the cursor file
C
      IF (CURFILE.EQ.' ') THEN
         CALL SYSGTENV ('SDECURSOR', CURFILE)
         IF (CURFILE.EQ.' ') CURFILE = 'saoimage.reg'
      END IF
C
      MESSAGE = 'Opening cursor file ' // CURFILE
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('Cursor', CURFILE, 'READ')
      IF (ERROR) GO TO 990
C
C Find the cursor reference image
C
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      CREFFILE = LINE(3:INDEX(LINE(3:),' ')+1)
      IF (FILEXIST(CREFFILE)) THEN
         CALL FILIMHGE ('CRef', CREFFILE, ' ')
      ELSE
         CALL MSGPUT ('Can not find cursor reference image','W')
         CALL DATCREAT ('CRef')
      END IF
C
C Make the output mask, if necessary
C
      IF (.NOT.DATEXIST(MASK)) THEN
         IF (DATEXIST(IMGTMPL)) THEN
            CALL DATGETAR (IMGTMPL, NAX, NAXIS, ATYPE, MADD)
            IF (ATYPE.EQ.'R') THEN
               CALL IMGCLONE (IMGTMPL, MASK)
            ELSE
               CALL ARRABS (IMGTMPL, MASK)
               CALL HEDCOPY (IMGTMPL, MASK)
            END IF
         ELSE
            IF (IMGTMPL.EQ.' ') THEN
               IMGTMPL = CREFFILE
            END IF
            CALL FILIMGGE (MASK, IMGTMPL, ' ')
            CALL DATGETAR (MASK, NAX, NAXIS, ATYPE, MADD)
            IF (ATYPE.NE.'R') THEN
               CALL DATRENAM (MASK, 'SAOTEMP')
               CALL ARRABS ('SAOTEMP', MASK)
               CALL HEDCOPY ('SAOTEMP', MASK)
               CALL DATDELET ('SAOTEMP')
            END IF
         END IF
         IF (ERROR) GO TO 990
         CALL ARRSETCO (MASK, 0.0, BACKVAL)
      END IF
C
C Sort out the coordinate systems
C
      REFCRD = CRDEXIST('CRef')
      IF (.NOT.REFCRD) CALL CRDNULL ('CRef')
C
      MAPCRD = CRDEXIST(MASK)
      IF (.NOT.MAPCRD) THEN
         IF (CRDEXIST(IMGTMPL)) THEN
            CALL MSGPUT (
     $         'Using Template Coordinate System for Mask', 'W')
            CALL CRDCOPY (IMGTMPL, MASK)
         ELSE
            CALL CRDNULL (MASK)
         END IF
      END IF
      CALL CRDTIDY ('CRef', 2)
      CALL CRDTIDY (MASK, 2)
      CALL MSGPUT ('Cursor File Coordinate System','I')
      CALL CRDLIST ('CRef')
      CALL MSGPUT ('Mask Coordinate System','I')
      CALL CRDLIST (MASK)
C
C A few addresses and basic sanity checks.
C
      CALL DATGETAR (MASK, NAX, NAXIS, ATYPE, MADD)
      IF (ATYPE.NE.'R') CALL ERRREPOR (ERRWRGTP, ROUTINE,
     $   'Image must be real')
      IF (ERROR) GO TO 990
C
C Main loop over cursors in file
C
      CALL MSGPUT ('Constructing mask from cursor file','I')
      IST = -1
 100  CONTINUE
      IF (ERROR) GO TO 999
C
C Get next non-comment line
C
      LINEERR = .TRUE.
      IF (IST.LE.0) THEN
         CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
         IF (EOF) GO TO 500
         IST = 1
      END IF
C
      IF (LINE(1:1).EQ.'#') THEN
         IST = -1
         GO TO 100
      END IF
C
      IF (LINE.EQ.' ') THEN
         IST = -1
         GO TO 100
      END IF
C
      I = INDEX(LINE(IST:),'(') + IST - 1
      J = INDEX(LINE(IST:),')') + IST - 1
      IF (I.LE.IST) GO TO 400
      IF (J-1.LE.I+1) THEN
         L = STRLEN(LINE)
         IF (LINE(L:L).NE.',') GO TO 400
         J = L
      END IF
      DOEXCLD = LINE(IST:IST) .EQ. '-'
      IF (DOEXCLD) THEN
         CURVAL = EXCLVAL
         NCURVAL = FILLVAL
         AEXCLD = '-'
         NAEXCLD = ' '
      ELSE
         CURVAL = FILLVAL
         NCURVAL = EXCLVAL
         AEXCLD = ' '
         NAEXCLD = '-'
      END IF
      CURST = IST+1
      CUREND = I-1
      NUMST = I+1
      NUMEND = J-1
      CURTYPE = LINE(CURST:CUREND)
C
      PX = 0.0
      PY = 0.0
      PXR = 0.0
      PYR = 0.0
      ROT = 0.0
      READ (LINE(NUMST:NUMEND), *, ERR=400) PX, PY
      READ (LINE(NUMST:NUMEND), *, END=110) PX, PY, PXR, PYR, ROT
 110  CONTINUE
      IF (CURTYPE.EQ.'POLYGON') THEN
C				Fill the array with stuff we'll recognize
         DO 120 I = 1, 2*MAXPOLY
            POLY1(I) = MARKER
 120     CONTINUE
C				Loop over possible continuation lines
 125     CONTINUE
C				Find a place to put the next batch of #s
         DO 126 POLYST = 1, 2*MAXPOLY
            IF (POLY1(POLYST).EQ.MARKER) GO TO 127
 126     CONTINUE
 127     CONTINUE
C				Read all we can from a line
         READ (LINE(NUMST:NUMEND), *, END=130, ERR=130)
     $      (POLY1(K), K=POLYST, 2*MAXPOLY)
 130     CONTINUE
C				Does it continue?
         L = STRLEN(LINE)
         IF (LINE(L:L).EQ.',') THEN
C				Yes, get next line
            CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
            IF (EOF) GO TO 400
            NUMST = 1
            J = INDEX(LINE,')')
            IF (J.GT.0) THEN
               NUMEND = J - 1
            ELSE
               NUMEND = STRLEN(L)
            END IF
C				Reprocess continuation
            GO TO 125
         END IF
C
      END IF
      LINEERR = .FALSE.
C
      IF (DOEXCLD) THEN
         MESSAGE = 'Excluded ' // CURTYPE
      ELSE
         MESSAGE = CURTYPE
      END IF
      WRITE (MESSAGE(STRLEN(MESSAGE)+1:), 140) PX, PY
 140  FORMAT (' at (',1PG15.7E2,',',1PG15.7E2,')')
      CALL MSGPUT (MESSAGE, 'L')
C
C Translate coordinate systems
C
      IF (CURTYPE.EQ.'POINT') THEN
         PIXEL(1) = PX
         PIXEL(2) = PY
         CALL CRDDPTOW ('CRef', PIXEL, WORLD)
         CALL CRDDWTOP (MASK, WORLD, PIXEL)
         TPX = PIXEL(1)
         TPY = PIXEL(2)
      END IF
C
      IF ((CURTYPE.EQ.'CIRCLE').OR.(CURTYPE.EQ.'ANNULUS')) THEN
         PYRS = PYR
         PYR = PXR
         ROT = 0.0
      END IF
C
      IF ((CURTYPE.EQ.'CIRCLE').OR.(CURTYPE.EQ.'ELLIPSE').OR.
     $    (CURTYPE.EQ.'ANNULUS')) THEN
         XPOL(1) = PX + PXR*COS(ROT*D2R)
         XPOL(2) = PX - PXR*COS(ROT*D2R)
         YPOL(1) = PY + PXR*SIN(ROT*D2R)
         YPOL(2) = PY - PXR*SIN(ROT*D2R)
         XPOL(3) = PX + PYR*COS((ROT+90.0)*D2R)
         XPOL(4) = PX - PYR*COS((ROT+90.0)*D2R)
         YPOL(3) = PY + PYR*SIN((ROT+90.0)*D2R)
         YPOL(4) = PY - PYR*SIN((ROT+90.0)*D2R)
         R = PXR*PYR*SQRT(2.0/(PXR**2 + PYR**2))
         XPOL(5) = PX + R*COS((ROT-45.0)*D2R)
         XPOL(6) = PX - R*COS((ROT-45.0)*D2R)
         YPOL(5) = PY + R*SIN((ROT-45.0)*D2R)
         YPOL(6) = PY - R*SIN((ROT-45.0)*D2R)
         XPOL(7) = PX + R*COS((ROT+45.0)*D2R)
         XPOL(8) = PX - R*COS((ROT+45.0)*D2R)
         YPOL(7) = PY + R*SIN((ROT+45.0)*D2R)
         YPOL(8) = PY - R*SIN((ROT+45.0)*D2R)
C
         DO 200 I = 1, 8
            PIXEL(1) = XPOL(I)
            PIXEL(2) = YPOL(I)
            CALL CRDDPTOW ('CRef', PIXEL, WORLD)
            CALL CRDDWTOP (MASK, WORLD, PIXEL)
            XPOL(I) = PIXEL(1)
            YPOL(I) = PIXEL(2)
 200     CONTINUE
C
         CALL UTLFTQDR (XPOL, YPOL, 8, A, B, C, D, E, F)
         CALL UTLQ2EL (A, B, C, D, E, F, TPX, TPY, TPXR, TPYR, TROT)
      END IF
C
      IF (CURTYPE.EQ.'BOX') THEN
         R = SQRT(PXR**2 + PYR**2) / 2.0
         PHI = ATAN(PYR/PXR)
         DX1 = R * COS(ROT*D2R + PHI)
         DX2 = R * COS(ROT*D2R - PHI)
         DY1 = R * SIN(ROT*D2R + PHI)
         DY2 = R * SIN(ROT*D2R - PHI)
C
         POLY(1,1) = PX + DX1
         POLY(2,1) = PY + DY1
         POLY(1,2) = PX + DX2
         POLY(2,2) = PY + DY2
         POLY(1,3) = PX - DX1
         POLY(2,3) = PY - DY1
         POLY(1,4) = PX - DX2
         POLY(2,4) = PY - DY2
         POLY(1,5) = MARKER
         POLY(2,5) = MARKER
      END IF
C
      IF ((CURTYPE.EQ.'POLYGON').OR.(CURTYPE.EQ.'BOX')) THEN
         DO 320 I = 1, 2*MAXPOLY
            IF (POLY1(I).EQ.MARKER) THEN
               NPTS = I - 1
               IF (CURTYPE.EQ.'POLYGON') THEN
                  WRITE (MESSAGE,1320) NPTS/2
 1320             FORMAT (I5,' points in the POLYGON list')
                  CALL MSGPUT (MESSAGE,'I')
               END IF
               GO TO 325
            END IF
 320     CONTINUE
 325     CONTINUE
         NPTS = NPTS / 2
         DO 330 I = 1, NPTS
            PIXEL(1) = POLY(1,I)
            PIXEL(2) = POLY(2,I)
            CALL CRDDPTOW ('CRef', PIXEL, WORLD)
            CALL CRDDWTOP (MASK, WORLD, PIXEL)
            XPOL(I) = PIXEL(1)
            YPOL(I) = PIXEL(2)
 330     CONTINUE
      END IF
C
C Now actually interpret the cursor
C
      IF (CURTYPE.EQ.'POINT') THEN
         CALL PIXDRPNT (MEMR(MADD), NAXIS(1), NAXIS(2), TPX, TPY,
     $      CURVAL)
      ELSE IF (CURTYPE.EQ.'CIRCLE') THEN
         CALL PIXDRFEL (MEMR(MADD), NAXIS(1), NAXIS(2),
     $      TPX, TPY, TPXR, TPYR, TROT, CURVAL)
      ELSE IF (CURTYPE.EQ.'ELLIPSE') THEN
         CALL PIXDRFEL (MEMR(MADD), NAXIS(1), NAXIS(2),
     $      TPX, TPY, TPXR, TPYR, TROT, CURVAL)
      ELSE IF (CURTYPE.EQ.'ANNULUS') THEN
         CALL PIXDRFEL (MEMR(MADD), NAXIS(1), NAXIS(2),
     $      TPX, TPY, TPXR, TPXR, TROT, CURVAL)
         CALL PIXDRFEL (MEMR(MADD), NAXIS(1), NAXIS(2),
     $      TPX, TPY, TPXR*PYRS/PX, TPYR*PYRS/PY, TROT, NCURVAL)
      ELSE IF ((CURTYPE.EQ.'POLYGON').OR.
     $         (CURTYPE.EQ.'BOX')) THEN
         CALL PIXDRFPL (MEMR(MADD), NAXIS(1), NAXIS(2), XPOL, YPOL,
     $      NPTS, CURVAL, WORK)
      ELSE
         LINEERR = .TRUE.
      END IF
C
C Final bookkeeping and error handling
C
 400  CONTINUE
      IF (LINEERR) THEN
         WRITE (MESSAGE,1400) LINE(1:STRLEN(LINE))
 1400    FORMAT ('Cannot parse line ''',A,'''')
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C
C Reprocess line if necessary (for logical ANNULUS types)
C
      I = INDEX(LINE(IST:),'!')
      IF (I.GT.0) THEN
         IST = IST + I - 1
         LINE(IST:IST) = NAEXCLD
      ELSE
         IST = -1
      END IF
C
C Loopback and exit
C
      GO TO 100
 500  CONTINUE
      CALL TXTCLOSE('Cursor')
      CALL DATDELET ('CRef')
      IF (.NOT.MAPCRD) CALL CRDDEL (MASK)
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
