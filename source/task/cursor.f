C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)cursor.f	1.6    24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to interpret SAOimage cursors
C
C Audit trail:
C	Original version: Very few cursors recognized, and doesn't do
C        rotations or axis units.
C				D.S.Briggs	Sept 24 1991
C       Pixel positions done in double precision, and deprojected
C       properly for standard axis types.  RA & Dec printed in
C	reasonable formats.
C				D.S.Briggs	May 1993
C	RA & Dec offsets from reference pixel printed in arcseconds
C				D.S.Briggs	17 June 1993
C	Print cursor filename being interpreted.  Fix bug in RA offset
C	display.
C				D.S.Briggs	9 July 1993
C	Print out image value at fiducial location of cursors
C				D.S.Briggs	Jan 26 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CURSOR')
C
      CHARACTER*(SYSMXNAM)	FILENAME
      CHARACTER		LINE*500, NUMDATA*132, CURTYPE*20
      INTEGER		I, J, NCHAR
      LOGICAL		EOF, ISSQUARE

      DOUBLE PRECISION	PX, PY, PXR, PYR, ROT,
     $   		X, Y, XR, YR, X1, Y1, X2, Y2
      REAL		RRA(3), RDEC(3)
      CHARACTER*15	ARA, ADEC
      CHARACTER*(SYSMXNAM)	BUNIT
      INTEGER		I, ADD, NDUMMY
C
      REAL		PIX(SYSMXDIM)
      DOUBLE PRECISION	WORLD(SYSMXDIM)
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
C
      DOUBLE PRECISION	PI, D2R
C
      LOGICAL		DATEXIST
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I interpret SAOimage cursors')
C
      PI = 4.D0 * ATAN (1.D0)
      D2R = PI / 180.D0
C
      CALL SYSGTENV ('SDECURSOR', FILENAME)
      IF (FILENAME.EQ.' ') FILENAME = 'saoimage.reg'
C
      MESSAGE = 'Interpreting cursor file ' // FILENAME
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('Cursor', FILENAME, 'READ')
      IF (ERROR) GO TO 999
C
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      FILENAME = LINE(3:)
      CALL FILIMGGE ('Image', FILENAME, ' ')
      IF (ERROR) GO TO 999
C
      BUNIT = ' '
      IF (DATEXIST('Image/BUNIT')) THEN
         CALL DATGETC ('Image', 'BUNIT', BUNIT, 1, NDUMMY)
      END IF
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, ADD)
      CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      ISSQUARE = DELT(1) .EQ. DELT(2)
C
      WRITE (MESSAGE, 1000) TYPE(1), TYPE(2)
 1000 FORMAT ('Axes are ', A, ' and ', A)
      CALL MSGPUT (MESSAGE, 'I')
C
C This should track the list in CRDPTOW
C
      IF (((TYPE(1).EQ.'RA---SIN').AND.(TYPE(2).EQ.'DEC--SIN')) .OR.
     $    ((TYPE(1).EQ.'RA---ARC').AND.(TYPE(2).EQ.'DEC--ARC'))) THEN
         CALL MSGPUT ('Will be deprojected to RA & Dec','I')
      END IF
      CALL MSGPUT (' ','I')
C
C Main loop over cursors in file
C
 100  CONTINUE
C
C Get next non-comment line
C
      CALL TXTREAD ('Cursor', LINE, NCHAR, EOF)
      IF (EOF) GO TO 999
      IF (LINE(1:1).EQ.'#') GO TO 100
C
      I = INDEX(LINE,'(')
      J = INDEX(LINE,')')
      IF ((I.LE.1).OR.(J-1.LE.I+1)) GO TO 900
      CURTYPE = LINE(2:I-1)
      NUMDATA = LINE(I+1:J-1)
C
      PX = 0.0D0
      PY = 0.0D0
      PXR = 0.0D0
      PYR = 0.0D0
      ROT = 0.0D0
      READ (NUMDATA, *, ERR=900) PX, PY
      READ (NUMDATA, *, END=110) PX, PY, PXR, PYR, ROT
 110  CONTINUE
C
      IF ((ROT.NE.0.0D0).AND.(CURTYPE.NE.'POLYGON')) THEN
         CALL MSGPUT ('Cannot deal with rotations.','W')
         CALL MSGPUT ('Line = '//LINE, 'W')
         GO TO 100
      END IF
C
C We do this via CRDDPTOW, so that inverse projections are done properly.
C
      PIX(1) = PX
      PIX(2) = PY
      CALL CRDDPTOW ('Image', PIX, WORLD)
      X = WORLD(1)
      Y = WORLD(2)
C
      XR = PXR * ABS(DELT(1))
      YR = PYR * ABS(DELT(2))
C
      IF (LINE(1:1).EQ.'-') THEN
         MESSAGE = 'Excluded ' // CURTYPE
      ELSE
         MESSAGE = CURTYPE
      END IF
      IF ((TYPE(1)(1:2).EQ.'RA').AND.(TYPE(2)(1:3).EQ.'DEC')) THEN
         CALL CRDD2RD (X, Y, RRA, RDEC, ARA, ADEC)
         WRITE (MESSAGE(STRLEN(MESSAGE)+1:), 205)
     $      ARA(1:STRLEN(ARA)), ADEC(1:STRLEN(ADEC))
 205     FORMAT (' at ',A,'  ',A)
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 208) (X-RVAL(1))*3600.0*COS(RVAL(2)*D2R),
     $      (Y-RVAL(2))*3600.0
 208     FORMAT (' delta = (',1PG17.9E2,',',1PG17.9E2,') arcsec')
         CALL MSGPUT (MESSAGE, 'L')
      ELSE
         WRITE (MESSAGE(STRLEN(MESSAGE)+1:), 210) X, Y
 210     FORMAT (' at (',1PG17.9E2,',',1PG17.9E2,')')
         CALL MSGPUT (MESSAGE, 'L')
      END IF
C
      IF ((NINT(PX).GE.1).AND.(NINT(PX).LE.NAXIS(1)).AND.
     $    (NINT(PY).GE.1).AND.(NINT(PY).LE.NAXIS(2))) THEN
         IF (ATYPE.EQ.'R') THEN
 1310       FORMAT (' value =',1PE12.4,' ',A)
            WRITE (MESSAGE, 1310) MEMR(ADD+(NINT(PY)-1)*NAXIS(1)+
     $         (NINT(PX)-1)), BUNIT(1:MAX(1,STRLEN(BUNIT)))
            CALL MSGPUT (MESSAGE,'L')
         ELSE IF (ATYPE.EQ.'D') THEN
 1320       FORMAT (' value =',1PE19.9,' ',A)
            WRITE (MESSAGE, 1310) MEMD(ADD+(NINT(PY)-1)*NAXIS(1)+
     $         (NINT(PX)-1)), BUNIT(1:MAX(1,STRLEN(BUNIT)))
            CALL MSGPUT (MESSAGE,'L')
         ELSE IF (ATYPE.EQ.'I') THEN
 1330       FORMAT (' value = ',I10,' ',A)
            WRITE (MESSAGE, 1310) MEMI(ADD+(NINT(PY)-1)*NAXIS(1)+
     $         (NINT(PX)-1)), BUNIT(1:MAX(1,STRLEN(BUNIT)))
            CALL MSGPUT (MESSAGE,'L')
         ELSE IF (ATYPE.EQ.'X') THEN
 1340       FORMAT (' value = (',1PE12.4,',',E12.4,') ',A)
            WRITE (MESSAGE, 1310) MEMX(ADD+(NINT(PY)-1)*NAXIS(1)+
     $         (NINT(PX)-1)), BUNIT(1:MAX(1,STRLEN(BUNIT)))
            CALL MSGPUT (MESSAGE,'L')
         END IF
      ELSE
         CALL MSGPUT (' value = *off image*','L')
      END IF
C
      IF (CURTYPE.EQ.'CIRCLE') THEN
         IF (ISSQUARE) THEN
            WRITE (MESSAGE, 320) XR
 320        FORMAT (' Radius is ',1PG15.7E2)
         ELSE
            YR = PXR * ABS(DELT(2))
            WRITE (MESSAGE, 330) XR, YR
 330        FORMAT (' X Radius is', 1PG15.7E2, '    Y Radius is',
     $         1PG15.7E2)
         END IF
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 340) PI * XR * YR
 340     FORMAT (' Area is', 1PG15.7E2)
         CALL MSGPUT (MESSAGE, 'L')
      ELSE IF (CURTYPE.EQ.'ELLIPSE') THEN
         WRITE (MESSAGE, 330) XR, YR
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 340) PI * XR * YR
         CALL MSGPUT (MESSAGE, 'L')
      ELSE IF (CURTYPE.EQ.'BOX') THEN
         X1 = X - XR
         X2 = X + XR
         Y1 = Y - YR
         Y2 = Y + YR
         WRITE (MESSAGE, 410) X1, X2
 410     FORMAT (' From',1PG15.7E2, ' to ',1PG15.7E2,' in X')
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 420) Y1, Y2
 420     FORMAT (' From',1PG15.7E2, ' to ',1PG15.7E2,' in Y')
         CALL MSGPUT (MESSAGE, 'L')
         WRITE (MESSAGE, 430) XR * YR
 430     FORMAT (' Area is', 1PG15.7E2)
         CALL MSGPUT (MESSAGE, 'L')
      END IF
C
C Successful loopback
C
      GO TO 100
C
C Error loop back
C
 900  WRITE (MESSAGE,910) LINE(1:STRLEN(LINE))
 910  FORMAT ('Cannot parse line ''',A,'''')
      CALL MSGPUT (MESSAGE, 'W')
      GO TO 100
C
 999  CONTINUE
      END
