C
C       National Radio Astronomy Observatorv, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)visstat.f	1.4    7/13/94
C
      SUBROUTINE SDEMAIN
C
CD Program to print/plot visibility statistics and residuals
C
C All kinds of stuff lives in here.  Probably too much.
C
C Audit trail:
C       Original version: Audit trail comments go on this line
C and successive lines
C				D.S.Briggs	Aug 22 1991
C       Retyped in from paper listing.  Beware of transcription
C errors, since not all modes have been tested since then.
C				D.S.Briggs	Nov 5 1992
C	Statistics added to plotting modes, bare bones STAT option added.
C				D.S.Briggs	May 6 1993
C	Weight plot added.
C				D.S.Briggs	Nov 27 1993
C	Better statistics added to STAT
C				D.S.Briggs	May 10 1994
C	Made GAINIMG mode work (again), tweaked weights so that visibilites
C	flagged out during selfcal don't screw us up.  (This program was
C	really munged in the OCR.)
C				D.S.Briggs	July 8 1994
C -------------------------------------------------------------------------
#include "stdinc.h"
C
C Declare name of routine
C
      CHARACTER		ROUTINE
      PARAMETER         (ROUTINE = 'VISSTAT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, VIS1FILE, OUTFILE, STOKES,
     $				MODE, IMGFORM, PLABEL, VISWT
      REAL		VCELLSIZ(3), SHIFT(3), YLIMITS(2), UVLIMITS(2)
      INTEGER		TIMR(8), IMSIZE(3), SKIP
C
      INTEGER           I, NDUMMY, NSEL, NSEL1, IMDIM, IANT(10),
     $			NANT, NUMANT, YNLOC
      REAL		TIME(2), UVSEL(2), ICELLSIZ(3), WIDTH,
     $   		YMIN, YMAX, YAVE, YSUM, YDISP, YRMS,
     $   		UMIN, UMAX
      CHARACTER*(SYSMXNAM)	SUBCLASS, CALCLASS, BUFF
      LOGICAL           ISIMG, ISPLOT, ISSTAT, ISATIMG, ISLIST
      LOGICAL           ISERR, ISGAIN, ISVIS, ISCOV, ISWTERR,
     $			ISAMP, ISPHASE, USEVIS1
C
      REAL		D2R, PI
      PARAMETER		(PI = 3.14159265358)
      PARAMETER		(D2R = PI/180.0)
C
      INTEGER		FFTPWR2, STRLEN, DATFGETI, ARRNPIX, ARRNPOS
      LOGICAL           DATEXIST
      REAL              DATFGETR
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C===========================================================================
      CALL MSGWELCO ('I form statistics, images and plots of uv data')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Inputs
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Vis1', VIS1FILE, 1, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETC ('ImgForm', IMGFORM, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', VCELLSIZ, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETI ('Antennas', IANT, 10, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Skip', SKIP, 1, NDUMMY)
      CALL USRGETR ('Ylimits', YLIMITS, 2, NDUMMY)
      CALL USRGETC ('Plotlabel', PLABEL, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
C Massage the inputs a little
C
      BUFF = MODE
      CALL STRUC (BUFF, MODE)
      BUFF = IMGFORM
      CALL STRUC (BUFF, IMGFORM)
      BUFF = STOKES
      CALL STRUC (BUFF, STOKES)
      IF (STOKES.EQ.' ') STOKES = 'I'
C
C Decode the mode into more easily handled pieces
C
      ISERR = (MODE.EQ.'ERRIMG') .OR. (MODE.EQ.'AEPLOT')
     $    .OR.(MODE.EQ.'PEPLOT')
      ISWTERR = (MODE.EQ.'WEPLOT')
      ISGAIN = (MODE.EQ.'GAINIMG').OR.(MODE.EQ.'GAPLOT')
     $     .OR.(MODE.EQ.'GPPLOT').OR.(MODE.EQ.'GAINLIST')
     $     .OR.(MODE.EQ.'ATGIMG')
      ISVIS = (MODE.EQ.'VISIMG')
      ISCOV = (MODE.EQ.'COVIMG')
      ISAMP = (MODE.EQ.'AEPLOT').OR.(MODE.EQ.'GAPLOT')
      ISPHASE = (MODE.EQ.'PEPLOT').OR.(MODE.EQ.'GPPLOT')
      ISIMG = (MODE.EQ.'VISIMG').OR.(MODE.EQ.'GAINIMG')
     $    .OR.(MODE.EQ.'ERRIMG').OR.(MODE.EQ.'COVIMG')
      ISPLOT = (MODE.EQ.'GAPLOT').OR.(MODE.EQ.'GPPLOT')
     $     .OR.(MODE.EQ.'AEPLOT').OR.(MODE.EQ.'PEPLOT')
     $     .OR.(MODE.EQ.'WEPLOT')
      ISATIMG = (MODE.EQ.'ATGIMG')
      ISLIST = (MODE.EQ.'GAINLIST')
      ISSTAT = (MODE.EQ.'STAT') 
C
C Make sure it was *something* we recognize
C
      IF (.NOT.(ISIMG.OR.ISPLOT.OR.ISSTAT.OR.ISLIST.OR.ISATIMG)) THEN
         MESSAGE = 'Mode ''' // MODE(1:STRLEN(MODE)) //
     $      ''' is not recognized'
         CALL ERRREPOR (ERRBADID, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Get visibilities, as required.
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', ' ')
      IF (ERROR) GO TO 999
C
      USEVIS1 = ISERR
      IF (ISGAIN) THEN
         IF ((VIS1FILE.EQ.' ').OR.ISATIMG.OR.ISLIST) THEN
            USEVIS1 = .FALSE.
            CALL MSGPUT ('Will use precalculated gains', 'I')
         ELSE
            CALL MSGPUT ('Will calculate gains from model visibilities'
     $         , 'I')
            USEVIS1 = .TRUE.
         END IF
      END IF
C
      IF (USEVIS1) THEN
         CALL VISGET ('Vis1', VIS1FILE, STOKES(1:1), '*', ' ')
         IF (ERROR) GO TO 999
      END IF
C
C 'closure' currently overwrites the original visibility data with the
C calibrated data.  Thus, the appropriate place to find the gain information
C is the original subclass.  This may change.

C
      SUBCLASS = 'OBS/'//STOKES(1:1)
      CALCLASS = 'OBS/'//STOKES(1:1)
C
C Select the appropriate chunk
C
      IF (DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         UVSEL(1) = UVLIMITS(1)
         UVSEL(2) = UVLIMITS(2)
         IF ((UVSEL(1).EQ.0.0).AND.(UVSEL(2).EQ.0.0)) UVSEL(2) = 1.E15
         CALL VISSEL ('Vis', SUBCLASS, TIME, UVSEL, NSEL)
         IF (ERROR) GO TO 999
         IF (USEVIS1) THEN
            CALL VISSEL ('Vis1', SUBCLASS, TIME, UVSEL, NSEL1)
            IF (NSEL.NE.NSEL1) THEN
               IF (ARRNPIX(STRM3('Vis',SUBCLASS,'VIS')).EQ.
     $             ARRNPIX(STRM3('Vis1',SUBCLASS,'VIS'))) THEN
                  CALL ARRSEL(STRM3('Vis',SUBCLASS,'WT'),
     $                        STRM3('Vis1',SUBCLASS,'WT'),
     $                        STRM3('Vis',SUBCLASS,'WT'))
                  NSEL = ARRNPOS(STRM3('Vis',SUBCLASS,'WT'))
               ELSE
                  MESSAGE = 'Visibility files do not correspond with'
     $            // ' each other'
                  CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
                  GO TO 999
               END IF
            END IF
         END IF
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
C Calculate basic min/max statistics for later use
C
      CALL VISSTAT0 ('Vis', SUBCLASS, 'Stats')
      NUMANT = DATFGETI ('Stats','MAXANT')
      WRITE (MESSAGE, 1005) NUMANT
 1005 FORMAT (I2,' antennas found')
      CALL MSGPUT (MESSAGE, 'I')
C
C Decode Antennas input  (maybe baselines & triangles later?)
C
      NANT = 0
      DO 10 I = 1, 10
         IF (IANT(I).NE.0) THEN
            NANT = NANT + 1
            IANT(NANT) = IANT(I)
         END IF
 10   CONTINUE
C
C If no specific input, default to number of antennas found in the uv data
C
      IF (NANT.EQ.0) THEN
         NANT = 1
         IANT(1) = -NUMANT
      END IF
C
C Code to deal with imaging output begins here.  The remainder of the task
C  is essentially a single large 'if' clause.
C
      IF (ISIMG) THEN
C
C Force the image size to a power of two.  If we don't do it here, FFTCONJ
C will do it for use, and mess up the cellsize calculations.  A modified
C version of FFTCONJA could make this unnecessary.
C
         IMDIM = 1
         DO 100 I = 1, 3
            IF (IMSIZE(I).GT.1) THEN
               NDUMMY = FFTPWR2 (IMSIZE(I))
               IF (NDUMMY.GT.IMSIZE(I)) THEN
 1010               FORMAT ('Image size increased to',I4,
     $                      ' on axis ',I1)
                  WRITE (MESSAGE, 1010) NDUMMY, I
                  CALL MSGPUT (MESSAGE, 'W')
                  IMSIZE(I) = NDUMMY
               END IF
               IMDIM = I
            END IF
 100     CONTINUE
C
C Make an image header
C
         DO 200 I = 1, IMDIM
            IF (VCELLSIZ(I).EQ.0)
     $         VCELLSIZ(I) = DATFGETR('Stats','MAXUV') / 1000.0
     $         * 2.20 / IMSIZE(I)
            WIDTH = 1000.0 * VCELLSIZ(I) * IMSIZE(I)
            ICELLSIZ(I) = 3600.0 / (D2R * WIDTH)
 200     CONTINUE
         CALL IMGMAKE ('Vis/'//SUBCLASS, ICELLSIZ, IMSIZE, SHIFT,
     $      'R', 'TempImg')
         IF (ERROR) GO TO 999
C
C Conjugate it to get what we really want
C
         CALL FFTCONJA ('TempImg', 'VisImg', NDUMMY, IMDIM)
         IF (ERROR) GO TO 999
         CALL CRDLIST ('VisImg')
C
C Special case for recorded gains (ick)
C
         IF (ISGAIN.AND.(.NOT.USEVIS1)) THEN
            CALL ARRSETCO(STRM3('Vis',SUBCLASS,'VIS'), 0.0, 1.0)
            CALL ARRSETCO(STRM3('Vis',SUBCLASS,'WT'), 0.0, 1.0)
            CALL DATPUTC(STRM2('Vis',SUBCLASS),'GAITYPE','BOXCAR',1)
            CALL GAIAPPLY('Vis', SUBCLASS, 'Vis', CALCLASS,
     $                    'Vis', SUBCLASS)
            IF (ERROR) GO TO 999
         END IF
C
C Grid the vis datat
C
         CALL DATPUTC ('VisImg', 'CFTYPE', 'BOX', 1)
         CALL IMGCLONE ('VisImg', 'Weights')
         CALL VISGRID ('Vis', SUBCLASS, 'VisImg', .FALSE.)
         CALL VISGRID ('Vis', SUBCLASS, 'Weights', .TRUE.)
C
         CALL ARRDIV ('VisImg','Weights','VisImg')
         IF (ERROR) GO TO 999
C
         IF (USEVIS1) THEN
            CALL IMGCLONE ('VisImg', 'VisImg1')
            CALL VISGRID ('Vis1', SUBCLASS, 'VisImg1', .FALSE.)
            CALL VISGRID ('Vis1', SUBCLASS, 'Weights', .TRUE.)
            CALL ARRDIV ('VisImg1','Weights','VisImg1')
            IF (ERROR) GO TO 999
         END IF
C
C Final calculations
C
         IF (ISVIS) THEN
            CALL MSGPUT ('Output is gridded visibility','I')
         ELSE IF (ISCOV) THEN
            CALL MSGPUT ('Output is visibility coverage','I')
            CALL ARRDIV ('VisImg','VisImg','VisImg')
            IMGFORM = 'R'
         ELSE IF (ISERR) THEN
            CALL MSGPUT ('Output is visibility errors (V/V1 - 1)','I')
            CALL ARRSUBTR ('VisImg', 'VisImg1', 'VisImg')
            CALL ARRDIV ('VisImg', 'VisImg1', 'VisImg')
         ELSE IF (ISGAIN) THEN
            IF (USEVIS1) THEN
               CALL MSGPUT ('Output is visibility gains (V/V1)', 'I')
               CALL ARRDIV ('VisImg','VisImg1','VisImg')
            ELSE
               CALL MSGPUT ('Output is recorded gains', 'I')
            END IF
         END IF
C
C Mess with the history
C
         CALL HISINPUT ('VisImg')
         CALL HISPUT ('VisImg', '***** START OF VISFILE HISTORY *****')
         CALL HISCOPY ('Vis','VisImg')
         CALL HISPUT ('VisImg', '***** END OF VISFILE HISTORY *****')
         IF (USEVIS1) THEN
            CALL HISPUT ('VisImg',
     $         '***** START OF VISFILE HISTORY *****')
            CALL HISCOPY ('Vis1','VisImg')
            CALL HISPUT ('VisImg',
     $         '***** END OF VISFILE HISTORY *****')
         END IF
C
C Write result
C
         IF (OUTFILE.EQ.' ') CALL FILSYSRT (VISFILE, OUTFILE)
         CALL FILIMGXP('VisImg', OUTFILE, IMGFORM, ' ')
C
C Antenna time images
C
      ELSE IF (ISATIMG) THEN
C
C Make the image header and fill it with XGains
C
         CALL IMGATMAK ('Vis', SUBCLASS, 'Stats', 'X', 'XGain', 'ATImg')
C
C Mess with the history
C
         CALL HISINPUT ('ATImg')
         CALL HISPUT ('ATImg', '***** START OF VISFILE HISTORY *****')
         CALL HISCOPY ('Vis','ATImg')
         CALL HISPUT ('ATImg', '***** END OF VISFILE HISTORY *****')
C
C Write it out
C
         CALL FILIMGXP('ATImg', OUTFILE, IMGFORM, ' ')

C This code deals with plots of the various kinds.

      ELSE IF (ISPLOT) THEN

C Make an options directory, and fili it with common options

         CALL DATCREAT ('Options')
         CALL DATPUTC ('Options', 'DEV', OUTFILE, 1)
         CALL DATPUTI ('Options', 'PTSKIP', SKIP, 1)
         CALL DATPUTC ('Options', 'XLABEL', 'uv distance', 1)
         CALL DATPUTL ('Options', 'SQUARE', .FALSE., 1)
         CALL DATPUTR ('Options', 'XLIMITS', UVLIMITS, 2)
         CALL DATPUTR ('Options', 'YLIMITS', YLIMITS, 2)
         CALL DATPUTI ('Options', 'PTTYPE', 21, 1)
C
C Dig out the uvradius
C
         VISWT = STRM3('Vis',SUBCLASS,'WT')
         CALL ARRSCOPY ('Vis/UU', VISWT, 'uu')
         CALL ARRSCOPY ('Vis/VV', VISWT, 'vv')
         CALL ARRMULT ('uu', 'uu', 'u2')
         CALL ARRMULT ('vv', 'vv', 'v2')
         CALL ARRADD ('u2', 'v2', 'uvRadius')
         CALL ARRPOWER ('uvRadius', 0.5, 0.0, 'uvRadius')
C
C Select out the visibility data into appropriate arrays.
C
         CALL ARRSCOPY (STRM3('Vis',SUBCLASS,'VIS'), VISWT, 'v')
         IF (USEVIS1)
     $	    CALL ARRSCOPY (STRM3('Vis1',SUBCLASS,'VIS'), VISWT, 'v1')
C
C Plot specific processing
C
         IF (ISGAIN) THEN
            CALL ARRDIV ('v','v1','XGain')
            IF (ISAMP) THEN
               IF (PLABEL.EQ.' ') PLABEL = 'Gain Amplitude Plot'
               CALL DATPUTC('Options', 'YLABEL', 'Gain Amplitude', 1)
               CALL ARRX2AP ('XGain', 'Y', 'DUMMY')
            ELSE IF (ISPHASE) THEN
               IF (PLABEL.EQ.' ') PLABEL = 'Gain Phase Plot'
               CALL DATPUTC('Options', 'YLABEL', 'Gain Phase', 1)
               CALL ARRX2AP ('XGain', 'DUMMY', 'Y')
            END IF
         ELSE IF (ISERR) THEN
            CALL ARRSUBTR ('v', 'v1', 'v')
            CALL ARRDIV ('v','v1','XErr')
            IF (ISAMP) THEN
               IF (PLABEL.EQ.' ') PLABEL = 'Error Amplitude Plot'
               CALL DATPUTC ('Options', 'YLABEL', 'Error Amplitude', 1)
               CALL ARRX2AP ('XErr', 'Y', 'DUMMY')
            ELSE IF (ISPHASE) THEN
               IF (PLABEL.EQ.' ') PLABEL = 'Error Phase Plot'
               CALL DATPUTC ('Options', 'YLABEL', 'Error Phase', 1)
               CALL ARRX2AP ('XErr', 'DUMMY', 'Y')
            END IF
         ELSE IF (ISWTERR) THEN
            CALL ARRSCOPY (STRM3('Vis',SUBCLASS,'WT'), VISWT, 'w')
            CALL ARRSCOPY (STRM3('Vis1',SUBCLASS,'WT'), VISWT, 'w1')
            CALL ARRSUBTR ('w', 'w1', 'w')
            CALL ARRDIV ('w', 'w1', 'Y')
            IF (PLABEL.EQ.' ') PLABEL = 'Weight Error Plot'
            CALL DATPUTC ('Options', 'YLABEL', 'Error Amplitude', 1)
         END IF
         IF (ERROR) GO TO 999
C
C Finally make the plot
C
         CALL DATPUTC ('Options', 'PLTLABEL', PLABEL, 1)
         IF (OUTFILE.NE.' ') THEN
            CALL ARRCOPY ('Y', 'UnitWeight')
            CALL ARRSETCO ('UnitWeight', 0.0, 1.0)
            CALL ARR2DPLT ('uvRadius', 'Y', 'UnitWeight', 'Options')
         END IF
         IF (ERROR) GO TO 999
C
C Print statistics of plot
C
         CALL ARRSTAT ('Y', ' ')
         YMIN = DATFGETR ('Y', 'ARRMIN')
         YMAX = DATFGETR ('Y', 'ARRMAX')
         YAVE = DATFGETR ('Y', 'ARRAVE')
         YSUM = DATFGETR ('Y', 'ARRSUM')
         YRMS = DATFGETR ('Y', 'ARRRMS')
         YDISP = DATFGETR ('Y', 'ARRDISP')
         YNLOC = DATFGETI ('Y', 'ARRNLOC')
         CALL ARRSTAT ('uvRadius', ' ')
         UMIN = DATFGETR ('uvRadius', 'ARRMIN')
         UMAX = DATFGETR ('uvRadius', 'ARRMAX')
C
         CALL MSGPUT (' ','I')
         WRITE (MESSAGE, 1500) YAVE, ARRNPIX('Y')
 1500    FORMAT ('Average of ',1PE12.4, ' over',I8, ' samples')
         CALL MSGPUT (MESSAGE,'I')
         WRITE (MESSAGE, 1510) YSUM, YNLOC
 1510    FORMAT ('Sum = ',1PE12.4, ',',I8,' non-zero samples')
         CALL MSGPUT (MESSAGE,'I')
         WRITE (MESSAGE, 1520) YMIN, YMAX
 1520    FORMAT ('Min = ',1PE12.4,'  Max = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
         WRITE (MESSAGE, 1530) YRMS, YDISP
 1530    FORMAT ('RMS = ',1PE12.4,'  Disp = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
         WRITE (MESSAGE, 1530) UMIN, UMAX
 1540    FORMAT ('UVmin = ',1PE12.4,'  UVmax = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
C Processing for text listing
C
      ELSE IF (ISLIST) THEN
         CALL MSGPUT ('Writing textual gain listing','I')
         IF (OUTFILE.EQ.' ') OUTFILE = 'temp.out'
         CALL TXTOPEN ('Listing', OUTFILE, 'WRITE')
C
         MESSAGE = 'Filename = "' // VISFILE(1:STRLEN(VISFILE)) // '"'
         CALL TXTWRITE ('Listing', MESSAGE)
         CALL TXTWRITE ('Listing', ' ')
C
         CALL GAILIST ('Listing', 'Vis', CALCLASS, TIME, IANT, NANT)
         CALL TXTCLOSE ('Listing')
      ELSE IF (ISSTAT) THEN
         CALL CRDLISRD ('Vis/'//SUBCLASS)
         CALL CRDLIST ('Vis/'//SUBCLASS)
         CALL VISSTAT0 ('Vis', SUBCLASS, 'Stats')
C
         WRITE (MESSAGE, 2000) DATFGETI('Stats','MINANT'),
     $      DATFGETI('Stats','MAXANT')
 2000    FORMAT ('Minimum antenna number found = ',I3,'  Maximum = ',I3)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2010) DATFGETR('Stats','MINUV'),
     $      DATFGETR('Stats','MAXUV')
 2010    FORMAT ('Minimum uv radius = ',E12.4,'  Maximum = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2020) DATFGETR('Stats','MINUVW'),
     $      DATFGETR('Stats','MAXUVW')
 2020    FORMAT ('Minimum uvw radius = ',E12.4,'  Maximum = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2030) DATFGETR('Stats','MINUU'),
     $      DATFGETR('Stats','MAXUU')
 2030    FORMAT ('Umin = ',E12.4,'  Umax = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2040) DATFGETR('Stats','MINVV'),
     $      DATFGETR('Stats','MAXVV')
 2040    FORMAT ('Vmin = ',E12.4,'  Vmax = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2050) DATFGETR('Stats','MINWW'),
     $      DATFGETR('Stats','MAXWW')
 2050    FORMAT ('Wmin = ',E12.4,'  Wmax = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2060) DATFGETR('Stats','MINTIME'),
     $      DATFGETR('Stats','MAXTIME')
 2060    FORMAT ('Min time = ',E12.5,'  Max time = ',E12.5)
         CALL MSGPUT (MESSAGE,'I')
C
         WRITE (MESSAGE, 2070) DATFGETR('Stats','MINVIS'),
     $      DATFGETR('Stats','MAXVIS')
 2070    FORMAT ('Min VisAmp = ',E12.4,'  Max VisAmp = ',E12.4)
         CALL MSGPUT (MESSAGE,'I')
C
      END IF
C
 999  CONTINUE
      END
