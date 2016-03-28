C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pslplt.f	1.5    2/21/95
C
      SUBROUTINE SDEMAIN
C
CD Program to plot PSF slices
C
C Audit trail:
C	New task
C				D.S.Briggs	July 21 1994
C	Whoops!  Never connected up the 'Left' option.  Added annotation.
C				D.S.Briggs	Nov 15 1994
C	Added a position angle override and absolute radius specs
C				D.S.Briggs	Jan 25 1995
C	Allow ymin, ymax
C				D.S.Briggs	Feb 1 1995
C	Text file output
C				D.S.Briggs	Feb 21 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PSLPLT')
C
      CHARACTER*(SYSMXNAM)	PSFFILE, FITALG, XLABEL, YLABEL,
     $   		ANNOT, DEVICE, TEXTFILE
      REAL		RLO, RHI, BEAM(4)
      INTEGER		LWIDTH, NPTS
      LOGICAL		FGAUS, VLBI, LEFT, ABSPA, ABSRAD
C
      REAL		D2R, DELT(SYSMXDIM), RPIX(SYSMXDIM),
     $   		BMAJ, BMIN, BPA, SPA,
     $   		ZSTART, X1, X2, Y1, Y2, CHEIGHT, T,
     $			XMIN, XMAX, YMIN, YMAX, SX, R
      INTEGER		NDUMMY, XADD1, YADD1, XADD2, YADD2, PADD, GADD,
     $   		FGADD1, FGADD2, NAX, NAXIS(SYSMXDIM), ISTAT, I
      CHARACTER*1	ATYPE
C
      INTEGER		PGBEGIN, DATADD, STRLEN
C==================================================================
      CALL MSGWELCO ('I plot PSF slices')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETR ('Rlo', RLO, 1, NDUMMY)
      CALL USRGETR ('Rhi', RHI, 1, NDUMMY)
      CALL USRGETL ('AbsRadius', ABSRAD, 1, NDUMMY)
      CALL USRGETL ('AbsPA', ABSPA, 1, NDUMMY)
      CALL USRGETR ('Ymin', YMIN, 1, NDUMMY)
      CALL USRGETR ('Ymax', YMAX, 1, NDUMMY)
      CALL USRGETR ('SlicePA', SPA, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('Xlabel', XLABEL, 1, NDUMMY)
      CALL USRGETC ('Ylabel', YLABEL, 1, NDUMMY)
      CALL USRGETL ('Left', LEFT, 1, NDUMMY)
      CALL USRGETC ('Annotate', ANNOT, 1, NDUMMY)
      CALL USRGETI ('Lwidth', LWIDTH, 1, NDUMMY)
      CALL USRGETR ('CHeight', CHEIGHT, 1, NDUMMY)
      CALL USRGETI ('Npts', NPTS, 1, NDUMMY)
      CALL USRGETL ('FGaus', FGAUS, 1, NDUMMY)
      CALL USRGETL ('VLBI', VLBI, 1, NDUMMY)
      CALL USRGETC ('Device', DEVICE, 1, NDUMMY)
      CALL USRGETC ('Text', TEXTFILE, 1, NDUMMY)
C
      IF (XLABEL.EQ.'*') THEN
         IF (VLBI) THEN
            XLABEL = 'Radius (mas)'
         ELSE
            XLABEL = 'Radius (asec)'
         END IF
      END IF
      IF (YLABEL.EQ.'*') YLABEL = 'PSF'
C
      CALL FILIMGGE ('PSF', PSFFILE, ' ')
      CALL FILBEMGE ('PSF', BEAM, FITALG, 'BEAM')
C
      BMAJ = BEAM(1) / 3600.0
      BMIN = BEAM(2) / 3600.0
      BPA = BEAM(3)
C
      IF (ABSPA) THEN
 1000    FORMAT ('Using absolute slice angle of ',F7.2,' degrees')
         WRITE (MESSAGE, 1000) SPA
      ELSE
         SPA = BPA
 1010    FORMAT ('Using PSF slice angle of ',F7.2,' degrees')
         WRITE (MESSAGE,1010) SPA
      END IF
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (VLBI) THEN
         SX = 3600.0 * 1000.0
      ELSE
         SX = 3600.0
      END IF
C
      CALL DATGETR ('PSF', 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      CALL DATGETR ('PSF', 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL DATMAKAR ('Zpts1', 1, NPTS, 'R', XADD1)
      CALL DATMAKAR ('Ppts1', 1, NPTS, 'R', YADD1)
      CALL DATMAKAR ('Zpts2', 1, NPTS, 'R', XADD2)
      CALL DATMAKAR ('Ppts2', 1, NPTS, 'R', YADD2)
      CALL DATGETAR ('PSF', NAX, NAXIS, ATYPE, PADD)
      IF (ABSRAD) THEN
         IF (VLBI) THEN
            RLO = RLO / (3600.0 * 1000.0)
            RHI = RHI / (3600.0 * 1000.0)
         ELSE
            RLO = RLO / 3600.0
            RHI = RHI / 3600.0
         END IF
      ELSE
         RLO = SQRT(BMAJ*BMIN) * RLO
         RHI = SQRT(BMAJ*BMIN) * RHI
      END IF
      D2R = ATAN(1.0) * 4. / 180.
      IF (ERROR) GO TO 999
C
      IF (FGAUS) THEN
         CALL IMGCLONE ('PSF', 'Gauss')
         CALL DATMAKAR ('FGpts1', 1, NPTS, 'R', FGADD1)
         CALL DATMAKAR ('FGpts2', 1, NPTS, 'R', FGADD2)
         GADD = DATADD ('Gauss')
         R = (BMAJ * BMIN * ATAN(1.0) / LOG(2.0)) /
     $      ABS(DELT(1) * DELT(2))
         CALL MODIMG2D (1, R, 0.0, 0.0, BEAM(1), BEAM(2), BPA,
     $      'GAUS', MEMR(GADD), NAXIS(1), NAXIS(2), RPIX(1),
     $      DELT(1)*3600.0, RPIX(2), DELT(2)*3600.0)
      END IF
C
C This will break if xdelt != ydelt
C
      X1 = RPIX(1) - SIN(SPA*D2R)*RLO/ABS(DELT(1))
      Y1 = RPIX(2) + COS(SPA*D2R)*RLO/ABS(DELT(2))
      X2 = RPIX(1) - SIN(SPA*D2R)*RHI/ABS(DELT(1))
      Y2 = RPIX(2) + COS(SPA*D2R)*RHI/ABS(DELT(2))
      ZSTART = RLO * SX
      CALL PIXRASL2 (MEMR(PADD), NAXIS(1), NAXIS(2), X1, X2, Y1, Y2,
     $   NPTS, ZSTART, DELT(1)*SX, DELT(2)*SX, 4,
     $   MEMR(XADD1), MEMR(YADD1))
      IF (FGAUS) THEN
         CALL PIXRASL2 (MEMR(GADD), NAXIS(1), NAXIS(2), X1, X2,
     $      Y1, Y2, NPTS, ZSTART, DELT(1)*SX, DELT(2)*SX, 4,
     $      MEMR(XADD1), MEMR(FGADD1))
      END IF
C
      X1 = RPIX(1) - SIN((SPA+90.0)*D2R)*RLO/ABS(DELT(1))
      Y1 = RPIX(2) + COS((SPA+90.0)*D2R)*RLO/ABS(DELT(2))
      X2 = RPIX(1) - SIN((SPA+90.0)*D2R)*RHI/ABS(DELT(1))
      Y2 = RPIX(2) + COS((SPA+90.0)*D2R)*RHI/ABS(DELT(2))
      ZSTART = RLO * SX
      CALL PIXRASL2 (MEMR(PADD), NAXIS(1), NAXIS(2), X1, X2, Y1, Y2,
     $   NPTS, ZSTART, DELT(1)*SX, DELT(2)*SX, 4,
     $   MEMR(XADD2), MEMR(YADD2))
      IF (FGAUS) THEN
         CALL PIXRASL2 (MEMR(GADD), NAXIS(1), NAXIS(2), X1, X2,
     $      Y1, Y2, NPTS, ZSTART, DELT(1)*SX, DELT(2)*SX, 4,
     $      MEMR(XADD2), MEMR(FGADD2))
      END IF
      IF (ERROR) GO TO 999
C
C Autoscale
C
      XMIN = MEMR(XADD1)
      XMAX = XMIN
      DO 100 I = 1, NPTS-1
         XMIN = MIN(XMIN, MEMR(XADD1+I))
         XMAX = MAX(XMAX, MEMR(XADD1+I))
         XMIN = MIN(XMIN, MEMR(XADD2+I))
         XMAX = MAX(XMAX, MEMR(XADD2+I))
 100  CONTINUE
      IF (YMIN.EQ.0.0) THEN
         YMIN = MEMR(YADD1)
         DO 110 I = 1, NPTS-1
            YMIN = MIN(YMIN, MEMR(YADD1+I))
            YMIN = MIN(YMIN, MEMR(YADD2+I))
 110     CONTINUE
      END IF
      IF (YMAX.EQ.0.0) THEN
         YMAX = MEMR(YADD1)
         DO 120 I = 1, NPTS-1
            YMAX = MAX(YMAX, MEMR(YADD1+I))
            YMAX = MAX(YMAX, MEMR(YADD2+I))
 120     CONTINUE
      END IF
      T = (XMAX - XMIN) * 0.03
      XMAX = XMAX + T
      XMIN = XMIN - T
      T = (YMAX - YMIN) * 0.03
      YMAX = YMAX + T
      YMIN = YMIN - T
C
C Call PGPLOT routines: Get device, etc
C
      IF (DEVICE.NE.' ') THEN
         MESSAGE = 'Device = ' // DEVICE
         CALL MSGPUT (MESSAGE, 'I')
         ISTAT = PGBEGIN(0, DEVICE, 1, 1)
         IF (ISTAT.NE.1) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Cannot open graphics device '//DEVICE)
            GO TO 999
         END IF
         CALL PGSCH(CHEIGHT)
         CALL PGSCF(2)
         CALL PGSLW (LWIDTH)
         CALL PGSCH(1.25*CHEIGHT)
         CALL PGVSTD
         CALL PGWINDOW (XMIN,XMAX,YMIN,YMAX)
         IF (LEFT) THEN
            CALL PGBOX('BCNSTA', 0.0, 0, 'BCNSTA', 0.0, 0)
            CALL PGMTXT('L', 2.2, 0.5, 0.5, YLABEL)
         ELSE
            CALL PGBOX('BCNSTA', 0.0, 0, 'BCMSTA', 0.0, 0)
            CALL PGMTXT('R', 2.7, 0.5, 0.5, YLABEL)
         END IF
         CALL PGMTXT('B', 3.2, 0.5, 0.5, XLABEL)
         CALL PGSCH(CHEIGHT)
C
         CALL PGSLS (1)
         CALL PGLINE (NPTS, MEMR(XADD1), MEMR(YADD1))
         CALL PGLINE (NPTS, MEMR(XADD2), MEMR(YADD2))
C
         IF (FGAUS) THEN
            CALL PGSLS (2)
            CALL PGLINE (NPTS, MEMR(XADD1), MEMR(FGADD1))
            CALL PGLINE (NPTS, MEMR(XADD2), MEMR(FGADD2))
         END IF
C
C Annotate, if necessary
C
         IF (ANNOT.NE.' ') CALL IMGANNOT (ANNOT)
C
         CALL PGEND
      END IF
C
C Text file output
C
      IF (TEXTFILE.NE.' ') THEN
         CALL FILDEL (TEXTFILE)
         MESSAGE = 'Opening ' // TEXTFILE(1:STRLEN(TEXTFILE)) //
     $      ' for Text Output'
         CALL MSGPUT (MESSAGE, 'I')
         CALL TXTOPEN ('Text', TEXTFILE, 'WRITE')
         IF (ERROR) GO TO 999
C
         IF (VLBI) THEN
            IF (FGAUS) THEN
               MESSAGE = '# Index Radius(mas) Bmaj Bmin Fmaj Fmin'
            ELSE
               MESSAGE = '# Index Radius(mas) Bmaj Bmin'
            END IF
         ELSE
            IF (FGAUS) THEN
               MESSAGE = '# Index Radius(asec) Bmaj Bmin Fmaj Fmin'
            ELSE
               MESSAGE = '# Index Radius(asec) Bmaj Bmin'
            END IF
         END IF
         CALL TXTWRITE ('Text',MESSAGE)
C
         DO 200 I = 1, NPTS
 1200       FORMAT (I6,2X,1P,5E13.5)
            IF (FGAUS) THEN
               WRITE (MESSAGE, 1200) I, MEMR(XADD1+I-1),
     $            MEMR(YADD1+I-1), MEMR(YADD2+I-1),
     $            MEMR(FGADD1+I-1), MEMR(FGADD2+I-1)
            ELSE
               WRITE (MESSAGE, 1200) I, MEMR(XADD1+I-1),
     $            MEMR(YADD1+I-1), MEMR(YADD2+I-1)
            END IF
            CALL TXTWRITE ('Text',MESSAGE)
 200     CONTINUE
         CALL TXTCLOSE ('Text')
      END IF
C
 999  CONTINUE
      END

