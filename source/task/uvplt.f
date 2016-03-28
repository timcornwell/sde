C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvplt.f	1.8    3/3/95
C
      SUBROUTINE SDEMAIN
C
CD Program to plot uv data
C
C Audit trail:
C	New task
C				T.J. Cornwell	March 10 1989
C       Added SQUARE, XLIMITS
C                               T.J. Cornwell   April 17 1989
C       Made AUTOSCALE accessable
C				D.S. Briggs     August 30 1991
C       Added weight argument
C				D.S.Briggs	July 24 1992
C       Added PTtype argument and special axis name UVR
C				D.S.Briggs	July 12 1994
C	Added ID, Left, CHeight & Lwidth, Annot options
C				D.S.Briggs	July 22 1994
C	Added XMode, YMode to specify conversion from Complex to
C	real quantities
C				D.S.Briggs	July 28 1994
C	Added XTick, YTick, XNSub, YNSub for those really stubborn
C	cases where the plot won't look right.
C				D.S.Briggs	Nov 15 1994
C	Added XBinOut and YBinOUt
C				D.S.Briggs	March 3 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVPLT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, X, Y, PLFILE, TLABEL,
     $   XLABEL, YLABEL, WEIGHT, XARR, YARR, AFILE, XCONV, YCONV,
     $   XBINOUT, YBINOUT
      REAL              XLIMITS(2), YLIMITS(2), CHEIGHT, XTICK, YTICK
      LOGICAL           REVERSE, SQUARE, AUTOSCL, ID, LEFT
      INTEGER		NDUMMY, SKIP, PTTYPE, LWIDTH, XNSUB, YNSUB
C==================================================================
      CALL MSGWELCO ('I plot uv data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('X', X, 1, NDUMMY)
      CALL USRGETC ('Y', Y, 1, NDUMMY)
      CALL USRGETC ('Weight', WEIGHT, 1, NDUMMY)
      CALL USRGETI ('Skip', SKIP, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETL ('Conjugate', REVERSE, 1, NDUMMY)
      CALL USRGETL ('Square', SQUARE, 1, NDUMMY)
      CALL USRGETR ('Xlimits', XLIMITS, 2, NDUMMY)
      CALL USRGETR ('Ylimits', YLIMITS, 2, NDUMMY)
      CALL USRGETR ('XTick', XTICK, 1, NDUMMY)
      CALL USRGETR ('YTick', YTICK, 1, NDUMMY)
      CALL USRGETI ('XNSub', XNSUB, 1, NDUMMY)
      CALL USRGETI ('YNSub', YNSUB, 1, NDUMMY)
      CALL USRGETC ('XConvert', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, XCONV)
      CALL USRGETC ('YConvert', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, YCONV)
      CALL USRGETR ('CHeight', CHEIGHT, 1, NDUMMY)
      CALL USRGETI ('LWidth', LWIDTH, 1, NDUMMY)
      CALL USRGETC ('Plotlabel', TLABEL, 1, NDUMMY)
      CALL USRGETC ('Xlabel', XLABEL, 1, NDUMMY)
      CALL USRGETC ('Ylabel', YLABEL, 1, NDUMMY)
      CALL USRGETC ('Annot', AFILE, 1, NDUMMY)
      CALL USRGETL ('ID', ID, 1, NDUMMY)
      CALL USRGETL ('Left', LEFT, 1, NDUMMY)
      CALL USRGETI ('PTtype', PTTYPE, 1, NDUMMY)
      CALL USRGETC ('Plotfile', PLFILE, 1, NDUMMY)
      CALL USRGETC ('XBinOut', XBINOUT, 1, NDUMMY)
      CALL USRGETC ('YBinOut', YBINOUT, 1, NDUMMY)
C
      IF (XCONV.EQ.' ') XCONV = 'AMP'
      IF (YCONV.EQ.' ') YCONV = 'AMP'
      IF ((XCONV.NE.'AMP').AND.(XCONV.NE.'PHASE').AND.(XCONV.NE.'REAL')
     $    .AND.(XCONV.NE.'IMAG')) THEN
         CALL MSGPUT ('Unrecognized X conversion mode','E')
         GO TO 999
      END IF
      IF ((YCONV.NE.'AMP').AND.(YCONV.NE.'PHASE').AND.(YCONV.NE.'REAL')
     $    .AND.(YCONV.NE.'IMAG')) THEN
         CALL MSGPUT ('Unrecognized Y conversion mode','E')
         GO TO 999
      END IF
C
C Default a few things
C
      AUTOSCL = ((XLIMITS(1).EQ.0.0).AND.(XLIMITS(2).EQ.0.0).AND.
     $           (YLIMITS(1).EQ.0.0).AND.(YLIMITS(2).EQ.0.0))
      IF (WEIGHT.NE.' ') THEN
         STRBUF = WEIGHT
         WEIGHT = 'Vis/' // STRBUF
      END IF
      XARR = 'Vis/' // X
      YARR = 'Vis/' // Y
C
C Label defaults
C
      IF ((X.EQ.'UU').AND.(XLABEL.EQ.'*')) XLABEL = 'U (wavelengths)'
      IF ((Y.EQ.'UU').AND.(YLABEL.EQ.'*')) YLABEL = 'U (wavelengths)'
      IF ((X.EQ.'VV').AND.(XLABEL.EQ.'*')) XLABEL = 'V (wavelengths)'
      IF ((Y.EQ.'VV').AND.(YLABEL.EQ.'*')) YLABEL = 'V (wavelengths)'
      IF ((X.EQ.'UVR').AND.(XLABEL.EQ.'*'))
     $   XLABEL = 'uv Radius (wavelengths)'
      IF ((Y.EQ.'UVR').AND.(YLABEL.EQ.'*'))
     $   YLABEL = 'uv Radius (wavelengths)'
      IF ((INDEX(X,'VIS').GT.0).AND.(XLABEL.EQ.'*')) THEN
         IF (XCONV.EQ.'AMP') THEN
            XLABEL = 'Visibility Amplitude (Jy)'
         ELSE IF (XCONV.EQ.'PHASE') THEN
            XLABEL = 'Visibility Phase (degrees)'
         ELSE IF (XCONV.EQ.'REAL') THEN
            XLABEL = 'Visibility Real Part (Jy)'
         ELSE IF (XCONV.EQ.'IMAG') THEN
            XLABEL = 'Visibility Imaginary part (Jy)'
         END IF
      END IF
      IF ((INDEX(Y,'VIS').GT.0).AND.(YLABEL.EQ.'*')) THEN
         IF (YCONV.EQ.'AMP') THEN
            YLABEL = 'Visibility Amplitude (Jy)'
         ELSE IF (YCONV.EQ.'PHASE') THEN
            YLABEL = 'Visibility Phase (degrees)'
         ELSE IF (YCONV.EQ.'REAL') THEN
            YLABEL = 'Visibility Real Part (Jy)'
         ELSE IF (YCONV.EQ.'IMAG') THEN
            YLABEL = 'Visibility Imaginary part (Jy)'
         END IF
      END IF
C
C Get the visibilities
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C A special name
C
      IF ((X.EQ.'UVR').OR.(Y.EQ.'UVR')) THEN
         CALL ARRMULT ('Vis/UU', 'Vis/UU', 'U2')
         CALL ARRMULT ('Vis/VV', 'Vis/VV', 'V2')
         CALL ARRADD ('U2', 'V2', 'R2')
         CALL ARRSQRT ('R2', 'R')
         IF (X.EQ.'UVR') XARR = 'R'
         IF (Y.EQ.'UVR') YARR = 'R'
      END IF
C
C Make options directory
C
      CALL DATCREAT ('Options')
      CALL DATPUTC ('Options', 'DEV', PLFILE, 1)
      CALL DATPUTI ('Options', 'PTSKIP', SKIP, 1)
      CALL DATPUTI ('Options', 'PTTYPE', PTTYPE, 1)
      CALL DATPUTC ('Options', 'PLTLABEL', TLABEL, 1)
      CALL DATPUTC ('Options', 'XLABEL', XLABEL, 1)
      CALL DATPUTC ('Options', 'YLABEL', YLABEL, 1)
      CALL DATPUTC ('Options', 'XCONV', XCONV, 1)
      CALL DATPUTC ('Options', 'YCONV', YCONV, 1)
      CALL DATPUTC ('Options', 'ANNOT', AFILE, 1)
      CALL DATPUTL ('Options', 'REVERSE', REVERSE, 1)
      CALL DATPUTL ('Options', 'SQUARE', SQUARE, 1)
      CALL DATPUTL ('Options', 'ID', ID, 1)
      CALL DATPUTL ('Options', 'LEFT', LEFT, 1)
      CALL DATPUTR ('Options', 'CHEIGHT', CHEIGHT, 1)
      CALL DATPUTI ('Options', 'LWIDTH', LWIDTH, 1)
      IF (AUTOSCL) THEN
         CALL MSGPUT ('Autoscaling','I')
         CALL DATPUTL ('Options', 'AUTOSCL', AUTOSCL, 1)
      ELSE
         CALL DATPUTR ('Options', 'XLIMITS', XLIMITS, 2)
         CALL DATPUTR ('Options', 'YLIMITS', YLIMITS, 2)
      END IF
      CALL DATPUTR ('Options', 'XTICK', XTICK, 1)
      CALL DATPUTR ('Options', 'YTICK', YTICK, 1)
      CALL DATPUTI ('Options', 'XNSUB', XNSUB, 1)
      CALL DATPUTI ('Options', 'YNSUB', YNSUB, 1)
C
C Call routine to plot the data
C
      IF (PLFILE.NE.' ') CALL ARR2DPLT (XARR, YARR, WEIGHT, 'Options')
C
C Write output in binary if requested
C
      IF (XBINOUT.NE.' ') CALL FILARRPU (XARR, XBINOUT)
      IF (YBINOUT.NE.' ') CALL FILARRPU (YARR, YBINOUT)
C
 999  CONTINUE
      END
