C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)rvres.f	1.4	 6/28/94
C
      SUBROUTINE SDEMAIN
C
C  generate residual visibility between MODEL and IMAGE
C  and average them radially
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: 
C				M.A.Holdaway	March 12 1991
C	Quick Fix:  IMGBMCMP is broken, dont call it, which is fine
C	for "FULL RESOLUTION" images.
C				M.A.Holdaway	June 24 1991
C	Track changes to ARRPGGRF
C				D.S.Briggs	July 15 1992
C	IMGBMCMP magically works again
C				M.A. Holdaway	June 28 1994
C-----------------------------------------------------------------------
#include "stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'RVRES')
C
      CHARACTER*(SYSMXNAM) 	MODEL, IMAGE, DEV1, DEV2, DEV3,
     $   			SENS, FIL3
      REAL			PIXBIN, MAXRAD, MINRAD
      REAL			XMIN, XMAX, YMIN, YMAX
C
      INTEGER		NDUMMY, NAXI, NAXM, NAXV, I, NPIX
      INTEGER		NAXISI(SYSMXDIM), NAXISM(SYSMXDIM),
     $   		NAXISV(SYSMXDIM)
      CHARACTER*(8)	CTYPEI(SYSMXDIM), CTYPEM(SYSMXDIM), 
     $   		TYPEV (SYSMXDIM)
      REAL		RPIXI (SYSMXDIM), RPIXM (SYSMXDIM),
     $   RPIXV (SYSMXDIM)
      REAL		DELTI (SYSMXDIM), DELTM (SYSMXDIM),
     $   DELTV (SYSMXDIM)
      REAL		ROTAI (SYSMXDIM), ROTAM (SYSMXDIM),
     $   ROTAV (SYSMXDIM)
      DOUBLE PRECISION	RVALI (SYSMXDIM), RVALM (SYSMXDIM),
     $   RVALV (SYSMXDIM)
      REAL		CLIP
      REAL		SCALE, FREQ, IMFLUX, MODFLUX, FLUXSCALE
      REAL		TOL
      CHARACTER*(80)	TITLE, XLAB
      CHARACTER*(SYSMXNAM)	GC, COMMENT, LABEL, UVDIST, NAVRES
      LOGICAL		IMSCALE, DOEXIT, FIRST, DATEXIST
C==================================================================
C
      CALL MSGWELCO ('I find radially averged residual visibilities')
      FIRST = .TRUE.
 1    CONTINUE
      CALL USRCTL
      CALL USRGETGO(GC)
      IF (INDEX(GC, 'clear') .NE. 0) THEN
         FIRST = .TRUE.
         IF (DATEXIST('Image')) CALL DATDELET ('Image')
         IF (DATEXIST('Model')) CALL DATDELET ('Model')
         IF (DATEXIST('Ires'))  CALL DATDELET ('Ires')
         IF (DATEXIST('Vtemp')) CALL DATDELET ('Vtemp')
         IF (DATEXIST('Vres'))  CALL DATDELET ('Vres')
         IF (DATEXIST('Ravmod')) CALL DATDELET ('Ravmod')
         IF (DATEXIST('Ravres')) CALL DATDELET ('Ravres')
         IF (DATEXIST('PlotData')) CALL DATDELET ('PlotData')
         IF (DATEXIST('Sens')) CALL DATDELET ('Sens')
      END IF
      IF (FIRST) THEN
C
C Get Image
C
         CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
         CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
         CALL FILIMGGE ('Image', IMAGE, ' ')
         CALL USRGETC ('ModImage', MODEL, 1, NDUMMY)
         CALL FILIMGGE ('Model', MODEL, ' ')         
         CALL USRGETC ('Sensitivity', SENS, 1, NDUMMY)
         IF (SENS .NE. ' ') THEN
            CALL FILIMGGE ('Sens', SENS, ' ')
            CALL ARRMULT ('Image', 'Sens', 'Image')
            CALL ARRMULT ('Model', 'Sens', 'Model')
         ELSE
            CALL USRGETR ('Maxradius', MAXRAD, 1, NDUMMY)
            CLIP = 0.0
            MAXRAD = MAXRAD/3600.
            MINRAD = 0.0
            CALL ARRRDCLP ('Image', MINRAD, MAXRAD, CLIP)
            CALL ARRRDCLP ('Model', MINRAD, MAXRAD, CLIP)
         ENDIF
C
         TOL = .000001
         DOEXIT = .FALSE.
C
C Dump coordinates
C
         CALL MSGPUT ('Coordinates for Image:', 'I')
         CALL CRDLIST ('Image')
         CALL MSGPUT ('Coordinates for Model:', 'I')
         CALL CRDLIST ('Model')
C
C Compare Image and Model; are they compatible?
C
         CALL CRDGET ('Image', NAXI, CTYPEI, NAXISI, RVALI, RPIXI, 
     $      DELTI, ROTAI)
         CALL CRDGET ('Model', NAXM, CTYPEM, NAXISM, RVALM, RPIXM, 
     $      DELTM, ROTAM)
         DO 10 I = 1, NAXI
            IF (NAXISI(I) .LE. 1 .AND. NAXISM(I) .LE. 1) GOTO 10
            IF (NAXISI(I) .NE. NAXISM(I)) THEN
               DOEXIT = .TRUE.
               CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $            'Axes of Image and MODEL do not agree')
            ENDIF
            IF (ABS( (DELTI(I) - DELTM(I))/DELTI(I)) .GT. TOL) THEN
               DOEXIT = .TRUE.
               WRITE (MESSAGE, 1017) I, DELTI(I), DELTM(I)
 1017          FORMAT( 'Axis: ',I1,' Image Inc: ',F12.6,
     $            ' Model Inc: ',F12.6)
               CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            END IF
 10      CONTINUE
         IF (DOEXIT) GOTO 990
C
C Convolve the model with the IMAGE`s beam
C
         CALL IMGBMCMP ('Model', 'Image', 'Model')
         IF (ERROR) GOTO 999
C
C Do we need to scale the images to match the total flux?
C
         CALL USRGETL ('Imscale', IMSCALE, 1, NDUMMY)
         IF (IMSCALE) THEN
            CALL ARRSUM ('Image', IMFLUX)
            CALL ARRSUM ('Model', MODFLUX)
            FLUXSCALE = MODFLUX/IMFLUX
            CALL ARRSCALE ('Image', FLUXSCALE, 0.0, 'Image')
         END IF
C
C Subtract Image from 'Model'
C
         CALL IMGCLONE ('Image', 'Ires')
         CALL ARRLC ('Model', 1.0, 'Image', -1.0, 'Ires')
         IF (ERROR) GOTO 999
C
C Fourier transform the Image residuals, Get AMPLITUDE
C
         CALL IMGFFT ('Ires', 'Vtemp')
         CALL ARRABS ('Vtemp', 'Vres')
         IF (ERROR) GOTO 999
         CALL CRDGET ('Vtemp', NAXV, TYPEV, NAXISV, RVALV, RPIXV, DELTV,
     $      ROTAV)
         CALL CRDPUT ('Vres', NAXV, TYPEV, NAXISV, RVALV, RPIXV, DELTV,
     $      ROTAV)
         CALL MSGPUT ('Coordinates for Vis:', 'I')
         CALL CRDLIST ('Vres')
C      CALL IMGTVD ('Vres', 0., 0. )
C
C get scaling factor
C
         IF (RVALI(3) .NE. 0) THEN
            FREQ = RVALI(3)
            SCALE = ABS (DELTV(1) * 3.0E+8 / FREQ )
            XLAB = 'radial (u,v) distance, meters'
         ELSE
            SCALE = 1.
            XLAB = 'RADIUS--PIXELS'
         ENDIF
C
C Now average the residual visibilities radially
C
         CALL DATCREAT ('PlotData')
         UVDIST = 'PlotData/X'
         NAVRES = 'PlotData/Y'
         CALL USRGETR ('Binsize', PIXBIN, 1, NDUMMY)
         NPIX =  FLOAT ( NAXISI(1) * NAXISI(2) )/ PIXBIN + 1
         CALL IMGRAVE ('Vres', 'Ravres', UVDIST, NPIX, SCALE)
C
         CALL USRGETC ('Dev1', DEV1, 1, NDUMMY)
         IF (DEV1(1:1) .NE. ' ') THEN
            CALL USRGETR ('Xmin', XMIN, 1, NDUMMY)
            CALL USRGETR ('Xmax', XMAX, 1, NDUMMY)
            CALL USRGETR ('Ymin', YMIN, 1, NDUMMY)
            CALL USRGETR ('Ymax', YMAX, 1, NDUMMY)
            TITLE = 'Radially Averaged Residual Visibilities--'
     $         //IMAGE
            CALL ARRPGGRF (NPIX, UVDIST, 'Ravres', DEV1, 
     $         XLAB, 'Error, JY', TITLE, 0, 0, XMIN, XMAX, YMIN, YMAX)
         ENDIF
         IF (ERROR) GOTO 999
C
C Normalize the residual vis with the AMP of MODEL vis
C
         CALL IMGFFT ('Model', 'Vtemp')
         CALL ARRABS ('Vtemp', 'Vmod')
         CALL CRDGET ('Vtemp', NAXV, TYPEV, NAXISV, RVALV, RPIXV, 
     $      DELTV, ROTAV)
         CALL CRDPUT ('Vmod', NAXV, TYPEV, NAXISV, RVALV, RPIXV, 
     $      DELTV, ROTAV)
         CALL IMGRAVE ('Vmod', 'Ravmod', UVDIST, NPIX, SCALE)
C
         CALL USRGETC ('Dev2', DEV2, 1, NDUMMY)
         IF (DEV2(1:1) .NE. ' ') THEN
            CALL USRGETR ('Xmin', XMIN, 1, NDUMMY)
            CALL USRGETR ('Xmax', XMAX, 1, NDUMMY)
            CALL USRGETR ('Ymin', YMIN, 1, NDUMMY)
            CALL USRGETR ('Ymax', YMAX, 1, NDUMMY)
            TITLE = 'Radially Averaged Model Visibilities--'
     $         //IMAGE
            CALL ARRPGGRF (NPIX, UVDIST, 'Ravmod', DEV2, 
     $         XLAB, 'JY', TITLE, 0, 0, XMIN, XMAX, YMIN, YMAX)
         ENDIF
         IF (ERROR) GOTO 999
C
         CALL ARRDIV ('Ravres', 'Ravmod', NAVRES)
         IF (DEV2(1:1) .NE. '/' .AND. DEV2 .NE. ' ') THEN
            MESSAGE = 'Model visibility plot is in file '
            CALL STRAPPEN (MESSAGE, DEV2)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
         IF (DEV1(1:1) .NE. '/' .AND. DEV1 .NE. ' ') THEN
            MESSAGE = 'UN-Normalized residuals plot is in file '
            CALL STRAPPEN (MESSAGE, DEV1)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      ENDIF 
C 
C Graph it all up 
C
      CALL USRGETC ('Dev3', DEV3, 1, NDUMMY)
      IF (DEV3 .NE. '  ') THEN
         CALL USRGETR ('Xmin', XMIN, 1, NDUMMY)
         CALL USRGETR ('Xmax', XMAX, 1, NDUMMY)
         CALL USRGETR ('Ymin', YMIN, 1, NDUMMY)
         CALL USRGETR ('Ymax', YMAX, 1, NDUMMY)
         TITLE='Normalized Radially Averaged Residual Visibilities--'
     $      //IMAGE
         CALL ARRPGGRF (NPIX, UVDIST, NAVRES, DEV3, 
     $      XLAB, 'Fractional Error', TITLE, 0, 0, XMIN, XMAX,
     $      YMIN, YMAX)
         IF (ERROR) GOTO 999
         IF (DEV3(1:1) .NE. '/' ) THEN
            MESSAGE = 'Normaized residuals plot is in file '
            CALL STRAPPEN (MESSAGE, DEV3)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
      END IF
      CALL USRGETC ('Fil3', FIL3, 1, NDUMMY)
      CALL USRGETC ('Label', LABEL, 1, NDUMMY)
      IF (FIL3 .NE. ' ') THEN
         COMMENT='Normalized Radially Averaged Residual Visibilities--'
     $      //IMAGE
         CALL DATPUTC ('PlotData', 'COMMENT', COMMENT, 1)
         CALL DATPUTC ('PlotData', 'LABEL', LABEL, 1)
         CALL DATPUTL ('PlotData', 'DISCRETE', .FALSE., 1)
         CALL FILPUTPF ('PlotData', FIL3)
         IF (ERROR) GOTO 999
         MESSAGE = 'Normaized residuals data is in SDE plot file '
         CALL STRAPPEN (MESSAGE, FIL3)
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
      FIRST = .FALSE.
      GO TO 1
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE
      ENDIF
 999  CONTINUE
      END
