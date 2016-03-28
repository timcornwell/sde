C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fly.f	1.35    8/26/92
C
      SUBROUTINE SDEMAIN
C
#define maxnpat 625
#define Interp 'SEPTIC'
C
CD Program to map and clean visibility data looping as required to vis data.
C Always does slow transform in Z. Can handle
C up to maxnpat patchs each of which can be 3D (if your computer can handle
C it, that is). The memory required is dominated by that for one image
C and that for the visibility data. This program makes extensive use
C of the ability to link arrays. There are many more logical arrays
C than real arrays. Most just differ in the header.
C
C Storage requirements:
C
C	Name		Internal name	Size (fp words)
C
C	Components	CMP		4*NITER
C	Window 		WIN		NPATCH*NX*NY
C	MVis		MVS		NX*NY*NZ or NPATCH*NX*NY*NZ
C	Dirty		DRT		NX*NY*NZ
C	Dirty list	DRT/PIXLIST,etc	3*CCNL
C	2D Dirty	DRT2D		NX*NY
C	Obs. Visibility	Vis/OBS/I/VIS	2*NVIS
C	Temp Visibility	Vis/TMP/I/VIS	2*NVIS
C	Weights		Vis/OBS/I/WT	NVIS
C	Saved Weights	Vis/OBS/I/SWT	NVIS
C	U,V,W		Vis/UU, etc	3*NVIS
C	Clean image	Clean		NPATCH*NX*NY
C
C Hence the total storage in space saving mode is about:
C
C NPATCH*2*NX*NY+NX*NY*(2*NZ+1)+9*NVIS+4*NITER fp words
C
C In CPU saving mode the space required is about:
C
C NPATCH*[2*NX*NY+NX*NY*(2*NZ+1)]+9*NVIS+4*NITER fp words
C
C Audit trail:
C	Major re-write to reduce memory required, and otherwise
C	optimize things.
C					T.J. Cornwell  Feb 17 1990
C	Added HGEOM capability
C					T.J. Cornwell  April 4 1990
C	Added 3D capability
C					T.J. Cornwell  April 18 1990
C	Changed to IMGSUBPS to find exterior sidelobe
C					T.J. Cornwell  April 29 1990
C	Added checkpoint facility
C					T.J. Cornwell  May 5 1990
C	Saved some space by linking some of the WT arrays
C					T.J. Cornwell June 17 1990
C	Related spacing to BLC, TRC so that we can use a small portion
C	of a large image. This should be useful for BOX convolution.
C					T.J. Cornwell June 22 1990
C	Added IMGGRIDC before IMGFFT and VISDEGRI! Did not work without
C	this step. Also added renormalization of output of VISTOIMG.
C	Also does uniform weighting on grid for final image so fly
C	now gives answers very close to the output from uvmap,clean
C	imgdesph cycle.
C					T.J. Cornwell March 8 1991
C	Added window function to IMGFCLEA so that duplicated pixels
C	are not overcleaned. The window function for each patch is found
C	from the number of overlaps and are clipped to the range
C	0.25 to 1.0
C					T.J. Cornwell March 15 1991
C	Added capability to do zero iterations
C					T.J. Cornwell March 20 1991
C	Removed input of BLC, TRC. Changed maximum number of fields
C	to accomodate NP=7.
C					T.J. Cornwell April 3 1991
C	Now does uniform weighting for each field
C					T.J. Cornwell April 13 1991
C	NITER is now the maximum number of components for any field
C					T.J. Cornwell April 14 1991
C	Now does not expand a full factor of two. Instead expands to
C	nearest factor of two. Also removed some extraneous images
C	and put in estimate of memory usage. Changed method of
C	calculating WIN functions for 3D case.
C					T.J. Cornwell May 2 1991
C	Use larger border to ensure that no points are missed in the
C	overlap.
C					T.J. Cornwell May 10 1991
C	Fixed for IMGHGEOM which adds to existing image
C					T.J. Cornwell May 19 1991
C	Now makes header only image in uniform weighting: saves
C	space!
C					T.J. Cornwell May 31 1991
C	FOV now controls fraction of FOV which is used for uniform
C	weighting
C					T.J. Cornwell June 4 1991
C	Added BMAJ, BMIN, BPA, BZ to header of CLEAN image
C					M.A.Holdaway  Sept 27 1991
C	Added BORDER as an explicit input parameter
C					T.J. Cornwell November 8 1991
C	NPATCH.EQ.1 bug found by Mark
C					T.J. Cornwell April 10 1992
C	Added STOKES capability
C					T.J. Cornwell June 26 1992
C	Changed memory calculation to be consistent with flydoc.
C	Also expanded maxnpat to 625 in anticipation of A-array
C	Also changed to consolidate all clean components in one
C	place by using an extra array FIELD to hold the field
C	number. This results in large savings in some cases.
C	Does cleaning in order of residuals so that when iteration
C	is stopped early something moderately sensible results.
C					T.J. Cornwell July 16 1992
C	Added flagging of local V-axis (ie for each patch). Don't
C	restore empty patches.
C					T.J. Cornwell July 22 1992
C	Output rectangular coordinates of patches
C					T.J. Cornwell July 23 1992
C	Now passes BITER for a group of iterations so that the SMN
C	conservatism factor is computed correctly.
C
C	Can expand number of clean components. Can restart.
C					T.J. Cornwell August 16 1992
C	Now calculates SLIM from summmed effect of all sources in the
C	field. This should prevent over-cleaning during any minor
C	cycle.
C					T.J. Cornwell August 17 1992
C	Now does full 3D phase transform img->vis for points 
C	brighter than some threshold. This increases the
C	dynamic range considerably.
C					T.J. Cornwell August 19 1992
C	Fixed error on making dirty image
C					T.J. Cornwell August 26 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLY')
C
      INTEGER 		NDUMMY, I, DIR, NSEL, NCYCLE, TV, TNITER,
     1			BP, NITER, LNITER, IMSIZE(3), CCNL, TIMR(8),
     2			PIMSIZE(3), NX, NY, NZ, NVIS, BORDER, BITER,
     $   		RITER
      REAL		CELLSIZE(3), SPEED, D2R, TAPER(3),
     1			TIME(2), UVLIMITS(2), SHIFT(2), SLIM, AFLUX,
     2			FLUX, GAIN, BEAM(4), LIMIT,
     3			ARRMAX, DATFGETR, MAXARES, NOV
      LOGICAL		DATEXIST, SAVESPC, THREED, RESTART, 
     $			MAKEPSF, FLYNEIGH, RESCURR
C
C Parameters of patches
C
      INTEGER		IPATCH, INPATCH, NPATCH, ANPATCH, IX, IY,
     $			PNAXIS(SYSMXDIM), HNAXIS(SYSMXDIM), IPATCHI
      REAL		PMAXRES(maxnpat), TFLUX(maxnpat), TTFLUX,
     $			PMINRES(maxnpat), PRMSRES(maxnpat), ITFLUX
      REAL		SPACING(2), RASPACE, DECSPACE, FOV,
     $			PSFPEAK, FSWITCH
      INTEGER		ANITER, ORDER(maxnpat)
C
      INTEGER           VNAX, NRNAX, STRSEARC, NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  DEC
      DOUBLE PRECISION  RVAL(SYSMXDIM), RVAL2D(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       ITYPE(SYSMXDIM), VTYPE(SYSMXDIM), TYPE(SYSMXDIM)
C
      CHARACTER*(SYSMXNAM)	RESFILE, VISFILE, CHPTFILE, STRM2,
     1			SCLNFILE, CONVFN, CMP, WIN, DRT, DRT2D, MVS, 
     $			PSFFILE, CMPN, WINN, DRTN, DRT2DN, MVSN, STOKES
      CHARACTER*1	SS, ATYPE
      CHARACTER*13	FS
C
      DATA		PNAXIS	/SYSMXDIM * 1/
      DATA		HNAXIS	/SYSMXDIM * 1/
      DATA		PIMSIZE	/3 * 1/
      DATA		TIME	/2*0.0/
      DATA		PMAXRES /maxnpat * 0.0/
      DATA		PMINRES /maxnpat * 0.0/
      DATA		PRMSRES /maxnpat * 0.0/
      DATA		TFLUX /maxnpat * 0.0/
      DATA		TTFLUX /0.0/
      DATA		ANITER /0/
C==================================================================
      D2R = ATAN(1.0) / 45.0 
C
      CALL MSGWELCO('I perform wide-field imaging using a polyhedron')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 2, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETC ('ConvType', CONVFN, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('UDistance', NOV, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Checkpoint', CHPTFILE, 1, NDUMMY)
      CALL USRGETL ('Restart', RESTART, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETI ('Niter', NITER, 1, NDUMMY)
      CALL USRGETI ('Riter', RITER, 1, NDUMMY)
      CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
      CALL USRGETR ('Fswitch', FSWITCH, 1, NDUMMY)
      CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
      CALL USRGETI ('Border', BORDER, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Speed', Speed, 1, NDUMMY)
      CALL USRGETR ('Limit', LIMIT, 1, NDUMMY)
      CALL USRGETL ('Space', SAVESPC, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI ('TVImage', TV, 1, NDUMMY)
      CALL USRGETI ('Numcl', CCNL, 1, NDUMMY)
      CALL USRGETI ('Bpatch', BP, 1, NDUMMY)
      CALL USRGETI ('Np', NPATCH, 1, NDUMMY)
      IF (NPATCH.GT.12) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Np must be < 13')
         GO TO 999
      END IF
      THREED = IMSIZE(3).GT.1
      RESTART = RESTART.AND.(CHPTFILE.NE.' ')
      SS = STOKES(1:1)
C
C Now get visibility data and select it. Then clone model visibility.
C
      CALL VISGET ('Vis', VISFILE, SS, '*', ' ')
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis','OBS/'//SS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
         CALL DATDELET ('Vis/TIME')
      ENDIF
      IF(DATEXIST('Vis/BASELINE')) THEN
         CALL DATDELET ('Vis/BASELINE')
      END IF
C
C Make PSF?
C
      IF(PSFFILE.NE.' ') THEN
         MAKEPSF=.TRUE.
         CALL ARRSETCO ('Vis/OBS/'//SS//'/VIS', 0.0, 1.0)
         CALL MSGPUT ('Making PSF only', 'I')
      ELSE
         MAKEPSF=.FALSE.
      ENDIF
C
C Make final image to be used for uniform weighting. Make header only.
C
      PNAXIS(1) = MAX(IMSIZE(1), (IMSIZE(1)-2*BORDER)*(2*NPATCH+1))
      PNAXIS(2) = MAX(IMSIZE(2), (IMSIZE(2)-2*BORDER)*(2*NPATCH+1))
      PNAXIS(3) = 1
C
C Re-weight the data to be uniform. 
C
      IF (FOV.GT.0.0) THEN
         HNAXIS(1)=FOV * PNAXIS(1)
         HNAXIS(2)=FOV * PNAXIS(2)
         CALL IMGMAKE ('Vis/OBS/'//SS, CELLSIZE, HNAXIS, SHIFT, ' ',
     $      'HClean')
         CALL FFTCONJA ('HClean', 'BigMVis', DIR, 2)
         WRITE (MESSAGE, 1100) FOV
 1100    FORMAT ('Weighting for fraction of field of view = ',
     1      F7.2)
         CALL MSGPUT (MESSAGE, 'I')
         CALL DATPUTR ('BigMVis', 'WTFOV', FOV, 1)
         CALL GRDUWT ('Vis', 'OBS/'//SS, 'BigMVis')
         CALL DATDELET ('BigMVis')
         CALL DATDELET ('HClean')
      END IF
C
C Now make actual image
C
      CALL IMGMAKE ('Vis/OBS/'//SS, CELLSIZE, PNAXIS, SHIFT, 'R', 
     $   'Clean')
      CALL MSGPUT ('Coordinates for final image', 'I')
      CALL CRDLIST ('Clean')
C
C Apply any taper required
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         CALL VISTAPER ('Vis', 'OBS/'//SS, TAPER, 'OBS/'//SS)
      END IF
C
C Clone model and residual visibilities
C
      CALL DATCREAT ('Vis/TMP')
      CALL DATCREAT ('Vis/TMP/'//SS)
      CALL ARRCOPY ('Vis/OBS/'//SS//'/VIS', 'Vis/TMP/'//SS//'/VIS')
      CALL DATLNARR('Vis/OBS/'//SS//'/WT',  'Vis/TMP/'//SS//'/WT')
      CALL CRDGET ('Vis/OBS/'//SS, NAX, TYPE, NAXIS, RVAL,
     $   RPIX, DELT, ROTA)
      NVIS = NAXIS(1)
      CALL CRDPUT ('Vis/TMP/'//SS, NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
      CALL ARRSETCO ('Vis/TMP/'//SS//'/VIS', 0.0, 0.0)
C
C Make image specifications. Remember that the gridding and FFT routines
C are smart enough to figure out what type of 3D transform is needed. All
C we need do here is set up the grids correctly. MVS is for a
C slow transform in Z.
C
      CALL IMGMAKE ('Vis/OBS/'//SS, CELLSIZE, IMSIZE, SHIFT, 'R', 'PSF')
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for central facet', 'I')
      CALL CRDLIST ('PSF')
C
C Only conjugate the first two axes for a slow transform in Z
C
      CALL FFTCONJA ('PSF', 'MVis', DIR, 2)
      IF (ERROR) GO TO 999
C
      IF(CONVFN.EQ.'SF') THEN
         CALL MSGPUT ('Using Spheroidal function convolution', 'I')
      ELSE
         CONVFN = 'BOX'
         CALL MSGPUT ('Using Box-car convolution', 'I')
      END IF
      CALL DATPUTC ('MVis', 'CFTYPE', CONVFN, 1)
      CALL VISTOIMG ('Vis', 'OBS/'//SS, 'PSF', 'MVis', .TRUE.)
      CALL ARRSTAT ('PSF', ' ')
      PSFPEAK = DATFGETR ('PSF', 'ARRMAX')
      IF(ERROR) GO TO 999
      CALL ARRSCALE ('PSF', 1.0/PSFPEAK, 0.0, 'PSF')
C
C Fit a Gaussian Beam to PSF
C
      IF (BEAM(1) .EQ. 0. ) THEN
         CALL IMGBMSHP('PSF')
         IF (ERROR) GO TO 999
         BEAM(1) = DATFGETR('PSF', 'BMAJ')*3600.0
         BEAM(2) = DATFGETR('PSF', 'BMIN')*3600.0
         BEAM(3) = DATFGETR('PSF', 'BPA')
         BEAM(4) = DATFGETR('PSF', 'BZ')*3600.0
      ELSE
         IF(BEAM(2).EQ.0) THEN
            CALL MSGPUT ('Bmin missing - using Bmaj', 'W')
            BEAM(2) = BEAM(1)
         END IF
         IF((BEAM(4).EQ.0.0).AND.THREED) THEN
            CALL MSGPUT ('Bz missing - using Bmaj', 'W')
            BEAM(4) = BEAM(1)
         END IF            
         CALL DATPUTR ('PSF', 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR ('PSF', 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR ('PSF', 'BPA', BEAM(3), 1)
         CALL DATPUTR ('PSF', 'BZ', BEAM(4)/3600.0, 1)
      ENDIF
C
C Find limit to clean in any one cycle.
C
      CALL IMGSUBPS ('PSF', 'SPSF', BP)
      IF(LIMIT.EQ.0.0) THEN
         CALL DATGETR ('SPSF', 'EXTSIDE', LIMIT, 1, NDUMMY)
      ENDIF
      WRITE (MESSAGE, 1105) LIMIT
 1105 FORMAT ('Using limit of ',F7.3, ' of the peak residual')
      IF (ERROR) GO TO 999
      CALL MSGPUT (MESSAGE, 'I')
      IF(ERROR) GO TO 999
C
C Find actual number of patchs
C
      ANPATCH = (2*NPATCH + 1)**2
      WRITE (MESSAGE, 1200) ANPATCH, IMSIZE(1), IMSIZE(2), IMSIZE(3)
 1200 FORMAT ('There will be ',I3,' facets, each of size ',I4,' by ',
     $   I4,' by ',I3)
      CALL MSGPUT (MESSAGE, 'I')
C
C Make master versions of all images. If we are trying to save space
C then we will link the others to these. Only the headers will differ.
C Otherwise we will clone images.
C
      NX = IMSIZE(1)
      NY = IMSIZE(2)
      NZ = IMSIZE(3)
      IF(SAVESPC) THEN
         CALL MSGPUT ('Minimizing space used', 'I')
         WRITE (MESSAGE,1800) 4*NITER+ANPATCH*(2*NX*NY)+
     $      NX*NY*(3*NZ+1)+9*NVIS+3*CCNL
 1800    FORMAT ('Will use about ',I10,' words of memory')
         CALL MSGPUT (MESSAGE, 'I') 
      ELSE
         CALL MSGPUT ('Minimizing CPU used', 'I')
         WRITE (MESSAGE,1800) 4*NITER+ANPATCH*(2*NX*NY+
     $      NX*NY*(3*NZ+1))+9*NVIS+3*CCNL
         CALL MSGPUT (MESSAGE, 'I') 
      END IF
      IF (ERROR) GO TO 999
      CALL DATRENAM ('PSF', 'Dirty')
      NAXIS(1) = CCNL
      CALL DATMAKAR ('Dirty/PIXLIST', 1, NAXIS, 'R', NDUMMY)
      IF(THREED) THEN
         NAXIS(2) = 3
      ELSE
         NAXIS(2) = 2
      ENDIF
      CALL DATMAKAR ('Dirty/PIXLOC', 2, NAXIS, 'I', NDUMMY)
      IF(.NOT.RESTART) THEN
         CALL DATCREAT ('Checkpoint')
         CALL IMGCLONE ('Dirty', 'Checkpoint/Comps')
         NAXIS(1) = NITER
         CALL DATMAKAR ('Checkpoint/Comps/PIXLIST', 1, NAXIS, 'R',
     1      NDUMMY)
         CALL DATMAKAR ('Checkpoint/Comps/PIXFIELD', 1, NAXIS, 'I',
     1      NDUMMY)
         NAXIS(1) = NITER
         IF(THREED) THEN
            NAXIS(2) = 3
         ELSE
            NAXIS(2) = 2
         ENDIF
         CALL DATMAKAR ('Checkpoint/Comps/PIXLOC', 2, NAXIS, 'I',
     1      NDUMMY)
      ENDIF
      IF(THREED) THEN
         CALL IMGDESPH ('Dirty', 'Dirty2D')
         CALL MSGPUT ('Coordinates for 2D Dirty image', 'I')
         CALL CRDLIST ('Dirty2D')
      END IF
C
C Get reference RA and DEC for standard position. We will then offset
C from that. Each image will have the correct coordinates in the header.
C We chose the image spacing so that there are no gaps. This means that
C we must use the lowest absolute value of dec i.e. the edge nearest
C the equator. The pattern of windows is e.g.:
C
C	7	8	9
C	4	5	6
C	1	2	3
C
      IF (NPATCH.NE.0) THEN
         CALL DATGETD ('Dirty', 'CRVAL', RVAL, SYSMXDIM, NRNAX)
         SPACING(1) = CELLSIZE(1) * (IMSIZE(1)-2*BORDER) / 3600.0
         SPACING(2) = CELLSIZE(2) * (IMSIZE(2)-2*BORDER) / 3600.0
         CALL DATGETC ('Dirty', 'CTYPE', ITYPE, SYSMXDIM, NRNAX)
         CALL DATGETC ('MVis', 'CTYPE', VTYPE, SYSMXDIM, VNAX)
         I = STRSEARC('DEC--SIN', ITYPE, SYSMXDIM)
         IF (I.EQ.0) THEN
            IF (ERROR) GO TO 999
            CALL MSGPUT ('Warning: cannot find Dec axis', 'W')
            DEC = 0.0D0
         ELSE
            IF(RVAL(I).GT.0.0) THEN
               DEC = (RVAL(I) - (FLOAT(NPATCH) + 0.5) * SPACING(1))
     $              * D2R
            ELSE
               DEC = (RVAL(I) + (FLOAT(NPATCH) + 0.5) * SPACING(1))
     $              * D2R
            END IF
         END IF
         RASPACE = SPACING(1) / COS(DEC)
         WRITE (MESSAGE, 1005) 3600.0 * RASPACE
 1005    FORMAT ('RA  Spacing of fields = ',F7.1,' arcseconds')
         IF (ERROR) GO TO 999
         CALL MSGPUT (MESSAGE, 'I')
         DECSPACE = ATAN(D2R * SPACING(2)) / D2R
         WRITE (MESSAGE, 1006) 3600.0 * DECSPACE
 1006    FORMAT ('Dec Spacing of fields = ',F7.1,' arcseconds')
         CALL MSGPUT (MESSAGE, 'I')
         IPATCH = 0
         DO 11 IY = -NPATCH, NPATCH
            DO 10 IX = -NPATCH, NPATCH
               IPATCH = IPATCH + 1
               CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
               IF(.NOT.RESTART) THEN
                  CALL IMGLINK ('Checkpoint/Comps', CMP)
                  CALL DATLNARR ('Checkpoint/Comps/PIXLIST',
     $               STRM2(CMP, 'PIXLIST'))
                  CALL DATLNARR ('Checkpoint/Comps/PIXFIELD',
     $               STRM2(CMP, 'PIXFIELD'))
                  CALL DATLNARR ('Checkpoint/Comps/PIXLOC',
     $               STRM2(CMP, 'PIXLOC'))
               ENDIF
               CALL IMGLINK ('MVis', MVS)
               IF(SAVESPC) THEN
                  CALL IMGLINK ('Dirty', DRT)
               ELSE
                  CALL IMGCLONE ('Dirty', DRT)
               END IF
               CALL DATLNARR ('Dirty/PIXLIST', STRM2(DRT, 'PIXLIST'))
               CALL DATLNARR ('Dirty/PIXLOC',  STRM2(DRT, 'PIXLOC'))
               IF(THREED) THEN
                  CALL IMGLINK ('Dirty2D', DRT2D)
               END IF
               CALL DATGETD ('Dirty', 'CRVAL', RVAL, SYSMXDIM, NRNAX)
               IF(THREED) THEN
                  CALL DATGETD (DRT2D, 'CRVAL', RVAL2D, SYSMXDIM,
     $                 NDUMMY)
               ENDIF
               I = STRSEARC('RA---SIN', ITYPE, NRNAX)
               IF (I.EQ.0) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $                 'cannot find RA---SIN axis for image')
                  GO TO 999
               END IF
               RVAL(I) = RVAL(I) + FLOAT(IX) * RASPACE
               RVAL2D(I) = RVAL(I)
               I = STRSEARC('DEC--SIN', ITYPE, NRNAX)
               IF (I.EQ.0) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $                 'cannot find DEC--SIN axis for image')
                  GO TO 999
               END IF
               RVAL(I) = RVAL(I) + FLOAT(IY) * DECSPACE
               RVAL2D(I) = RVAL(I)
               CALL DATPUTD (DRT, 'CRVAL', RVAL, NRNAX)
               IF(.NOT.RESTART) THEN
                  CALL DATPUTD (CMP, 'CRVAL', RVAL, NRNAX)
               END IF
               IF(THREED) THEN
                  CALL DATPUTD (DRT2D, 'CRVAL', RVAL2D, SYSMXDIM)
               ENDIF
               CALL DATGETD (MVS, 'CRVAL', RVAL, SYSMXDIM, NRNAX)
               I = STRSEARC('RA---SIN', VTYPE, NRNAX)
               IF (I.EQ.0) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $                 'cannot find RA---SIN axis for transform')
                  GO TO 999
               END IF
               RVAL(I) = RVAL(I) + FLOAT(IX) * RASPACE
               I = STRSEARC('DEC--SIN', VTYPE, NRNAX)
               IF (I.EQ.0) THEN
                  CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $                 'cannot find DEC--SIN axis for transform')
                  GO TO 999
               END IF
               RVAL(I) = RVAL(I) + FLOAT(IY) * DECSPACE
               CALL DATPUTD (MVS, 'CRVAL', RVAL, NRNAX)
  10        CONTINUE
  11     CONTINUE
      ELSE
         IPATCH = 1
         CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
         IF(.NOT.RESTART) THEN
            CALL IMGLINK ('Checkpoint/Comps', CMP)
         END IF
         CALL DATRENAM ('Dirty', DRT)
         CALL DATRENAM ('MVis', MVS)
         IF(THREED) THEN
            CALL DATRENAM ('Dirty2D', DRT2D)
         ENDIF
      END IF
C
C Do we want to restart from a checkpoint?
C
      IF(RESTART) THEN
         TNITER = 0
         CALL MSGPUT ('Restarting from checkpoint file', 'I')
         CALL DATCREAT ('Checkpoint')
         CALL DATREAD ('Checkpoint', CHPTFILE)
         CALL DATGETAR ('Checkpoint/Comps/PIXLIST', NAX, NAXIS,
     &      ATYPE, NDUMMY)
         IF(NITER.GT.NAXIS(1)) THEN
            CALL MSGPUT ('Increasing number of clean components', 'I')
            CALL ARREXPAN ('Checkpoint/Comps/PIXLIST', 1, NITER)
            CALL ARREXPAN ('Checkpoint/Comps/PIXFIELD', 1, NITER)
            NAXIS(1) = NITER
            IF(THREED) THEN
               NAXIS(2) = 3
            ELSE
               NAXIS(2) = 2
            ENDIF
            CALL ARREXPAN ('Checkpoint/Comps/PIXLOC', 2, NAXIS)
         ENDIF
         IF (ERROR) GOTO 999
         DO 400 IPATCH = 1, ANPATCH
            CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
            CALL FLYSTR(FS, IPATCH, NPATCH)
            IF(DATEXIST(STRM2(CMP,'NITER'))) THEN
               CALL DATGETI (CMP, 'NITER', ANITER, 1, NDUMMY)
               IF(ERROR) GO TO 999
               TNITER = MAX(TNITER, ANITER)
               TNITER=MIN(RITER,TNITER+1,NITER)
               CALL DATPUTI (CMP, 'BITER', 1, 1)
               CALL DATPUTI (CMP, 'FIELD', IPATCH, 1)
               CALL DATPUTI (CMP, 'NITER', TNITER, 1)
               CALL IMGCCIMG (CMP)
               CALL ARRSTAT(CMP, ' ')
               CALL DATGETR (CMP, 'ARRSUM', TFLUX(IPATCH), 1, NDUMMY)
               IF(TFLUX(IPATCH).EQ.0.0) THEN
                  WRITE (MESSAGE, 1036) FS
 1036             FORMAT ('Field ',A,' is empty')
                  CALL MSGPUT (MESSAGE, 'I')
               ELSE
                  WRITE (MESSAGE, 1034) FS, TFLUX(IPATCH)
 1034             FORMAT ('Field ',A,' : previously found flux = ',
     $               1PE12.3)
                  CALL MSGPUT (MESSAGE, 'I')
                  CALL FLYTOVIS('Vis', 'TMP/'//SS, CMP, DRT, MVS, 
     &               DATFGETR (CMP, 'ARRMAX').GT.FSWITCH)
                  CALL ARRSUBTR ('Vis/OBS/'//SS//'/VIS', 
     $                           'Vis/TMP/'//SS//'/VIS',
     $                           'Vis/OBS/'//SS//'/VIS')
               END IF
            ELSE
               WRITE (MESSAGE, 1036) FS
               CALL MSGPUT (MESSAGE, 'I')
            END IF
 400     CONTINUE
         WRITE(MESSAGE,1777) TNITER
 1777    FORMAT ('Restarting clean at component ', I7)
         CALL MSGPUT (MESSAGE, 'I')
      ELSEIF(NPATCH.EQ.0) THEN
         CALL FLYNAMES (1, CMP, DRT, DRT2D, MVS, WIN)
         IF(THREED) THEN
            CALL IMGCLONE (DRT2D, WIN)
         ELSE
            CALL IMGCLONE (DRT, WIN)
         END IF
         CALL ARRSETCO (WIN, 0.0, 1.0)
      ELSE
C
C Find window functions by finding number of times a given location
C is represented in the overall grid. We also need this function
C for adding back the residuals later. WIN is used to post-multiply
C the components after a minor cycle clean. This means that the
C components in the checkpoint file will then be correct. 
C Note that although 3D windows may seem to be needed, 2D will do
C to a good accuracy.
C
         IF (ERROR) GO TO 999
         CALL MSGPUT ('Finding overlap region images', 'I')
         DO 800 IPATCH = 1, ANPATCH
            CALL FLYSTR(FS, IPATCH, NPATCH)
            WRITE (MESSAGE, 1045) FS
 1045       FORMAT ('Field ',A)
            CALL MSGPUT (MESSAGE, 'I')
            CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
            IF(THREED) THEN
               CALL IMGCLONE (DRT2D, WIN)
            ELSE
               CALL IMGCLONE (DRT, WIN)
            END IF
            CALL ARRSETCO (WIN, 0.0, 1.0)
            DO 810 INPATCH = 1, ANPATCH
               IF(.NOT.FLYNEIGH(IPATCH,INPATCH,NPATCH)) GO TO 810
               CALL FLYNAMES (INPATCH, CMPN, DRTN, DRT2DN, MVSN, WINN)  
               SYSMSG=.FALSE.
               IF(THREED) THEN
                  CALL ARRSETCO (DRT2DN, 0.0, 1.0)
                  CALL IMGHGEOM (DRT2DN, WIN, WIN, Interp)
               ELSE
                  CALL ARRSETCO (DRTN, 0.0, 1.0)
                  CALL IMGHGEOM (DRTN, WIN, WIN, Interp)
               ENDIF
               SYSMSG=.TRUE.
 810        CONTINUE
            CALL ARRSTAT (WIN, ' ')
            CALL DATGETR (WIN, 'ARRMAX', ARRMAX, 1, NDUMMY)
            IF(ERROR) GO TO 999
            IF(ARRMAX.EQ.0.0) THEN
               CALL MSGPUT ('No overlap with neighbors', 'W')
            END IF
            CALL ARRCINV (WIN, 0.50, WIN)
            IF (ERROR) GO TO 999
 800     CONTINUE
      END IF
C
C Save the current weights so that we can flag the V axis if
C required
C
      CALL ARRCOPY ('Vis/OBS/'//SS//'/WT', 'Vis/OBS/'//SS//'/SWT')

      IF (NITER.EQ.0) THEN
         CALL MSGPUT ('Making dirty image only', 'I')
         GO TO 101
      END IF
C
C ************************* Start of cycling ************************
C
      NCYCLE = 0
 100  CONTINUE
      IF (ERROR) GO TO 999
C
      NCYCLE = NCYCLE + 1
      WRITE (MESSAGE, 1050) NCYCLE
 1050 FORMAT ('***** Starting cycle ',I4,' *****')
      CALL MSGPUT (MESSAGE, 'I')
C
C Make dirty images from current residual visibilities. Subsection
C and then find maximum residual. 
C
      DO 500 IPATCH = 1, ANPATCH
         CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
         IF(NOV.GT.0.0) THEN
            CALL ARRCOPY ('Vis/OBS/'//SS//'/SWT', 'Vis/OBS/'//SS//'/WT')
            SYSMSG=SYSDEBUG
            CALL VISFLAGV('Vis', 'OBS/'//SS, DRT, NOV)
            SYSMSG=.TRUE.
         ENDIF
         CALL VISTOIMG ('Vis', 'OBS/'//SS, DRT, MVS, MAKEPSF)
         CALL ARRSCALE (DRT, 1.0/PSFPEAK, 0.0, DRT)
         CALL ARRSTAT (DRT, ' ')
         CALL DATGETR (DRT, 'ARRMAX', PMAXRES(IPATCH), 1, NDUMMY)
         CALL DATGETR (DRT, 'ARRMIN', PMINRES(IPATCH), 1, NDUMMY)
         CALL DATGETR (DRT, 'ARRRMS', PRMSRES(IPATCH), 1, NDUMMY)
         IF(ERROR) GO TO 999
         CALL FLYSTR(FS, IPATCH, NPATCH)
         WRITE (MESSAGE, 1025) FS, PMINRES(IPATCH), PMAXRES(IPATCH)
 1025    FORMAT ('Field ',A, ': Min., max. res = ',F10.5,
     $      ' , ',F10.5,' Jy/beam')
         CALL MSGPUT (MESSAGE, 'I')
         IF(TV.EQ.0) THEN
            CALL IMGTVD (DRT, 0.0, 0.0)
         ELSEIF(IPATCH.EQ.TV) THEN
            CALL IMGTVD (DRT, 0.0, 0.0)
         END IF
 500  CONTINUE
C
C Now sort the list of residuals so that we go through the fields
C in order of descending residual
C
      CALL SORTR (PMAXRES, ANPATCH, ORDER)
C
C Now find the clean limit by summing all the sources in all the
C fields in quadrature.
C
      IF(ANPATCH.GT.1) THEN
         MAXARES = 0.0
         SLIM = 0.0
         DO 550 IPATCH = 1, ANPATCH
            MAXARES = MAX(MAXARES, ABS(PMAXRES(IPATCH)),
     $         ABS(PMINRES(IPATCH)))
            SLIM = SLIM + PMAXRES(IPATCH)**2
 550     CONTINUE
         SLIM = LIMIT * MAX(MAXARES, SQRT(SLIM))
      ELSE
         MAXARES = MAX(ABS(PMAXRES(1)), ABS(PMINRES(1)))
         SLIM = LIMIT * MAXARES
      END IF
C
      IF (MAXARES.LT.FLUX) THEN
         CALL MSGPUT('***** Reached stopping point *****', 'I')
         RESCURR=.NOT.SAVESPC
         GO TO 101
      ELSE
         AFLUX = MAX(SLIM, 0.9*FLUX)
         WRITE (MESSAGE, 1020) AFLUX
 1020    FORMAT ('Cleaning down to ',F10.5,' Jy/beam')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Call Minor cycle Clean for each of the patchs and then subtract the
C transform to obtain the residual visibility. 
C
      LNITER=TNITER
      DO 600 IPATCHI = ANPATCH, 1, -1
         IPATCH=ORDER(IPATCHI)
         CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
C
C Only clean this image if the maximum residual is above the
C threshold.
C
         IF (MAX(ABS(PMAXRES(IPATCH)),ABS(PMINRES(IPATCH)))
     $      .LT.AFLUX) THEN
            GO TO 600
         END IF
C
C Must remake dirty image if we are trying to save space
C
         IF(SAVESPC) THEN
            IF(NOV.GT.0.0) THEN
               CALL ARRCOPY ('Vis/OBS/'//SS//'/SWT',
     $            'Vis/OBS/'//SS//'/WT')
               SYSMSG=SYSDEBUG
               CALL VISFLAGV('Vis', 'OBS/'//SS, DRT, NOV)
               SYSMSG=.TRUE.
            ENDIF
            CALL VISTOIMG ('Vis', 'OBS/'//SS, DRT, MVS, MAKEPSF)
            CALL ARRSCALE (DRT, 1.0/PSFPEAK, 0.0, DRT)
         END IF
C
C Do minor cycle clean for this patch. Switch off messages from 
C IMGFCLEA. Set components image to be zero.
C
         CALL DATPUTR (CMP, 'GAIN', GAIN, 1)           
         CALL DATPUTR (CMP, 'SPEED', SPEED, 1)
         CALL DATPUTR (CMP, 'SLIM', SLIM, 1)
         CALL DATPUTR (CMP, 'FLUX', AFLUX, 1)
         BITER=TNITER+1
         CALL DATPUTI (CMP, 'BITER', BITER, 1)
         CALL DATPUTI (CMP, 'GITER', LNITER, 1)
         CALL DATPUTI (CMP, 'NITER', NITER, 1)
         CALL DATPUTI (CMP, 'CCNL', CCNL, 1)
         CALL DATPUTR (CMP, 'TFLUX', 0.0, 1)
         CALL ARRSETCO (CMP, 0.0, 0.0)
         CALL DATPUTI (CMP, 'FIELD', IPATCH, 1)
         SYSMSG = SYSDEBUG
         CALL IMGFCLEA (DRT, 'SPSF', CMP, WIN)
         SYSMSG = .TRUE.
         CALL DATGETI (CMP, 'NITER', TNITER, 1, NDUMMY)
         CALL DATGETR (CMP, 'FLUX', PMAXRES(IPATCH), 1, NDUMMY)
         CALL DATGETR (CMP, 'TFLUX', ITFLUX, 1, NDUMMY)
         IF(ITFLUX.EQ.0.0) GO TO 600
         TFLUX(IPATCH)=TFLUX(IPATCH)+ITFLUX
         CALL FLYSTR(FS, IPATCH, NPATCH)
         WRITE (MESSAGE, 1035) FS, TNITER, PMAXRES(IPATCH),
     $      TFLUX(IPATCH)
 1035    FORMAT ('Field ', A,': ',I6,' comps, Res. = ', F10.5,
     $      ' Flux = ',1PE12.3)
         CALL MSGPUT (MESSAGE, 'I')
C
         IF (TNITER.GE.NITER) GO TO 610
 600  CONTINUE
 610  CONTINUE
C
C Check to see if anything was done
C
      IF (LNITER.EQ.TNITER) THEN
         RESCURR=.NOT.SAVESPC
         CALL MSGPUT ('No new components found - quitting', 'I')
         GO TO 101
      END IF
C
C Now find total flux
C
      TTFLUX = 0.0
      DO 650 IPATCH = 1, ANPATCH
         TTFLUX = TTFLUX + TFLUX(IPATCH)
 650  CONTINUE
C
      WRITE (MESSAGE, 1040) TNITER, TTFLUX
 1040 FORMAT ('All fields: Found ',I6,' comps, Flux = ',1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C Now do uvw subtraction for these components. So BITER so that all
C potential components found in the last loop are examined.
C
      CALL MSGPUT ('Subtracting model from visibilities', 'I')
      IF(NOV.GT.0.0) THEN
         CALL ARRCOPY ('Vis/OBS/'//SS//'/SWT',
     $                 'Vis/OBS/'//SS//'/WT')
      ENDIF
      DO 620 IPATCH = 1, ANPATCH
         CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
         IF(.NOT.DATEXIST(CMP)) GO TO 620
         CALL DATPUTI (CMP, 'FIELD', IPATCH, 1)
         CALL DATPUTI (CMP, 'BITER', LNITER+1, 1)
         CALL DATPUTI (CMP, 'NITER', TNITER, 1)
         CALL IMGCCIMG (CMP)
         CALL ARRSTAT (CMP, ' ')
         IF(DATFGETR (CMP, 'ARRRMS').NE.0.0) THEN
            CALL FLYTOVIS('Vis', 'TMP/'//SS, CMP, DRT, MVS, 
     &         DATFGETR (CMP, 'ARRMAX').GT.FSWITCH)
            CALL ARRSUBTR ('Vis/OBS/'//SS//'/VIS',
     $                     'Vis/TMP/'//SS//'/VIS', 
     1                     'Vis/OBS/'//SS//'/VIS')
            RESCURR=.FALSE.
         END IF
 620  CONTINUE
C
C Checkpoint
C
 102  CONTINUE
      IF(CHPTFILE.NE.' ') THEN
         CALL DATWRITE ('Checkpoint', CHPTFILE)
      END IF
C
C Look to see if there were any interrupts: if there were then change
C some of the inputs
C
      IF (SYSINTRP) THEN
         SYSINTRP = .FALSE.
         IF (SYSINTAC.EQ.'QUIT') THEN
            GO TO 101
         ELSE IF (SYSINTAC.EQ.'INPUTS') THEN
            CALL MSGPUT ('Getting new inputs', 'I')
            CALL USRGETI ('Niter', NITER, 1, NDUMMY)
            CALL USRGETI ('TVImage', TV, 1, NDUMMY)
            CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
            CALL USRGETR ('Flux', FLUX, 1, NDUMMY)
            CALL USRGETR ('Gain', GAIN, 1, NDUMMY)
         END IF
      END IF
C
C Now loop back for more if neccessary
C
      IF (TNITER.LT.NITER) GO TO 100
 101  CONTINUE
C
C  ************************ End of cycle *****************************
C
C Clean up a bit
C
      CALL DATDELET ('Vis/TMP')
C
C Output images: we can change the file names up to this point
C
      CALL USRGETC ('Residual', RESFILE, 1, NDUMMY)
      IF(PSFFILE.NE.' ') THEN
         RESFILE=PSFFILE
      ENDIF
      CALL USRGETC ('CLEAN', SCLNFILE, 1, NDUMMY)
C
C Now calculate residuals. First find output image size. Only remake
C residuals if they are no longer correct.
C
      CALL ARRSETCO ('Clean', 0.0, 0.0)
      CALL MSGPUT ('Making residual image', 'I')
      DO 700 IPATCH = 1, ANPATCH
         CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
         IF(.NOT.RESCURR) THEN
            IF(NOV.GT.0.0) THEN
               CALL ARRCOPY ('Vis/OBS/'//SS//'/SWT',
     $            'Vis/OBS/'//SS//'/WT')
               SYSMSG=SYSDEBUG
               CALL VISFLAGV('Vis', 'OBS/'//SS, DRT, NOV)
               SYSMSG=.TRUE.
            ENDIF
            CALL VISTOIMG ('Vis', 'OBS/'//SS, DRT, MVS, MAKEPSF)
            CALL ARRSCALE (DRT, 1.0/PSFPEAK, 0.0, DRT)
         ENDIF
         CALL ARRSTAT (DRT, ' ')
         CALL DATGETR (DRT, 'ARRMAX', PMAXRES(IPATCH), 1, NDUMMY)
         CALL DATGETR (DRT, 'ARRMIN', PMINRES(IPATCH), 1, NDUMMY)
         IF(ERROR) GO TO 999
         CALL FLYSTR(FS, IPATCH, NPATCH)
         WRITE (MESSAGE, 1025) FS, PMINRES(IPATCH), PMAXRES(IPATCH)
         CALL MSGPUT (MESSAGE, 'I')
         CALL ARRMULT (DRT, WIN, DRT)
         CALL DATDELET (WIN)
         IF(THREED) THEN
            CALL IMGDESPH (DRT, DRT2D)
            CALL IMGHGEOM (DRT2D, 'Clean', 'Clean', Interp)
         ELSE
            CALL IMGHGEOM (DRT, 'Clean', 'Clean', Interp)
         ENDIF
 700  CONTINUE
C
      TTFLUX = 0.0
      DO 710 IPATCH = 1, ANPATCH
         TTFLUX = TTFLUX + TFLUX(IPATCH)
 710  CONTINUE
C
      CALL HISOPEN ('Clean')
      CALL HISINPUT ('Clean')
      WRITE (MESSAGE, 1040) TNITER, TTFLUX
      CALL HISPUT ('Clean','/'//MESSAGE)
      IF (RESFILE.NE.' ') THEN
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Clean', RESFILE, ' ')
      END IF
C 
C Clean images can simply be added
C
      IF ((SCLNFILE.NE.' ').AND.(NITER.GT.0)) THEN
         CALL MSGPUT ('Restoring clean components', 'I')
         DO 750 IPATCH = 1, ANPATCH
            CALL FLYNAMES (IPATCH, CMP, DRT, DRT2D, MVS, WIN)
            IF ((TFLUX(IPATCH).NE.0.0).AND.DATEXIST(CMP)) THEN
               CALL DATPUTR (CMP, 'TFLUX', TFLUX(IPATCH), 1)
               CALL FLYSTR(FS, IPATCH, NPATCH)
               WRITE (MESSAGE, 1037) FS, PMAXRES(IPATCH), TFLUX(IPATCH)
 1037          FORMAT ('Field ',A,': Res. = ', F10.5, ', Flux = ',
     $            1PE12.3)
               CALL MSGPUT (MESSAGE, 'I')
               CALL DATPUTI (CMP, 'BITER', 1, 1)
               CALL DATPUTI (CMP, 'NITER', TNITER, 1)
               CALL DATPUTI (CMP, 'FIELD', IPATCH, 1)
               CALL IMGCCIMG (CMP)
               CALL IMGSMOOT (CMP, BEAM, DRT, MVS)
               CALL IMGP2PB (DRT, BEAM, DRT)
               IF(THREED) THEN
                  CALL IMGDESPH (DRT, DRT2D)
                  CALL IMGHGEOM (DRT2D, 'Clean', 'Clean', Interp)
               ELSE
                  CALL IMGHGEOM (DRT, 'Clean', 'Clean', Interp)
               ENDIF
            END IF
            IF(ERROR) GO TO 999
 750     CONTINUE
         CALL DATPUTC ('Clean', 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTR ('Clean', 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR ('Clean', 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR ('Clean', 'BPA', BEAM(3), 1)
         CALL DATPUTR ('Clean', 'BZ', BEAM(4)/3600.0, 1)
         CALL FILIMGPU ('Clean', SCLNFILE, ' ')
      END IF
C
 999  CONTINUE
      END
C++
      LOGICAL FUNCTION FLYNEIGH (I1, I2, NPATCH)
C
C Check if patchs are neighbors. Note that a patch is its own
C neighbor.
C
C	I1	INT	input	Patch number
C	I2	INT	input	Patch number
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYNEIGH')
      INTEGER	I1, I2, NPATCH
      INTEGER	X1, X2, Y1, Y2
C==================================================================
C
      X1 = MOD(I1-1, (2*NPATCH+1))+1
      Y1 = (I1-X1)/(2*NPATCH+1)+1
      X2 = MOD(I2-1, (2*NPATCH+1))+1
      Y2 = (I2-X2)/(2*NPATCH+1)+1
      FLYNEIGH=(I1.NE.I2).AND.(ABS(X1-X2).LE.1).AND.(ABS(Y1-Y2).LE.1)
C
      RETURN
      END
C++
      SUBROUTINE FLYSTR (FS, I, NPATCH)
C
C Returns character string containing coordinates of this patch
C relative to the center.
C
C	I1	INT	input	Patch number
C	I2	INT	input	Patch number
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	FS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSTR')
      INTEGER	I, NPATCH
      INTEGER	X, Y
C==================================================================
C
      X = MOD(I-1, (2*NPATCH+1))+1
      Y = (I-X)/(2*NPATCH+1)+1
      WRITE(FS, 1000) I, X-(NPATCH+1), Y-(NPATCH+1)
 1000 FORMAT (I3,' (',I3,',',I3,')')
C
      END
C++
      SUBROUTINE FLYNAMES (I, CMP, DRT, DRT2D, MVS, WIN)
C
C	I	INT	input	Patch number
C	CMP	CHAR	output	Name of components image for this patch
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYNAMES')
      INTEGER	I
      CHARACTER*(*)	CMP, DRT, DRT2D, MVS, WIN
      CHARACTER*6	STRINT
C
C==================================================================
C
      CMP = 'Checkpoint/Comps'//STRINT(I)
      MVS = 'MVis'//STRINT(I)
      DRT = 'Dirty'//STRINT(I)
      DRT2D = 'Dirty2D'//STRINT(I)
      WIN = 'Checkpoint/Win'//STRINT(I)
C
      END
C++
      SUBROUTINE IMGFCLEA (DRT, PSF, CLN, WIN)
C
C Do limited minor cycle clean only. This is called by fly.
C
C	DRT	CH*(*)	input	Name of Dirty image
C	PSF	CH*(*)	input	Name of Point Spread Function
C	CLN	CH*(*)	input	Name of CLN image
C	WIN	CH*(*)	input	Name of window image
C
C Audit trail:
C	Removed redundant MSGPUTs (This is turned off by fly) and
C	also removed initial calculation of MAXRES since this should
C	be passed down. Also initialized TFLUX correctly.
C					T.J. Cornwell	Feb 24 1990
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, PSF, CLN, WIN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGFCLEA')
C
      REAL		GAIN, FLUX, MAXRES, TFLUX, SLIM, ASLIM, TTFLUX,
     $			SPEED
      INTEGER		NITER, BITER, ANITER, FN, GITER
      CHARACTER*1	DATYPE, PATYPE, CATYPE, WATYPE
      INTEGER		DNAX, DADD, DNAXIS(SYSMXDIM)
      INTEGER		PNAX, PADD, PNAXIS(SYSMXDIM)
      INTEGER		WNAX, WADD, WNAXIS(SYSMXDIM)
      INTEGER		CNAX, CADD, CNAXIS(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM)
      INTEGER		CLADD, CXLADD, CYLADD, CZLADD, CFLADD
      INTEGER		DLADD, DXLADD, DYLADD, DZLADD
      INTEGER		IAX, NREAL, NDUMMY, NL, ANL, NAX
      LOGICAL		DATEXIST
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      IF (DATEXIST (STRM2 (CLN, 'CCNL'))) THEN
         CALL DATGETI (CLN, 'CCNL', NL, 1, NDUMMY)
      ELSE
         NL = 16384
      END IF
      IF (DATEXIST(STRM2(CLN, 'FIELD'))) THEN
         CALL DATGETI(CLN, 'FIELD', FN, 1, NDUMMY)
      ELSE
         FN = 1
         CALL DATPUTI(CLN, 'FIELD', FN, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'NITER'))) THEN
         CALL DATGETI(CLN, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 100
         CALL DATPUTI(CLN, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'BITER'))) THEN
         CALL DATGETI(CLN, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(CLN, 'BITER', BITER, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'GITER'))) THEN
         CALL DATGETI(CLN, 'GITER', GITER, 1, NDUMMY)
      ELSE
         GITER = BITER
         CALL DATPUTI(CLN, 'GITER', GITER, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'GAIN'))) THEN
         CALL DATGETR(CLN, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
         CALL DATPUTR(CLN, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'FLUX'))) THEN
         CALL DATGETR(CLN, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(CLN, 'FLUX', FLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'TFLUX'))) THEN
         CALL DATGETR(CLN, 'TFLUX', TFLUX, 1, NDUMMY)
      ELSE
         TFLUX = 0.0
         CALL DATPUTR(CLN, 'TFLUX', TFLUX, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'SLIM'))) THEN
         CALL DATGETR(CLN, 'SLIM', SLIM, 1, NDUMMY)
      ELSE
         SLIM = 0.0
         CALL DATPUTR(CLN, 'SLIM', SLIM, 1)
      END IF
      IF (DATEXIST(STRM2(CLN, 'SPEED'))) THEN
         CALL DATGETR(CLN, 'SPEED', SPEED, 1, NDUMMY)
      ELSE
         SPEED = 1.0
         CALL DATPUTR(CLN, 'SPEED', SPEED, 1)
      END IF
C
C Make lists
C
      CALL DATGETAR (DRT, DNAX, DNAXIS, DATYPE, DADD)
      IF (ERROR) GO TO 990
      IF (DATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//DATYPE//' for Dirty Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (PSF, PNAX, PNAXIS, PATYPE, PADD)
      IF (ERROR) GO TO 990
      IF (PATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//PATYPE//' for PSF')
         GO TO 990
      END IF
C
      CALL DATGETAR (CLN, CNAX, CNAXIS, CATYPE, CADD)
      IF (ERROR) GO TO 990
      IF (CATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//CATYPE//' for CLEAN Image')
         GO TO 990
      END IF
C
      CALL DATGETAR (WIN, WNAX, WNAXIS, WATYPE, WADD)
      IF (ERROR) GO TO 990
      IF (WATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//WATYPE//' for window Image')
         GO TO 990
      END IF
C
      NREAL = 0
      DO 10 IAX = 1, SYSMXDIM
         IF (CNAXIS(IAX).NE.DNAXIS(IAX)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Clean and Dirty Axes disagree')
            GO TO 990
         END IF
         IF (DNAXIS(IAX).GT.1) THEN
            NREAL = NREAL + 1
         ELSE
            GO TO 11
         END IF
  10  CONTINUE
  11  CONTINUE
C
C Make locations for components list
C
      IF (.NOT.DATEXIST (STRM2(CLN, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = ABS(NITER)
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         ATYPE = 'I'
         CALL DATMAKAR (STRM2(CLN, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(CLN, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     CLADD)
         CALL DATGETAR (STRM2(CLN, 'PIXFIELD'), NAX, NAXIS, ATYPE,
     1     CFLADD)
         CALL DATGETAR (STRM2(CLN, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     CXLADD)
         CYLADD = CXLADD + NAXIS(1)
         CZLADD = CXLADD + 2 * NAXIS(1)
      END IF
C
C Now make locations for residual list
C
      IF (.NOT.DATEXIST (STRM2(DRT, 'PIXLIST'))) THEN
         NAX = 1
         NAXIS(1) = NL
         ATYPE = 'R'
         CALL DATMAKAR (STRM2(DRT, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     DLADD)
         NAX = 2
         ATYPE = 'I'
         NAXIS(2) = NREAL
         CALL DATMAKAR (STRM2(DRT, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     DXLADD)
         DYLADD = DXLADD + NAXIS(1)
         DZLADD = DXLADD + 2 * NAXIS(1)
      ELSE
         CALL DATGETAR (STRM2(DRT, 'PIXLIST'), NAX, NAXIS, ATYPE,
     1     DLADD)
         CALL DATGETAR (STRM2(DRT, 'PIXLOC'), NAX, NAXIS, ATYPE,
     1     DXLADD)
         DYLADD = DXLADD + NAXIS(1)
         DZLADD = DXLADD + 2 * NAXIS(1)
      END IF
C
C Construct histogram and find limiting value such that no more than
C NL points will be selected. Then ensure that this limit is no
C lower than the flux limit passed.
C
      CALL ARRABSHI (DRT, STRM2(DRT, 'ABSHIST'))
      CALL ARRFLIM (STRM2(DRT, 'ABSHIST'), NL, ASLIM)
      IF (ASLIM.LT.0.0) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $        'Limit in absolute value < 0.0')
         GO TO 999
      END IF
C
C This next line is changed for multi-field cleans. We need to
C set SLIM based upon the FLUX limit for all fields which is what is
C passed to us.
C
      SLIM = MAX(SLIM, ASLIM)
C
C Finally do something
C
      IF (NREAL.EQ.2) THEN
C
C Construct the list of pixels having values > SLIM. ANL is the actual
C number of such values.
C
         CALL PIX2DRLI (MEMR(DADD), DNAXIS(1), DNAXIS(2), SLIM, 
     1      MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD), NL, ANL)
         IF (ERROR) GO TO 990
C
C Now do minor cycle clean on the list of residuals stopping at the greater
C of the specified flux limit or the cutoff in residuals. We must pass
C both FLUX and SLIM since the latter must be used to limit the
C number of clean components found.
C
         CALL PIX2DRCC (MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD),
     1      ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), GAIN, NITER, 
     2      BITER, GITER, FLUX, SPEED, SLIM, MEMR(CLADD), MEMI(CXLADD), 
     3      MEMI(CYLADD), MEMI(CFLADD), FN, ANITER, MAXRES, TTFLUX)
C
C Apply postclean window to compensate for any overlapping regions
C
         CALL PIX2DRMW (MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), 
     1      BITER, ANITER, MEMR(WADD), WNAXIS(1), WNAXIS(2), TFLUX)
C
C Convert the list of clean components into an image (we actually
C add the specified clean components to the current image)
C
         CALL PIX2DRMA (MEMR(CLADD), MEMI(CFLADD), FN,
     $      MEMI(CXLADD), MEMI(CYLADD), 
     1      BITER, ANITER, MEMR(CADD), CNAXIS(1), CNAXIS(2))
      ELSE IF (NREAL.EQ.3) THEN
         CALL PIX3DRLI (MEMR(DADD), DNAXIS(1), DNAXIS(2), 
     1      DNAXIS(3), SLIM, MEMR(DLADD), MEMI(DXLADD), 
     2      MEMI(DYLADD), MEMI(DZLADD), NL, ANL)
         CALL PIX3DRCC (MEMR(DLADD), MEMI(DXLADD), MEMI(DYLADD),
     1      MEMI(DZLADD), ANL, MEMR(PADD), PNAXIS(1), PNAXIS(2), 
     2      PNAXIS(3), GAIN, NITER, BITER, GITER, FLUX, SPEED, SLIM, 
     3      MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), MEMI(CZLADD), 
     4      MEMI(CFLADD), FN, ANITER, MAXRES, TTFLUX)
         CALL PIX2DRMW (MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), 
     1      BITER, ANITER, MEMR(WADD), WNAXIS(1),
     $      WNAXIS(2), TFLUX)
         CALL PIX3DRMA (MEMR(CLADD), MEMI(CXLADD), MEMI(CYLADD), 
     1      MEMI(CZLADD), BITER, ANITER, MEMR(CADD), CNAXIS(1), 
     2      CNAXIS(2), CNAXIS(3))
      ELSE
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'Illegal dimension')
         GO TO 999
      END IF
C
C Store goodies
C
      CALL DATPUTR (CLN, 'FLUX', MAXRES, 1)
      CALL DATPUTR (CLN, 'TFLUX', TFLUX, 1)
      CALL DATPUTI (CLN, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
