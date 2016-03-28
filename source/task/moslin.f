C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moslin.f	1.4    11/13/90
C
      SUBROUTINE SDEMAIN
C
#define ntel 20
C
CD Program to perform linear mosaicing
C
C Audit trail:
C	The pointing center of 'Denom' and 'PBFlat' were not set
C	correctly
C				T.J.Cornwell	Jan 25 1989
C	Weight by number of visibility points. Add user-specified
C	clipping.
C				T.J.Cornwell	Jan 26 1989
C	Changed to VISMOSPU etc.
C				T.J.Cornwell	Feb 3 1989
C	Added capability to generate nice PSF by minimizing the
C	sidelobes inside the primary beam
C				T.J.Cornwell	March 18 1990
C	Added DFT list
C				T.J.Cornwell	April 17 1990
C	Added TELDIAM
C				M.A.Holdaway	Nov 13 1990
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSLIN')
C
      INTEGER		NDUMMY,  NDFT
      REAL		CELLSIZE(3), TAPER(3), 
     1			TIME(2), UVLIMITS(2), SHIFT(3)
      INTEGER 		DIR, NSEL, TIMR(8), IMSIZE(3), NPC, IPC,
     1			FFTPWR2, PIMSIZE(3), STRSEARC,
     2			BLC (SYSMXDIM), TRC (SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CTIME, SUBCLASS, PSUBCLAS
      REAL		FOV, WEIGHT, SUMWT, SSUMWT, CLIP, MAXSEN, PBP
      DOUBLE PRECISION  RVAL(SYSMXDIM), OBSRA, OBSDEC
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	IMGFILE, VISFILE, VIS, MOSFILE,
     $			SENSFILE, PSFFILE
      LOGICAL		IMGUNI, NICEPSF
      LOGICAL		T, F, DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2, TELESCOP, DFT(ntel)
      REAL		TELDIAM
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
      DATA		BLC	/SYSMXDIM*1/
      DATA		TRC	/SYSMXDIM*1/
C==================================================================
      CALL MSGWELCO ('I perform linear mosaicing')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 3, NDUMMY)
      CALL USRGETR ('FOV', FOV, 1, NDUMMY)
      CALL USRGETR ('MinPB', CLIP, 1, NDUMMY)
      CALL USRGETL ('Equalize', IMGUNI, 1, NDUMMY)
      CALL USRGETL ('NicePSF', NICEPSF, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Filter', TAPER, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('Sens', SENSFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('DFT', DFT, ntel, NDFT)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Subclass to be gridded: can only deconvolve Stokes I
C
      SUBCLASS = 'OBS/I'
      PSUBCLAS = 'POINT/I'
C
C Now get relevant files
C
      CALL DATCREAT ('M')
      IF (MOSFILE.NE.' ') THEN
         CALL MSGPUT ('Reading Mosaic visibility file', 'I')
         CALL VISMOSGE ('M', MOSFILE)
         CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
         IF (NPC.EQ.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'No pointings')
            GO TO 999
         ELSE
         WRITE (MESSAGE, 1050) NPC
 1050       FORMAT ('There are ',I4,' pointings')
            CALL MSGPUT (MESSAGE, 'I')
         END IF
      ELSE IF (VISFILE.NE.' ') THEN
         CALL MSGPUT ('Reading standard visibility file', 'I')
         NPC = 1
         CALL DATPUTI ('M', 'NPC', NPC, 1)
         CALL VISGET ('M/PC1', VISFILE, 'I', '*', ' ')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'You must specify an input file via Vis or Mosaic')
         GO TO 999
      END IF
C
C Now select data
C
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      DO 1 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL VISSEL (VIS, SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) IPC, NSEL
 1000    FORMAT ('Pointing ',I4,' : selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
   1  CONTINUE
      CALL SYSETIME (CTIME)
      CALL MSGPUT ('Got data: '//CTIME, 'I')
C
C Make the output image
C
      CALL IMGMAKE ('M/PC1/'//SUBCLASS, CELLSIZE, IMSIZE, SHIFT,
     1   'R', 'Image')
      CALL IMGCLONE ('Image', 'PSF')
C
C Find dual image
C
      PIMSIZE(1) = FFTPWR2(IMSIZE(1))
      PIMSIZE(2) = FFTPWR2(IMSIZE(2))
      PIMSIZE(3) = 1
      CALL IMGMAKE (STRM2('M/PC1', SUBCLASS), CELLSIZE, PIMSIZE, 
     1   SHIFT, 'R', 'TImage')
      CALL FFTCONJA ('TImage', 'Grid', DIR, 0)
      IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for Image:', 'I')
      CALL CRDLIST ('Image')
         IF (ERROR) GO TO 999
      CALL MSGPUT ('Coordinates for transform of Image:', 'I')
      CALL CRDLIST ('Grid')
      IF (ERROR) GO TO 999
C
C Now do uniform weighting if required
C
      IF (NICEPSF.OR.(FOV.NE.0.0)) THEN
         CALL MSGPUT ('Reweighting data', 'I')
         DO 2 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            IF(NICEPSF) THEN
               CALL IMGMSPSF (VIS, 'OBS/I', 'Grid', 'PSF')
            ELSE
               CALL DATPUTR ('Grid', 'WTFOV', FOV, 1)
               CALL GRDUWT (VIS, 'OBS/I', 'Grid')
            ENDIF
  2      CONTINUE
         IF (ERROR) GO TO 999
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Uniform weighting of visibilities: '//CTIME, 
     1      'I')
      END IF
C
C Apply taper
C
      IF ((TAPER(1).NE.0.0).OR.(TAPER(2).NE.0.0)) THEN
         DO 3 IPC = 1, NPC
            VIS = 'M/PC'//STRINT(IPC)
            CALL VISTAPER (VIS, SUBCLASS, TAPER, SUBCLASS)
  3      CONTINUE
         CALL SYSETIME (CTIME)
         CALL MSGPUT ('Filtering: '//CTIME, 'I')
      END IF
C
C Write messages
C
      IF (IMGUNI) THEN
         CALL MSGPUT (
     1      'Dirty images added with equal weight', 'I')
      ELSE
         CALL MSGPUT (
     1      'Dirty images weighted by number of visibilities', 'I')
      END IF
      WRITE (MESSAGE, 1300) CLIP
 1300 FORMAT ('Clipping at fraction = ',F6.3,
     1   ' of effective sensitivity function')
      CALL MSGPUT (MESSAGE, 'I')
C
C Clone some stuff
C
      CALL IMGCLONE ('Image', 'Util')
      CALL IMGCLONE ('Image', 'Flat')
      CALL IMGCLONE ('Image', 'Denom')
C
C Make the dirty maps and XFRs
C
      CALL MSGPUT ('Making dirty images', 'I')
C
C Make the sub-sectioning window
C
      CALL DATCREAT ('Window')
      BLC(1) = PIMSIZE(1)/2 - IMSIZE(1) / 2 + 1
      BLC(2) = PIMSIZE(2)/2 - IMSIZE(2) / 2 + 1
      TRC(1) = PIMSIZE(1)/2 + IMSIZE(1) / 2 
      TRC(2) = PIMSIZE(2)/2 + IMSIZE(2) / 2 
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
C
C Initialize images
C
      CALL ARRSETCO ('Image', 0.0, 0.0)
      CALL ARRSETCO ('Denom', 0.0, 0.0)
      CALL ARRSETCO ('PSF', 0.0, 0.0)
      CALL ARRSETCO ('Flat', 0.0, 1.0)
      CALL IMGCLONE ('Image', 'Point')
      CALL IMGMODEL ('Point', 1, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     $      'POIN')
C
C Loop over all pointings
C
      SSUMWT = 0.0
      DO 7 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         IF (DATEXIST (STRM2(VIS, 'TELDIAM'))) THEN
            CALL DATGETR (VIS, 'TELDIAM', TELDIAM, 1, NDUMMY)
         ELSE
            TELDIAM = 0.0
         ENDIF
         CALL DATPUTD ('Util', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Util', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC ('Util', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTD ('Flat', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Flat', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC ('Flat', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTD ('Point', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Point', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC ('Point', 'TELESCOP', TELESCOP, 1)
         IF (TELDIAM .GT. 0.0) THEN
            CALL DATPUTR ('Util', 'TELDIAM', TELDIAM, 1)
            CALL DATPUTR ('Flat', 'TELDIAM', TELDIAM, 1)
            CALL DATPUTR ('Point', 'TELDIAM', TELDIAM, 1)
         ENDIF
         IF(STRSEARC(TELESCOP,DFT,NDFT).GT.0) THEN
            CALL MSGPUT ('Using DFT for telescope '//TELESCOP, 'I')
            CALL VISDFTPB (VIS, SUBCLASS, 'Util')
            IF (IMGUNI) THEN
               WEIGHT = 1.0
            ELSE
               CALL DATGETR ('Util', 'SUMWT', SUMWT, 1, NDUMMY)
               WEIGHT = SUMWT
            END IF
            SSUMWT = SSUMWT + WEIGHT
         ELSE
            CALL VISGRID (VIS, SUBCLASS, 'Grid', .FALSE.)
            IF (IMGUNI) THEN
               WEIGHT = 1.0
            ELSE
               CALL DATGETR ('Grid', 'SUMWT', SUMWT, 1, NDUMMY)
               WEIGHT = SUMWT
            END IF
            SSUMWT = SSUMWT + WEIGHT
            CALL IMGFFT ('Grid', 'TImage')
            CALL IMGGRIDC ('TImage', 'TImage', 'CORRECT')
            CALL IMGSUBSE ('TImage', 'Util', 'Window')
            CALL IMGPB ('Util', 'Util', 'APPLY')
         END IF
C
C Find the quotient: the sum of the dirty images weighted by the
C primary beams and by the number of visibility points
C
         CALL ARRLC ('Image', 1.0, 'Util', WEIGHT, 'Image')
         CALL IMGPB ('Flat', 'Util', 'APPLY')
         CALL IMGPB ('Util', 'Util', 'APPLY')
         CALL ARRLC ('Denom', 1.0, 'Util', WEIGHT, 'Denom')
C
C Now make the PSF and multiply by the PB. For the weight, we need
C the value of the primary beam for this pointing at the phase
C center. First find this:
C
         CALL IMGPB ('Point', 'Util', 'APPLY')
         CALL ARRSTAT  ('Util', ' ')
         CALL DATGETR ('Util', 'ARRMAX', PBP, 1, NDUMMY)
         IF(ERROR) GO TO 999
C
C Now actually make the PSF
C
         IF(STRSEARC(TELESCOP,DFT,NDFT).GT.0) THEN
            CALL VISPOINT (VIS, SUBCLASS, PSUBCLAS, 1.0)
            CALL VISDFTPB (VIS, PSUBCLAS, 'Util')
            WEIGHT = 1.0
            SSUMWT = SSUMWT + WEIGHT
         ELSE
            CALL VISGRID (VIS, SUBCLASS, 'Grid', .TRUE.)
            CALL IMGFFT ('Grid', 'TImage')
            CALL IMGGRIDC ('TImage', 'TImage', 'CORRECT')
            CALL IMGSUBSE ('TImage', 'Util', 'Window')
            CALL IMGPB ('Util', 'Util', 'APPLY')
         END IF
         CALL ARRLC ('PSF', 1.0, 'Util', WEIGHT*PBP, 'PSF')
         IF(ERROR) GO TO 999
C
C Write out sign of progress
C
         WRITE (MESSAGE, 1200) IPC, PBP
 1200    FORMAT ('Made pointing ',I4,' Primary beam weight = ', F7.3)
         CALL MSGPUT (MESSAGE, 'I')
C
  7   CONTINUE
C
C Divide to get the final image
C
      CALL ARRSTAT ('Denom', ' ')
      CALL DATGETR ('Denom', 'ARRMAX', MAXSEN, 1, NDUMMY)
      IF(ERROR) GO TO 999
      IF(MAXSEN.EQ.0.0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'Zero sensitivity')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1100) MAXSEN
 1100    FORMAT  ('Maximum sensitivity = ',F7.3,' Primary beam units')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      CALL ARRCDIV ('Image', 'Denom', CLIP*MAXSEN, 'Image')
      CALL ARRCDIV ('PSF', 'Denom', CLIP*MAXSEN, 'PSF')
C
      IF (IMGFILE.NE.' ' ) THEN         
         CALL HISOPEN ('Image')
         CALL HISINPUT ('Image')
         CALL DATGETD ('Image', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('Image', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('Image', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('Image', 'BUNIT', 'JY/BEAM', 1)
         CALL FILIMGPU ('Image', IMGFILE, ' ')
      END IF
C
      IF (PSFFILE.NE.' ' ) THEN         
         CALL HISOPEN ('PSF')
         CALL HISINPUT ('PSF')
         CALL FILIMGPU ('PSF', PSFFILE, ' ')
      END IF
C
      IF (SENSFILE.NE.' ') THEN
         CALL DATRENAM ('Denom', 'Sens')
         CALL HISOPEN ('Sens')
         CALL HISINPUT ('Sens')
         CALL DATGETD ('Sens', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
         CALL DATPUTD ('Sens', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('Sens', 'OBSDEC', RVAL(2), 1)
         CALL DATPUTC ('Sens', 'BUNIT', 'WEIGHT', 1)
         CALL FILIMGPU ('Sens', SENSFILE, ' ')
      END IF
C
 999  CONTINUE
      END
