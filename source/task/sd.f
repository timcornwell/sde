C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sd.f	1.4    3/25/93
C
      SUBROUTINE SDEMAIN
C
CD Program to perform single dish imaging
C
C Audit trail:
C	Added TELDIAM
C				M.A. Holdaway	April 25 1991
C	Changed TELDIAM logic (now there are AIRY, AIRYB, GAUS TELETYPES
C	which are all modified by TELDIAM; we are now flexible)
C				M.A.Holdaway	March 25 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SD')
C
      INTEGER		NDUMMY
      REAL		CELLSIZE(3), TIME(2), SUM, DATFGETR
      INTEGER 		NSEL, TIMR(8), IMSIZE(3), NPC, IPC
      CHARACTER*(SYSMXNAM)	CTIME, SUBCLASS
      REAL		TELDIAM
      REAL		WEIGHT, SSUMWT, UVLIMITS(2), SHIFT(3)
      REAL		WORLD(SYSMXDIM), PIXEL(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM), OBSRA, OBSDEC
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	IMGFILE, VIS, MOSFILE, PSFFILE, TELE,
     $				MODE
      LOGICAL		T, F, DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*(SYSMXNAM)	TELESCOP
      DATA		T	/.TRUE./
      DATA		F	/.FALSE./
      DATA		UVLIMITS	/0.0, 1E-8/
      DATA		SHIFT		/3 * 0.0/
      DATA		PIXEL		/SYSMXDIM * 0.0/
      DATA		WORLD		/SYSMXDIM * 0.0/
C==================================================================
      CALL MSGWELCO ('I perform single dish imaging')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Mosaic', MOSFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC ('Telescope', TELE, 1, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Subclass to be gridded: can only deconvolve Stokes I
C
      SUBCLASS = 'OBS/I'
C
C Now get relevant files
C
      CALL DATCREAT ('M')
      CALL MSGPUT ('Reading Mosaic visibility file', 'I')
      CALL VISMOSGE ('M', MOSFILE)
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      IF (NPC.EQ.0) THEN
         CALL ERRREPOR (ERRNTFND, ROUTINE, 'No pointings')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1050) NPC
 1050    FORMAT ('There are ',I4,' pointings')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C What type of image?
C
      IF(MODE.EQ.'OPTIMAL') THEN
         CALL MSGPUT ('Optimal image produced', 'I')
      ELSE
         CALL MSGPUT ('Standard image produced', 'I')
      END IF
C
C Now select data
C
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      IF(ERROR) GO TO 999
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
C
      CALL MSGPUT ('Coordinates for Image:', 'I')
      CALL CRDLIST ('Image')
C
C Write messages
C
      CALL MSGPUT ('Pointings added with equal weight', 'I')
C
C Clone some stuff
C
      CALL IMGCLONE ('Image', 'Util')
C
C Initialize images
C
      CALL ARRSETCO ('Image', 0.0, 0.0)
C
C Loop over all pointings
C
      IF(ERROR) GO TO 999
      SSUMWT = 0.0
      DO 7 IPC = 1, NPC
         WRITE (MESSAGE, 1100) IPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         IF ((TELE.NE.' ').AND.(TELESCOP.NE.TELE)) GO TO 7
 1100    FORMAT ('Making image for pointing ',I4)
         CALL MSGPUT (MESSAGE, 'I')
C
C Make sure that all the images have the correct pointing center
C
         CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATPUTD ('Util', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Util', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC ('Util', 'TELESCOP', TELESCOP, 1)
         TELDIAM = 0.0
         IF (DATEXIST(STRM2(VIS, 'TELDIAM'))) THEN
            CALL DATGETR (VIS, 'TELDIAM', TELDIAM, 1, NDUMMY)
            CALL DATPUTR ('Util', 'TELDIAM', TELDIAM, 1)
         ENDIF
         IF(MODE.EQ.'OPTIMAL') THEN
            CALL VISDFTPB(VIS, SUBCLASS, 'Util')
         ELSE
            CALL VISSD(VIS, SUBCLASS, 'Util')
         END IF
         WEIGHT = 1.0
         SSUMWT = SSUMWT + WEIGHT
         CALL ARRLC ('Image', 1.0, 'Util', WEIGHT, 'Image')
 7    CONTINUE
      CALL DATDELET ('Util')
C
      CALL HISOPEN ('Image')
      CALL HISINPUT ('Image')
      CALL DATGETD ('Image', 'CRVAL', RVAL, SYSMXDIM, NDUMMY)
      CALL DATPUTD ('Image', 'OBSRA', RVAL(1), 1)
      CALL DATPUTD ('Image', 'OBSDEC', RVAL(2), 1)
      CALL DATPUTC ('Image', 'TELESCOP', TELESCOP, 1)
      IF (TELDIAM .GT. 0.0) THEN
         CALL DATPUTR ('Image', 'TELDIAM', TELDIAM, 1)
      ENDIF
      CALL DATPUTC ('Image', 'BUNIT', 'JY/BEAM', 1)
      CALL IMGCLONE ('Image', 'PSF')
      PIXEL(1) = FLOAT(IMSIZE(1)) / 2.0
      PIXEL(2) = FLOAT(IMSIZE(2)) / 2.0
      CALL CRDPTOW ('PSF', PIXEL, WORLD)
      CALL DATPUTD ('PSF', 'OBSRA', DBLE(WORLD(1)), 1)
      CALL DATPUTD ('PSF', 'OBSDEC', DBLE(WORLD(2)), 1)
      CALL DATPUTC ('PSF', 'BUNIT', 'JY/BEAM', 1)
      CALL DATPUTC ('PSF', 'TELESCOP', TELESCOP, 1)
      IF (TELDIAM .GT. 0.0) THEN
         CALL DATPUTR ('Image', 'TELDIAM', TELDIAM, 1)
      ENDIF
      CALL ARRSETCO ('PSF', 0.0, 1.0)
      CALL IMGPB ('PSF', 'PSF', 'APPLY')
      IF(MODE.EQ.'OPTIMAL') THEN
         CALL IMGFFT ('PSF', 'XFR')
         CALL ARRMULT ('XFR', 'XFR', 'XFR')
         CALL IMGFFT ('XFR', 'PSF')
         CALL ARRSTAT ('PSF', ' ')
         SUM = DATFGETR ('PSF', 'ARRMAX')
         IF(ERROR) GO TO 999
         CALL ARRSCALE ('Image', 1.0/SUM, 0.0, 'Image')
         CALL ARRSCALE ('PSF', 1.0/SUM, 0.0, 'PSF')
      END IF
      CALL FILIMGPU ('Image', IMGFILE, ' ')
      CALL FILIMGPU ('PSF', PSFFILE, ' ')
C
 999  CONTINUE
      END


