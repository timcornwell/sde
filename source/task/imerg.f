C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imerg.f	1.4	 9/4/92
C
      SUBROUTINE SDEMAIN
C
C Program to make a linear combination of two images. This routine is
C called by the main program /sde/source/tsk/tsk.f and must be
C called SDEMAIN.
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 10 1991
C	Added JY/PIXEL as a possible image unit
C				M.A.Holdaway	June 11 1991
C	General mucking around
C				M.A.Holdaway	Jan 2 1992
C	Was not correctly dealing with units for JY/BEAM.
C	Removed HGEOM, must take care of HGEOMING before running.
C				M.A.Holdaway	Sept 4 1992
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMERG')
C
C Names of input variables
C
      REAL		RA, RB, SCALE
      CHARACTER*(SYSMXNAM) 	INFILE1,
     1				INFILE2,
     2                          OUTFILE, AVETYPE
C
      REAL		R1, R2, BMAJ1, BMAJ2, BMIN1, BMIN2, BPA1, BPA2
      REAL		TAPER1(4), TAPER2(4), DELT(SYSMXDIM), FACT
      REAL		MIND1, MIND2, JPB2J, J2JPB, RMAX
      INTEGER		NAX, NAXIS(SYSMXDIM), BCEN(SYSMXDIM)
      INTEGER		NDUMMY, IAX, ADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	BUNIT, BUNIT2
      DATA		TAPER1 /4 * 0.0/
      DATA		TAPER2 /4 * 0.0/
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I merg images of differing resolution')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
C
      CALL USRGETC  ('HighRes', INFILE1, 1, NDUMMY)
      CALL USRGETC  ('LowRes',INFILE2, 1, NDUMMY)
      CALL USRGETR  ('UVLim1', RA, 1, NDUMMY)
      CALL USRGETR  ('UVLim2', RB, 1, NDUMMY)
      CALL USRGETR  ('ScaleLow', SCALE, 1, NDUMMY)
      CALL USRGETC  ('MergType', AVETYPE, 1, NDUMMY)
      CALL USRGETC  ('Output', OUTFILE, 1, NDUMMY)
C
      MIND1 = .1
      MIND2 = .1
      R1 = MIN (RA, RB)
      R2 = MAX (RA, RB)
      CALL FILIMGGE ('ImageA', INFILE1, ' ')
      CALL FILIMGGE ('ImageB', INFILE2, ' ')
      CALL IMGDOUBL ('ImageA', 'Image1')
      CALL IMGDOUBL ('ImageB', 'Image2')
      CALL DATDELET ('ImageB')
C
C Make the images look the same
C

      IF (ERROR) GOTO 999
      CALL DATGETC ('Image2', 'BUNIT', BUNIT2, 1, NDUMMY)
      IF (ERROR) THEN
         BUNIT2 = 'JY/PIXEL'
      ENDIF
      CALL MSGPUT ('High Res Listing', 'D')
      CALL CRDLIST ('Image1')
      CALL MSGPUT ('Low Res Listing', 'D')
      CALL CRDLIST ('Image2')
C
C FFT to Fourier Plane
C
      IF (ERROR) GOTO 999
      CALL IMGFFT ('Image1', 'UV1')
      CALL IMGFFT ('Image2', 'UV2')
C
      CALL MSGPUT ('Listing for HighRes Transform', 'I')
      CALL CRDLIST ('UV1')
      CALL MSGPUT ('Listing for LowRes Transform', 'I')
      CALL CRDLIST ('UV2')
C
      IF (ERROR) GOTO 999
      CALL DATGETAR ('UV1', NAX, NAXIS, ATYPE, ADD)
      BCEN(1) = 1
      DO 10 IAX = 2, NAX
         IF (NAXIS(IAX).GT.1) THEN
            BCEN(IAX) = NAXIS(IAX) / 2
         ELSE
            BCEN(IAX) = 1
         END IF
  10  CONTINUE
      CALL DATGETR ('Image1', 'CDELT', DELT, SYSMXDIM, NDUMMY)
      FACT = LOG(2.0)/ATAN(1.0)
C
      IF (ERROR) GOTO 999
      CALL DATGETC ('Image1', 'BUNIT', BUNIT, 1, NDUMMY)
      IF (ERROR) THEN
C	 Assume JY/PIXEL
         CALL ERRCANCE
         CALL MSGPUT ('Assuming HighRes Image is in JY/PIXEL', 'I')
      ELSE IF (BUNIT.EQ. 'JY/BEAM') THEN
         CALL MSGPUT  ('Deconvolving HighRes by Gaussian Beam', 'I')
         CALL DATGETR ('Image1', 'BMAJ', BMAJ1, 1, NDUMMY)
         CALL DATGETR ('Image1', 'BMIN', BMIN1, 1, NDUMMY)
         CALL DATGETR ('Image1', 'BPA', BPA1, 1, NDUMMY)
         TAPER1(1) = FACT*FLOAT(NAXIS(2))/(BMAJ1/ABS(DELT(1)))
         TAPER1(2) = FACT*FLOAT(NAXIS(2))/(BMIN1/ABS(DELT(1)))
         TAPER1(3) = 90.0 + BPA1
         JPB2J = 1./( 1.133 * BMAJ1 * BMIN1 / DELT(1)**2 )
         CALL IMGCLONE ('UV1', 'BEAM1')
         CALL ARRSETCO ('BEAM1', 0., 1.)
         CALL ARRGTAPE ('BEAM1', TAPER1, BCEN, 'BEAM1')
         CALL ARRCDIV ('UV1', 'BEAM1', MIND1, 'UV1')         
         CALL ARRSCALE ('UV1', JPB2J, 0.0, 'UV1')
         CALL DATDELET ('BEAM1')
      ELSE IF (BUNIT .NE. 'JY/PIXEL') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Unknown image units: '//BUNIT(1:30))
         GOTO 999
      ENDIF
C
      IF (ERROR) GOTO 999
      CALL DATGETC ('Image2', 'BUNIT', BUNIT, 1, NDUMMY)
      IF (ERROR) THEN
C	 Assume JY/PIXEL
         CALL ERRCANCE
         CALL MSGPUT ('Assuming LowRes Image is in JY/PIXEL', 'I')
      ELSE IF (BUNIT .EQ. 'JY/BEAM') THEN
         CALL MSGPUT  ('Deconvolving LowRes by Gaussian Beam', 'I')
         CALL DATGETR ('Image2', 'BMAJ', BMAJ2, 1, NDUMMY)
         CALL DATGETR ('Image2', 'BMIN', BMIN2, 1, NDUMMY)
         CALL DATGETR ('Image2', 'BPA', BPA2, 1, NDUMMY)
         TAPER2(1) = FACT*FLOAT(NAXIS(2))/(BMAJ2/ABS(DELT(1)))
         TAPER2(2) = FACT*FLOAT(NAXIS(2))/(BMIN2/ABS(DELT(1)))
         TAPER2(3) = 90.0 + BPA2
         JPB2J = 1./( 1.133 * BMAJ2 * BMIN2 / DELT(1)**2 )
         CALL IMGCLONE ('UV2', 'BEAM2')
         CALL ARRSETCO ('BEAM2', 0., 1.)
         CALL ARRGTAPE ('BEAM2', TAPER2, BCEN, 'BEAM2')
         CALL ARRCDIV ('UV2', 'BEAM2', MIND2, 'UV2')
         CALL ARRSCALE ('UV2', JPB2J, 0.0, 'UV2')
         CALL DATDELET ('BEAM2')
      ELSE IF (BUNIT .NE. 'JY/PIXEL') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Unknown image units: '//BUNIT(1:30))
         GOTO 999
      ENDIF
      CALL ARRSTAT ('UV2', ' ')
      CALL DATGETR ('UV2', 'ARRMAX', RMAX, 1, NDUMMY)
      WRITE (MESSAGE, 1212) RMAX
 1212 FORMAT ('Peak in LOW RES Fourier plane is: ',F15.4)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (ERROR) GOTO 999
      CALL MSGPUT ('Merging the images in the UV plane', 'I')
      IF (SCALE .GT. 0.0) THEN
         WRITE (MESSAGE, 1111) SCALE
 1111    FORMAT ('Scaling low resolution image by ',E15.5)
         CALL MSGPUT (MESSAGE, 'I')
         CALL ARRSCALE ('UV2', SCALE, 0.0, 'UV2')
      ENDIF
      CALL IMGCLONE ('UV1', 'UVALL')
      CALL ARRSETCO ('UVALL', 0., 0.)
      CALL ARRRDMRG ('UV1', 'UV2', R1, R2, AVETYPE, 'UVALL')
C
      IF (ERROR) GOTO 999
      IF (TAPER1(1) .NE. 0.0) THEN
         CALL MSGPUT ('Retapering Image', 'I')
         CALL ARRGTAPE ('UVALL', TAPER1, BCEN, 'UVALL')      
         J2JPB = 1.133 * BMAJ1 * BMIN1 / DELT(1)**2
         CALL ARRSCALE ('UVALL', J2JPB, 0.0, 'UVALL')
      ENDIF
C
      IF (ERROR) GOTO 999
      CALL MSGPUT ('Transforming back to image plane', 'I')
      CALL IMGFFT ('UVALL', 'Image')
      CALL IMGFITTO ('Image', 'ImageA', 'Output')
      CALL FILIMGPU('Output',OUTFILE,' ')
C
 999  CONTINUE
      END



