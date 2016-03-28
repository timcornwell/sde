C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)surfmake.f	1.3	 12/12/91
C
      SUBROUTINE SDEMAIN
C
CD Make an aperture with surface errors
C
C Recomendation: This program should be run on a hefty machine:
C Should do 1K x 1K X-->X FFT's, or worse
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 20 1991
C	OOOPS!  We thought we were dealing with radians, but we were
C	dealing with degrees.  Now we know.
C				M.A.Holdaway	April 1 1991
C
C	Changed call to ARRSETAV to include a mask image,
C	ARRSFFIT also has MASK
C	Changed inner and outer to BIG, SMALL, as measured in the
C	aperture plane in meters
C				M.A.Holdaway	Nov 26 1991
C	Now can make up to 5 different surfaces at the same time
C	(That is, for a given surface pattern, it will scale
C	to different RMS.)
C				M.A.Holdaway	Dec 12 1991
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SURFMAKE')
C
C Names of input variables
C
      CHARACTER*(SYSMXNAM)	BEAM, VP, SURFACE, PB
      INTEGER		NDUMMY, PNAXIS(SYSMXDIM), I
      INTEGER		XADD
      CHARACTER*1	ATYPE
C
      INTEGER		DIR, SEED, ISIZE, FFTPWR2, NCOMP
      INTEGER           NAX, NAXIS(SYSMXDIM), NRMS
      INTEGER		IRMS, ISRMS
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
      DOUBLE PRECISION  CRVAL(SYSMXDIM)
      REAL              CRPIX(SYSMXDIM), CDELT(SYSMXDIM), 
     $   		CROTA(SYSMXDIM)
      CHARACTER*8       CTYPE(SYSMXDIM)
      REAL		BIG, SMALL, INNER, OUTER
      REAL		DIAM, LAMBDA, SRMS(5), POWER
      REAL		SCALE, ARRMAX, TELDIAM, DSCALE, FREQ
      REAL		MFLUX(2), MRA(2), MDEC(2), MXSIZE(2), MYSIZE(2),
     $   		MPA(2), PI, R2D
      LOGICAL		DOFIT
      CHARACTER*(SYSMXNAM)	MTYPE(2), VP2, VPFILE, PBFILE
C
      CHARACTER*(SYSMXNAM)	STRRMBL
      CHARACTER*6		STRINT
      REAL		DATFGETR
C
      DATA		NAXIS /SYSMXDIM * 1/
      DATA		PNAXIS /SYSMXDIM * 1/
      DATA		BLC /SYSMXDIM * 1/
      DATA		TRC /SYSMXDIM * 1/
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I simulate antenna surface errors')
      PI = ATAN2 (1.0, 1.0) * 4.0
      R2D = 180./PI
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('BEAM', BEAM, 1, NDUMMY)
      CALL USRGETC ('Surface', SURFACE, 1, NDUMMY)
      CALL USRGETC ('VP', VP, 1, NDUMMY)
      CALL USRGETC ('PB', PB, 1, NDUMMY)
      CALL USRGETI ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('RMS', SRMS, 5, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETI ('Blocked', NCOMP, 1, NDUMMY)
      CALL USRGETR ('Diam', TELDIAM, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETI ('Size', ISIZE, 1, NDUMMY)
      CALL USRGETR ('DeltScale', DSCALE, 1, NDUMMY)
      CALL USRGETR ('Power', POWER, 1, NDUMMY)
      CALL USRGETR ('Big', BIG, 1, NDUMMY)
      CALL USRGETR ('Small', SMALL, 1, NDUMMY)
      CALL USRGETL ('DoFit', DOFIT, 1, NDUMMY)
C
C SRMS is measured in meters
C Big, Small measured in meters
C
      CALL FILIMGGE ('BEAM', BEAM, ' ')
C
      CALL DATGETAR ('BEAM', NAX, NAXIS, ATYPE, XADD)
      DO 100 I = 1, SYSMXDIM
         IF (NAXIS(I) .GT. 1) PNAXIS(I) = FFTPWR2 ( ISIZE )
 100  CONTINUE
      DO 145 I = 1, 2
         BLC(I) = FFTPWR2 ( ISIZE ) / 2 - 63
         TRC(I) = FFTPWR2 ( ISIZE ) / 2 + 64
 145  CONTINUE
      CALL DATCREAT ('Window')
      CALL DATPUTI  ('Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI  ('Window', 'BLC', BLC, SYSMXDIM)
      CALL CRDGET ('BEAM', NAX, CTYPE, NAXIS,CRVAL,CRPIX,CDELT,CROTA)
      CDELT(1) = CDELT(1) * DSCALE
      CDELT(2) = CDELT(2) * DSCALE
      CALL CRDPUT ('BEAM', NAX, CTYPE, NAXIS,CRVAL,CRPIX,CDELT,CROTA)
      CALL IMGPAD ('BEAM', 'RBEAM', PNAXIS, 0.0)
      CALL DATDELET ('BEAM')
C
C we want a FULL transform, not the half plane thing, so lets
C make a complex image, filling imaginary part with 0.0
C
      CALL DATMAKAR ('XBEAM', NAX, PNAXIS, 'X', XADD)
      CALL HEDCOPY ('RBEAM', 'XBEAM')
      CALL IMGCLONE ('RBEAM', 'IBEAM')
      CALL ARRSETCO ('IBEAM', 0.0, 0.0 )
      CALL ARRQU2X  ('RBEAM', 'IBEAM', 'XBEAM')
      CALL DATDELET ('RBEAM')
      CALL DATDELET ('IBEAM')
      CALL FFTCONJX ('XBEAM', 'XAPERTURE', DIR, 0)
      CALL IMGREAL  ('XAPERTURE', 'Aperture')
      CALL DATDELET ('XAPERTURE')
      CALL CRDGET ('Aperture', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1   CROTA)
      LAMBDA = 3.0E+8 / FREQ
      CRVAL(3) = FREQ
      CALL CRDPUT ('Aperture', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     1   CROTA)
C
C Make antenna from CIRCULAR MODEL COMPONENTS in the "image" modeling facility
C
      DIAM = TELDIAM * 3600. /LAMBDA
      MFLUX(1) = 100.0
      MFLUX(2) = -1.0
      MRA(1) = 0.0
      MRA(2) = 0.0
      MDEC(1) = 0.0
      MDEC(2) = 0.0
      MXSIZE(1) = DIAM
      MXSIZE(2) = DIAM/10.
      MYSIZE(1) = DIAM
      MYSIZE(2) = DIAM/10.
      MPA(1) = 0.0
      MPA(2) = 0.0
      MTYPE(1) = 'DISK'
      MTYPE(2) = 'DISK'
      CALL ARRSETCO ('Aperture', 0.0, 0.0)
      CALL IMGMODEL ('Aperture', NCOMP, MFLUX, MRA, MDEC, MXSIZE, 
     $   MYSIZE, MPA, MTYPE )
C
      IF (ERROR) GOTO 999
      CALL MSGPUT ('Made Perfect Aperture', 'I')
      CALL ARRSTAT ('Aperture', ' ')
      ARRMAX = DATFGETR ('Aperture', 'ARRMAX')
      IF (ARRMAX .EQ. 0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'The model aperture''s max is ZERO')
         GOTO 990
      ELSE
         SCALE = 1.0 / ARRMAX
         CALL ARRSCALE ('Aperture', SCALE, 0.0, 'Aperture')
      ENDIF
C
C Make the surface error distribution
C Note for the confused:  We take two real images, initialize them
C with random values, turn them into a complex image, then apply 
C IMGPOWF for a power law distribution of surface errors.  Then the
C real part is taken.  Why complex to complex FFT?  Because
C that way we get the FULL PLANE rather than a HALF PLANE.
C 
C
      CALL IMGCLONE ('Aperture', 'Surface1')
      CALL ARRSETCO ('Surface1', 0.0, 0.0)
      CALL IMGCLONE ('Surface1', 'Surface2')
      CALL ARRGAUSS ('Surface1', 'Surface1', 1.0, SEED)
      SEED = SEED + 100
      CALL ARRGAUSS ('Surface2', 'Surface2', 1.0, SEED)
      CALL ARRQU2X  ('Surface1', 'Surface2', 'XSurface')
      CALL HEDCOPY  ('Surface1', 'XSurface')
      CALL MSGPUT ('1', 'D')
C
C Convert BIG, SMALL (in meters) to INNER, OUTER (in degrees on sky)
C
      INNER = R2D * 1./( BIG/LAMBDA )
      OUTER = R2D * 1. / ( SMALL/LAMBDA )
C
      CALL IMGPOWF ('XSurface', INNER, OUTER, POWER, 'XSurface', 
     $   'Swork')
C      
      CALL DATDELET ('Swork')
      CALL IMGREAL  ('XSurface', 'Surface1')
      CALL DATDELET ('XSurface')
      CALL MSGPUT ('Made Surface Errors', 'I')
C
C Fit a phase slope and curvature term, set average and RMS to specifications
C
      CALL ARRMULT ('Aperture', 'Surface1', 'Surface2')
      CALL DATDELET ('Surface1')
      IF (DOFIT) THEN
         CALL ARRSFFIT ('Surface2', 'Aperture', 0.5)
         CALL ARRMULT ('Surface2', 'Aperture', 'Surface2')
         CALL MSGPUT ('Corrected Surface Errors', 'I')
      ENDIF
      CALL ARRSETAV ('Surface2', 'Aperture', 0.0, 1.0, 0.5)
C
      IF (SURFACE .NE. ' ') THEN
         CALL FILIMGPU ('Surface2', SURFACE, ' ')
      ENDIF
C
C The above stuff is independent of SRMS
C Loop through for the surface dependent stuff
C
      DO 800 IRMS = 1, NRMS
C
C Do not delete Aperture, Surface2
C
         CALL ARRSETAV ('Surface2', 'Aperture', 0.0, SRMS(IRMS), 0.5)
C
C From Surface, make a complex image:
C Aperture * exp{ i4\pi * Surface(u)/lambda}
C
         CALL IMGCLONE ('Surface2', 'Surface3')
         SCALE = R2D * 4.0 * ( 4.0 * ATAN2 (1.0, 1.0) ) / LAMBDA
         CALL ARRSCALE ('Surface2', SCALE, 0.0, 'Surface3')
         CALL ARRAP2X ('Aperture', 'Surface3', 'XAperture')
         CALL DATDELET ('Surface3')
         CALL HEDCOPY ('Aperture', 'XAperture')
C
C X->X Fourier Transform to get voltage pattern corrupted by surface errors
C
         CALL MSGPUT ('3', 'D')
         CALL IMGFFT ('XAperture', 'VP')
         CALL DATDELET ('XAperture')
C
         CALL ARRSTAT ('VP', ' ')
         ARRMAX = DATFGETR ('VP', 'ARRMAX')
         IF (ARRMAX .LE.  0.) THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $         'Problem: VP max is LE ZERO')
            GOTO 990
         ENDIF
         SCALE = 1.0/ARRMAX
         CALL ARRSCALE ('VP', SCALE, 0., 'VP')
         CALL MSGPUT ('Made Voltage Pattern', 'I')
C
C Make a small version of VP
C
         CALL IMGSUBSE ('VP', 'VPS', 'Window' )
         CALL DATDELET ('VP')
         IF (VP .NE. ' ') THEN
            IF (INDEX (VP, 'SDE')  .EQ. 0) THEN
               VP2 = STRRMBL (VP//'.SDE')
            ELSE
               VP2 = VP
            ENDIF
            ISRMS = NINT ( 1.E+6 * SRMS(IRMS) )
            VPFILE = STRRMBL ( 'S'//STRINT(ISRMS)//VP2)
            CALL FILIMGPU('VPS', VPFILE,' ')
         ENDIF
C
C Make primary beam:
C
         IF (PB .NE. ' ') THEN
            CALL IMGCLONE ('VPS', 'VPSconj')
            CALL ARRCOPY ('VPS', 'VPSconj')
            CALL ARRCONJ ('VPSconj')
            CALL ARRMULT ('VPS', 'VPSconj', 'XPB')
            CALL HEDCOPY ('VPS', 'XPB')
            CALL DATDELET ('VPSconj')
            CALL IMGREAL ('XPB', 'PB')
            CALL DATDELET ('XPB')
            ISRMS = NINT ( 1.E+6 * SRMS(IRMS) )
            PBFILE = STRRMBL ( 'S'//STRINT(ISRMS)//PB)
            CALL FILIMGPU ('PB', PBFILE, ' ')
            CALL DATDELET ('PB')
         ENDIF
C
         CALL DATDELET ('VPS')
 800  CONTINUE
C
 990  IF (ERROR) THEN
      CALL ERRTRACE(ROUTINE)
      ENDIF
 999  CONTINUE
      END
