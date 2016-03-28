C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgbmshp.f	1.8 24 Feb 1995
C
      SUBROUTINE IMGBMSHP (PSF)
C
CD Fits a Gaussian beam to the PSF and writes BMAJ, BMIN, BPA into
C  the PSF header.
C
C	PSF	CH*(*)	input	Name of point spread function
C	PSF/FIT-ALG	CH*(*)	input	Select fitting algorithm to be used
C
C One may choose from a linearized least squares fit algorithm, a non-linear
C algorithm, or the incorrect linear algorithm found in AIPS and SDE prior
C to late 1993.  FIT-ALG should be 'LINEAR', 'NONLINEAR', or 'ARCHIVE'
C   ' ' => 'LINEAR'
C
C Do not use 'ARCHIVE', unless to estimate beam fitting errors in
C archival images.  It gets one of the two parameters correct, BMAJ or BMIN,
C and misses the other in the direction of the one it got right.
C
C The non-linear fitter is arguably more correct if a high precision match
C is desired for the purposes of photometry of weak sources, but the
C difference is quite small for well formed beams.  When seeded by the linear
C algorithm, the non-linear algorithm is fairly robust.
C
C Audit trail:
C	Original version
C				M.A.Holdaway	Aug 28 1989
C	Only warn if peak not unity
C				T.J. Cornwell   June 13 1991
C	Nonlinear and corrected linear algorithms added, with provisions
C	for selecting between them.  Fit to peak of image, but warn if peak
C	is not at ref pix.
C				D.S.Briggs	Aug 29 1993
C	Tweaked calling convention to PIXBMSHP
C				D.S.Briggs	Feb 8 1994
C	Always call the linear routine to start the non-linear one.
C	Ignore any beam that might be in the header already.
C				D.S.Briggs	March 22 1994
C	Put "#include stdinc.h" before any vars declared to eliminate
C	compiler warning.
C				M. Stupar	Dec 28 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGBMSHP')
C
      CHARACTER*(*)	PSF
C
      CHARACTER*1	T
      CHARACTER*(SYSMXNAM)	FITALG
      INTEGER		N, P, NDUMMY, ADD, NAXIS(SYSMXDIM)
      INTEGER		NROW(SYSMXDIM), IPEAK(SYSMXDIM), MAXPTS, SADD
      REAL		AMIN, BMAJA, BMINA, CRPIX(SYSMXDIM)
      REAL		CDELT(SYSMXDIM), BMAJ, BMIN, BPA, BAMP, BZ
C
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
      REAL		DATFGETR
      INTEGER		PIXISAMA
C=======================================================================
      IF (ERROR) GO TO 999
C
C See the bottom of this file for a really ugly kludge!
C
      CALL IMGBMPRECHK (PSF)
C
      FITALG = ' '
      IF (DATEXIST(STRM2(PSF,'FIT-ALG')))
     $   CALL DATGETC (PSF, 'FIT-ALG', FITALG, 1, NDUMMY)
      IF (FITALG.EQ.' ') FITALG = 'LINEAR'
      IF ((FITALG.NE.'LINEAR').AND.(FITALG.NE.'NONLINEAR').AND.
     $    (FITALG.NE.'ARCHIVE')) THEN
         CALL MSGPUT ('Unrecognised FIT-ALG selector in IMGBMSHP'
     $      // ' -- Using LINEAR','W')
         FITALG = 'LINEAR'
      END IF
C
      IF (FITALG.EQ.'NONLINEAR') THEN
         BMAJ = DATFGETR (PSF, 'BMAJ')
         BMIN = DATFGETR (PSF, 'BMIN')
         BPA = DATFGETR (PSF, 'BPA')
         BZ = DATFGETR (PSF, 'BZ')
      END IF
C
      AMIN = .35
      BAMP = 0.
      CALL DATGETAR(PSF, N, NAXIS, T, ADD)
      CALL DATGETR(PSF, 'CDELT', CDELT, N, NDUMMY)
      CALL DATGETR(PSF, 'CRPIX', CRPIX, N, NDUMMY)
      IF (ERROR) GOTO 990
      IF(T .NE. 'R') THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $      'PSF image type ='//T)
         GOTO 990
      ENDIF
      IF (NAXIS(3).GT.1) THEN
         CALL ERRREPOR(ERRFATAL, ROUTINE, 'Only programmed for 2-D')
         GO TO 990
      END IF
C
      P = PIXISAMA(NAXIS(1)*NAXIS(2), MEMR(ADD), 1) - 1
      IPEAK(1) = 1 + MOD(P, NAXIS(1))
      IPEAK(2) = 1 + (P - IPEAK(1) + 1)/NAXIS(1)
      IF (IPEAK(1).GT.NAXIS(1)) IPEAK(1) = NAXIS(1) / 2
      IF (IPEAK(2).GT.NAXIS(2)) IPEAK(2) = NAXIS(2) / 2

      IF ((IPEAK(1).NE.NINT(CRPIX(1))).OR.
     $    (IPEAK(2).NE.NINT(CRPIX(2)))) THEN
         WRITE (MESSAGE, 1000) IPEAK(1), IPEAK(2)
 1000    FORMAT ('Warning: Peak of PSF image is',I5,',',I5)
         CALL MSGPUT (MESSAGE, 'W')
         WRITE (MESSAGE, 1010) NINT(CRPIX(1)), NINT(CRPIX(2))
 1010    FORMAT ('Warning: PSF reference position is',I5,',',I5)
         CALL MSGPUT (MESSAGE, 'W')
      END IF
C						Gaussian to be fit to
C						2*NROW(1)+1 by 2*NROW(2)+1
C						patch of PSF about IPEAK
      NROW(1) = 5
      NROW(2) = 5
      MAXPTS = (2*NROW(1)+1)*(2*NROW(2)+1)
C
      IF (FITALG.EQ.'LINEAR') THEN
         CALL PIXBMSHP (MEMR(ADD), NAXIS(1), NAXIS(2), CDELT, IPEAK,
     $      NROW, AMIN, BMAJ, BMIN, BPA, BZ, BAMP)
      ELSE IF (FITALG.EQ.'NONLINEAR') THEN
         CALL DATMAKAR ('BmshpScratch', 1, 4*MAXPTS, 'R', SADD)
         CALL PIXBMSH1 (MEMR(ADD), NAXIS(1), NAXIS(2), CDELT, IPEAK,
     $      NROW, AMIN, MEMR(SADD), MEMR(SADD+MAXPTS),
     $      MEMR(SADD+2*MAXPTS), MEMR(SADD+3*MAXPTS),
     $      BMAJ, BMIN, BPA, BZ, BAMP)
         CALL DATDELET ('BmshpScratch')
      ELSE IF (FITALG.EQ.'ARCHIVE') THEN
         CALL PIXBMSHX (MEMR(ADD), N, NAXIS, CDELT, IPEAK, NROW, AMIN,
     $      BMAJ, BMIN, BPA, BZ, BAMP)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Unrecognized FIT-ALG')
         GO TO 999
      END IF
      IF (ERROR) GO TO 990
C
      BMAJA = BMAJ*3600.
      BMINA = BMIN*3600.
      WRITE (MESSAGE, 180) BMAJA,BMINA,BPA
 180  FORMAT ('Fit Beam (FWHM in sec) = ',2F12.5,F7.1)
      CALL MSGPUT( MESSAGE, 'I')
C
      IF (BMAJ .LE. 0. .OR. BMIN .LE. 0.) THEN
         CALL ERRREPOR(ERRBDARG,  ROUTINE, 'Beam Axes are .LE. 0')
         GO TO 990
      ELSE IF ( ABS( BAMP - 1.0 ) .GT. .001 ) THEN
         WRITE (MESSAGE,200) BAMP
 200     FORMAT ('Warning: Amplitude of PSF at center was ', F12.5)
         CALL MSGPUT(MESSAGE, 'W')
      END IF
      CALL DATPUTR(PSF, 'BMAJ', BMAJ, 1)
      CALL DATPUTR(PSF, 'BMIN', BMIN, 1)
      CALL DATPUTR(PSF, 'BPA', BPA, 1)
      CALL DATPUTR(PSF, 'BZ', BZ, 1)
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
C
C This is not standard Fortran 77, but I can't think of a compiler
C currently extant that will break on this code.  It is recursing, but
C does not rely on any state information in the subroutine at all.  The
C idea is that if we are using the non-linear solver we'll need to call
C the linear solver first to get an initial guess -- which means that
C imgbmshp wants to call itself with different arguments.  We don't really
C need to compile SDE on a PDP-8, after all.
C
      SUBROUTINE IMGBMPRECHK (PSF)
#include "stdinc.h"
      CHARACTER*(*) PSF
      LOGICAL DATEXIST, OLDMSG
      CHARACTER*(SYSMXNAM) STRM2, FITALG
      INTEGER NDUMMY
C
      IF (DATEXIST(STRM2(PSF,'FIT-ALG'))) THEN
         CALL DATGETC (PSF, 'FIT-ALG', FITALG, 1, NDUMMY)
         IF (FITALG.EQ.'NONLINEAR') THEN
            CALL DATPUTC (PSF,'FIT-ALG','LINEAR',1)
            OLDMSG = SYSMSG
            SYSMSG = .FALSE.
            CALL IMGBMSHP(PSF)
            SYSMSG = OLDMSG
            CALL DATPUTC (PSF,'FIT-ALG','NONLINEAR',1)
         END IF
      END IF
      END

