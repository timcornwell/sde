C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrmean.f	1.2 07 Nov 1995
C
      SUBROUTINE ARRRMEAN (IMAGE, RAVE, RDISP, PIXELAVE, NAXISI, RPIXI,
     $     NBINS, SCALE, SHAPE, NX)
C
CD Get a radial MEAN and RMS about the mean, of an image
C
C	IMAGE	REAL	input	Real array
C	RAVE	REAL	output	Real 1-D array, radial ave
C	RDISP	REAL	output	Real 1-D array, dispersion about ave
C	PIXELAVEREAL	out	Real 1-D array, ave pix dist to RAVE(i)
C	NAXISI	INT	input	Size of IMAGE
C	RPIXI	REAL	input	Reference Pixel of IMAGE
C	NBINS	INT	input	Size of RAVE
C       SCALE   REAL    input   scales the pixels
C	SHAPE	REAL(2)	input	S(1)=elongation, S(2) = position angle
C       NX      INT     input   X-size of IMAGE (redundant-- NAXISI(1))
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 15 1991
C	Changed the way PIXELLIM and PIXELAVE were assigned
C	near ZERO to make all points equidistant
C				M.A.Holdaway	April 2 1991
C	Changed MAXBINS to 4096
C				M.A.Holdaway	July 12 1991
C	Minor bugfix.  Also made zero pixel return finite radius.
C				D.S.Briggs	July 1 1992
C       Bugfix: added NX to inputs (needed because can't
C       declare IMAGE(NAXISI(1),*).
C                               M.P.Rupen       November 6 1995
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAXISI(SYSMXDIM), NBINS, NX
      REAL		IMAGE(NX,*), RPIXI(SYSMXDIM)
      REAL		PIXELAVE(NBINS), RAVE(NBINS), RDISP(NBINS),
     $     		SCALE, SHAPE(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRMEAN')
C
      INTEGER		MAXBINS
      PARAMETER		(MAXBINS = 4096)
      INTEGER		IX, IY, NINBINS(MAXBINS), IBIN, I
      REAL		YFACT(MAXBINS), XFACT(MAXBINS), RAD2, PIXMAX1
      REAL		DELTAPIX, COSTHETA, SINTHETA
      REAL		PI, PIXLIM2(MAXBINS), D2R, XP, YP
C
      CHARACTER*6	STRINT
      DATA		PI/3.1415926536/
C
C=======================================================================
C
C If an error on input then exit immediately
C
      D2R = PI/180.0
      IF (ERROR) GO TO 999
C
      IF (NBINS .GT. MAXBINS) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'change MAXBINS to be greater than '//STRINT(NBINS) )
         GO TO 990
      ENDIF
      DO 3 I = 1, NBINS
         RAVE(I) = 0.
         NINBINS(I) = 0.
         RDISP(I) = 0.
 3    CONTINUE
C
C create PIXELAVE AND PIXLIM2
C
      PIXMAX1 = MIN ((NAXISI(1)-RPIXI(1)), (NAXISI(2)-RPIXI(2)) )
      PIXELAVE(1) = 0.
      PIXLIM2(1) = 0.
      DELTAPIX = PIXMAX1/ FLOAT(NBINS)
      DO 5 IBIN = 2, NBINS
         PIXELAVE(IBIN) = PIXELAVE(IBIN-1) + DELTAPIX
         PIXLIM2(IBIN) = (PIXELAVE(IBIN) + DELTAPIX/2.)**2
 5    CONTINUE
C
C Loop through pixels, calc radius, sum into each bin, keep track
C of how many in each bin
C
C Geometry: SHAPE(1) = BMAJ/BMIN
C           SHAPE(2) = PA of BMAJ measured counter-clockwise from Y axis
C           (Hence, we - 90 degrees from it for this rotation to XP, YP)
C
      COSTHETA = COS(D2R*(SHAPE(2)-90.0))
      SINTHETA = SIN(D2R*(SHAPE(2)-90.0))
      DO 20 IY = 1, NAXISI(2)
         YFACT(IY) = (IY - RPIXI(2))
 20   CONTINUE
      DO 25 IX = 1, NAXISI(1)
         XFACT(IX) = (IX - RPIXI(1))
 25   CONTINUE

      DO 40 IY = 1, NAXISI(2)
         DO 30 IX = 1, NAXISI(1)
            XP =  COSTHETA * XFACT(IX) + SINTHETA * YFACT(IY)
            YP = -SINTHETA * XFACT(IX) + COSTHETA * YFACT(IY)            
            RAD2 = (XP/SHAPE(1))**2 + YP**2
            IBIN = NBINS
            DO 27 I = NBINS, 1, -1
               IF (RAD2 .LE. PIXLIM2(I)) IBIN = I
 27         CONTINUE
            NINBINS(IBIN) = NINBINS(IBIN) + 1
            RAVE(IBIN)    = RAVE(IBIN) + IMAGE(IX, IY)
 30      CONTINUE
 40   CONTINUE
C
C
C the zero spacing sits in a bin all by itself.  We report its distance
C as non-zero to dodge problems in log-log plots.
C
      RAVE(1) = ABS(IMAGE(NINT(RPIXI(1)), NINT(RPIXI(2))))
      PIXELAVE(1) = DELTAPIX / 4.0 * SCALE
C
      DO 50 IBIN = 2, NBINS
         IF (NINBINS(IBIN) .NE. 0 )  THEN
            RAVE(IBIN) = RAVE(IBIN)/FLOAT(NINBINS(IBIN)) 
         ELSE
            RAVE(IBIN) = RAVE(IBIN-1)
         ENDIF
         PIXELAVE(IBIN) = PIXELAVE(IBIN) * SCALE
 50   CONTINUE
C
C get dispersion about the mean
C
      DO 140 IY = 1, NAXISI(2)
         DO 130 IX = 1, NAXISI(1)
            XP =  COSTHETA * XFACT(IX) + SINTHETA * YFACT(IY)
            YP = -SINTHETA * XFACT(IX) + COSTHETA * YFACT(IY)            
            RAD2 = (XP/SHAPE(1))**2 + YP**2
            IBIN = NBINS
            DO 127 I = NBINS, 1, -1
               IF (RAD2 .LE. PIXLIM2(I)) IBIN = I
 127        CONTINUE
            RDISP(IBIN) = RDISP(IBIN) +
     $           (IMAGE(IX, IY) - RAVE(IBIN))**2
 130     CONTINUE
 140  CONTINUE
C
      RDISP(1) = 0.0001
      DO 150 IBIN = 2, NBINS
         IF (NINBINS(IBIN) .NE. 0 )  THEN
            RDISP(IBIN) = SQRT(RDISP(IBIN)/(FLOAT(NINBINS(IBIN))-1))
         ELSE
            RDISP(IBIN) = RDISP(IBIN-1)
         ENDIF
 150  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

