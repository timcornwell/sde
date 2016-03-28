C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrave.f	1.5	 24 Jul 1995
C
      SUBROUTINE ARRRAVE (IMAGE, RAVE, PIXELAVE, NX, NY, RPIXI, NBINS,
     $   SCALE)
C
CD Get a radial average of an image
C
C	IMAGE	REAL	input	Real array
C	RAVE	REAL	output	Real 1-D array, radial ave
C	PIXELAVEREAL	out	Real 1-D array, ave pix dist to RAVE(i)
C	NX	INT	input	Size of IMAGE
C	NY	INT	input	Size of IMAGE
C	RPIXI	REAL	input	Reference Pixel of IMAGE
C	NBINS	INT	input	Size of RAVE
C      SCALE   REAL    input   scales the pixels
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
C	Removed NAXIS in the dimensions
C				M.A. Holdaway	July 23 1995
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY, NBINS
      REAL		IMAGE(NX,*), RPIXI(SYSMXDIM)
      REAL		PIXELAVE(NBINS), RAVE(NBINS), SCALE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRAVE')
C
      INTEGER		MAXBINS
      PARAMETER		(MAXBINS = 4096)
      INTEGER		IX, IY, NINBINS(MAXBINS), IBIN, I, MBIN
      REAL		YFACT(MAXBINS), XFACT(MAXBINS), RAD, PIXMAX1
      REAL		DELTAPIX
      REAL		PI, PIXELLIM(MAXBINS)
C
      CHARACTER*6	STRINT
      DATA		PI/3.1415926536/
C
C=======================================================================
C
C If an error on input then exit immediately
C
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
 3    CONTINUE
C
C create PIXELAVE AND PIXELLIM
C
      PIXMAX1 = MIN ((NX-RPIXI(1)), (NY-RPIXI(2)) )
      PIXELAVE(1) = 0.
      PIXELLIM(1) = 0.
      DELTAPIX = PIXMAX1/ FLOAT(NBINS)
      DO 5 IBIN = 2, NBINS
         PIXELAVE(IBIN) = PIXELAVE(IBIN-1) + DELTAPIX
         PIXELLIM(IBIN) = PIXELAVE(IBIN) + DELTAPIX/2.
 5    CONTINUE
C
C Loop through pixels, calc radius, sum into each bin, keep track
C of how many in each bin
C
      DO 20 IY = 1, NY
         YFACT(IY) = (IY - RPIXI(2))**2
 20   CONTINUE
      DO 25 IX = 1, NX
         XFACT(IX) = (IX - RPIXI(1))**2
 25   CONTINUE
      MBIN = 70
      DO 40 IY = 1, NY
         DO 30 IX = 1, NX
            RAD = SQRT( XFACT(IX) + YFACT(IY) )
            IBIN = NBINS
            DO 27 I = NBINS, 1, -1
               IF (RAD .LE. PIXELLIM(I)) IBIN = I
 27         CONTINUE
            NINBINS(IBIN) = NINBINS(IBIN) + 1
            RAVE(IBIN)    = RAVE(IBIN) + IMAGE(IX, IY)*IMAGE(IX, IY)
 30      CONTINUE
 40   CONTINUE
C
C average
C
C the zero spacing sits in a bin all by itself.  We report its distance
C as non-zero to dodge problems in log-log plots.
C
      RAVE(1) = ABS(IMAGE(NINT(RPIXI(1)), NINT(RPIXI(2))))
      PIXELAVE(1) = DELTAPIX / 4.0 * SCALE
C
      DO 50 IBIN = 2, NBINS
         IF (NINBINS(IBIN) .NE. 0 )  THEN
            RAVE(IBIN) = SQRT( RAVE(IBIN)/FLOAT(NINBINS(IBIN)) )
         ELSE
            RAVE(IBIN) = RAVE(IBIN-1)
         ENDIF
         PIXELAVE(IBIN) = PIXELAVE(IBIN) * SCALE
 50   CONTINUE
C      WRITE (6,*) NINBINS(MBIN), RAVE(MBIN)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
