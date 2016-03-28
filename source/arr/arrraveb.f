C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrraveb.f	1.4    03 Aug 1995
C
      SUBROUTINE ARRRAVEB (IMAGE, BOX, RAVE, PIXELAVE, NX, NY, RPIXI,
     $   NBINS, SCALE, DOCOPY, UNDEF, AMODE)
C
CD Get a radial average of an image
C
C	IMAGE	REAL	input	Real array
C	BOX	REAL	input	Box array, same size as IMAGE
C	RAVE	REAL	output	Real 1-D array, radial ave
C	PIXELAVEREAL	out	Real 1-D array, ave pix dist to RAVE(i)
C	NX, NY	INT	input	Size of IMAGE
C	RPIXI	REAL	input	Reference Pixel of IMAGE
C	NBINS	INT	input	Size of RAVE
C       SCALE   REAL    input   scales the pixels
C	DOCOPY	LOG	input	Copy undefined values from last good value?
C	UNDEF	REAL	input	Set to this value if DOCOPY=F and no samples
C	AMODE	CH*(*)	input	Averaging mode:  'RMS' | 'MEAN'
C
C Audit trail:
C	Added BOX, cloned from ARRRAVE.  The structure of this entire
C	set of routines is something of a both.  Too damned many cross
C	calls at the wrong level!
C				D.S.Briggs	Apr 14 1993
C	Added DOCOPY and UNDEF
C				D.S.Briggs	Oct 30 1994
C	Added averaging mode
C				D.S.Briggs	Dec 22 1994
C	Fixed the NAXIS dimension problem
C				M.A.Holdaway	Aug 3 1995
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		NX, NY, NBINS
      REAL		IMAGE(NX,*), BOX(NX,*),
     $   		RPIXI(SYSMXDIM), PIXELAVE(NBINS),
     $   		RAVE(NBINS), SCALE, UNDEF
      CHARACTER*(*)	AMODE
      LOGICAL		DOCOPY
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRAVEB')
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
      IF (ERROR) GO TO 999
C
      IF ((AMODE.NE.'RMS').AND.(AMODE.NE.'MEAN')) THEN
         MESSAGE = 'Unrecognized averaging mode: ' // AMODE
         CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IF (NBINS .GT. MAXBINS) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'change MAXBINS to be greater than '//STRINT(NBINS) )
         GO TO 999
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
            IF (BOX(IX,IY).NE.0.0) THEN
               RAD = SQRT( XFACT(IX) + YFACT(IY) )
               IBIN = NBINS
               DO 27 I = NBINS, 1, -1
                  IF (RAD .LE. PIXELLIM(I)) IBIN = I
 27            CONTINUE
               NINBINS(IBIN) = NINBINS(IBIN) + 1
               IF (AMODE(1:1).EQ.'R') THEN
                  RAVE(IBIN) = RAVE(IBIN) + IMAGE(IX,IY)*IMAGE(IX,IY)
               ELSE
                  RAVE(IBIN) = RAVE(IBIN) + IMAGE(IX,IY)
               END IF
            END IF
 30      CONTINUE
 40   CONTINUE
C
C average
C
C the zero spacing sits in a bin all by itself.  We report its distance
C as non-zero to dodge problems in log-log plots.
C
      IF (DOCOPY.OR.(BOX(NINT(RPIXI(1)), NINT(RPIXI(2))).NE.0.0)) THEN
         IF (AMODE(1:1).EQ.'R') THEN
            RAVE(1) = ABS(IMAGE(NINT(RPIXI(1)), NINT(RPIXI(2))))
         ELSE
            RAVE(1) = IMAGE(NINT(RPIXI(1)), NINT(RPIXI(2)))
         END IF
      ELSE
         RAVE(1) = UNDEF
      END IF
      PIXELAVE(1) = DELTAPIX / 4.0 * SCALE
C
      DO 50 IBIN = 2, NBINS
         IF (NINBINS(IBIN) .NE. 0 )  THEN
            IF (AMODE(1:1).EQ.'R') THEN
               RAVE(IBIN) = SQRT( RAVE(IBIN)/FLOAT(NINBINS(IBIN)) )
            ELSE
               RAVE(IBIN) = RAVE(IBIN)/FLOAT(NINBINS(IBIN))
            END IF
         ELSE
            IF (DOCOPY) THEN
               RAVE(IBIN) = RAVE(IBIN-1)
            ELSE
               RAVE(IBIN) = UNDEF
            END IF
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
