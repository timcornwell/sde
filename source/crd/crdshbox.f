C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdshbox.f	1.3    1/11/95
C
      SUBROUTINE CRDSHBOX (IN, INBLC, INTRC, OUT, OUTBLC, OUTTRC)
C
CD Shift a box with respect to IN into a box with respect to OUT
C
C	IN	CH*(*)	input	Name of input image
C	INBLC	INT	input	Input box BLC
C	INTRC	INT	input	Input box TRC
C	OUT	CH*(*)	input	Name of output image
C	OUTBLC	INT	output	Output box BLC
C	OUTTRC	INT	output	Output box TRC
C
C This routine is fairly forgiving, in that it will work approximately
C even if such things as cell size and reference values are different.
C Things will break when the coordinates have different rotations.  Also,
C roundoff error will start to give problems when the cell size is less
C than 10 mas or so.  Watch out for VLB!
C 
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 28 1992
C	Sanity checking added to output.  Box may not be larger than
C	output image.
C				D.S.Briggs	10 May 1992
C	Use double precision pixel coordinate routines.  (Needed to
C	prevent catastrophic loss of precision in VLBI cases.)
C				D.S.Briggs	Jan 10 1995
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      INTEGER		INBLC(*), INTRC(*), OUTBLC(*), OUTTRC(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDSHBOX')
C
      INTEGER		I
      REAL		RPIX(SYSMXDIM)
      DOUBLE PRECISION	WORLD(SYSMXDIM)
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Do the BLC first
C
      DO 10 I = 1, SYSMXDIM
         RPIX(I) = INBLC(I)
 10   CONTINUE
C
      CALL CRDDPTOW (IN, RPIX, WORLD)
      CALL CRDDWTOP (OUT, WORLD, RPIX)
      DO 20 I = 1, SYSMXDIM
         OUTBLC(I) = NINT(RPIX(I))
 20   CONTINUE
C
C And now the TRC
C
      DO 30 I = 1, SYSMXDIM
         RPIX(I) = INTRC(I)
 30   CONTINUE
C
      CALL CRDDPTOW (IN, RPIX, WORLD)
      CALL CRDDWTOP (OUT, WORLD, RPIX)
      DO 40 I = 1, SYSMXDIM
         OUTTRC(I) = NINT(RPIX(I))
 40   CONTINUE
C
      CALL CRDCLBOX (OUT, OUTBLC, OUTTRC)
C
      IF (ERROR) CALL ERRTRACE
C
 999  CONTINUE
      END
