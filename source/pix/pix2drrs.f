C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drrs.f	1.2    6/3/92
C
      SUBROUTINE PIX2DRRS (RES, WORK, GAIN, NX, NY, SUM)
C
CD Rescale clean components based on local resolution.  2-D real only.
C
C	RES	REAL(*)	in/out	Residual image
C	WORK	REAL(*) in/out	Work array
C	GAIN	REAL	input	Loop Gain
C	NX	INT	input	Number of pixels in x axis
C	NY	INT	input	Number of pixels in y axis
C	SUM	REAL	output	Sum of flux in CCs
C Audit trail:
C	Original version:
C				D.S.Briggs	Mar 2 1992
C	Make it work correctly
C				D.S.Briggs	June 2 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY
      REAL		RES(NX,NY), WORK(NX,NY), GAIN, SUM
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRRS')
C
      REAL		Q, SQSUM, CRSUM
      INTEGER		IX, IY
C=====================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      SUM = 0.0
      SQSUM = 0.0
      CRSUM = 0.0
C
C Find scale factor to minimize Sum(q work - res)^2
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IF (RES(IX,IY).NE.0.0) THEN
               SQSUM = SQSUM + WORK(IX,IY)*WORK(IX,IY)
               CRSUM = CRSUM + WORK(IX,IY)*RES(IX,IY)
            END IF
  1      CONTINUE
  2   CONTINUE
      Q = CRSUM / SQSUM
C
C Do the rescale
C
      DO 12 IY = 1, NY
         DO 11 IX = 1, NX
            RES(IX,IY) = RES(IX,IY) * Q * GAIN
            SUM = SUM + RES(IX,IY)
 11      CONTINUE
 12   CONTINUE
C
 999  CONTINUE
      END
