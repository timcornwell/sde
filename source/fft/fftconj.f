C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftconj.f	1.8	 7/20/92
C
      SUBROUTINE FFTCONJ (TYPE, CTYPE, NAX, NAXIS, DELT, FFTSIZE,
     $   CDELT, DIR)
C
CD Conjugate axis types e.g. 'RA---SIN' becomes 'UU---SIN'.
C Following the AIPS convention, only the first four characters
C denote coordinate type: the rest denotes projection. Returns
C CTYPE = ' ' if no known conjugate. The direction of the transform
C is also found and returned.
C
C	TYPE	CH*(*)	Input	Axis types
C	CTYPE	CH*(*)	Output	Axis type
C	NAX	INT	Input	Number of axes
C	NAXIS	INT	Input	Number of pixels on each axis
C	DELT	REAL	Input	Axis increment of input
C	FFTSIZE CH*(*)	Input	'EXACT' or 'PWR2' : ' ' => 'PWR2'
C	CDELT	REAL	Output	Axis increment of output
C	DIR	INT	Output	Direction of transform
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added RX,RY,RZ <--> KX,KY,KZ to conjugation list
C				M.A.Holdaway	May 10 1991
C       Added DTX, DTY , FRX, FRY to list
C                               R.G.Marson      Aug 3 1991
C	Added FFTSIZE argument to support arbitrary N FFTs.
C				D.S.Briggs	Jan 3 1992
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	TYPE(*), CTYPE(*), FFTSIZE 
      INTEGER		DIR(*), NAX, NAXIS(*)
      REAL		DELT(*), CDELT(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTCONJ')
C
      REAL		D2R, PI
      PARAMETER		(PI = 3.141592654)
      PARAMETER		(D2R = PI/180.0)
C
      INTEGER		FFTPWR2, IAX
      REAL 		WIDTH
C=====================================================================
      DO 10 IAX = 1, NAX
         DIR(IAX) = +1
         CDELT(IAX) = 1.0
         CTYPE(IAX) = TYPE(IAX)
 10   CONTINUE
C
      IF (ERROR) GO TO 999
C
C Do all axes
C
      DO 20 IAX = 1, NAX
C
C Find width of axis
C
         IF (NAXIS(IAX).GT.1) THEN
            IF (FFTSIZE.NE.'EXACT') THEN
               WIDTH = FFTPWR2(NAXIS(IAX))*DELT(IAX)
            ELSE
               WIDTH = NAXIS(IAX)*DELT(IAX)
            END IF
         ELSE
            WIDTH = DELT(IAX)
         END IF
C
C See if we recognize this axis
C
         IF (TYPE(IAX)(1:4).EQ.'RA--') THEN
            DIR(IAX) = + 1
            CTYPE(IAX)(1:4) = 'UU--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'DEC-') THEN
            DIR(IAX) = + 1
            CTYPE(IAX)(1:4) = 'VV--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'N---') THEN
            DIR(IAX) = + 1
            CTYPE(IAX)(1:4) = 'WW--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'FREQ') THEN
            DIR(IAX) = + 1
            CTYPE(IAX)(1:4) = 'LAG-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'UU--') THEN
            DIR(IAX) = - 1
            CTYPE(IAX)(1:4) = 'RA--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'VV--') THEN
            DIR(IAX) = - 1
            CTYPE(IAX)(1:4) = 'DEC-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSEIF (TYPE(IAX)(1:4).EQ.'WW--') THEN
            DIR(IAX) = - 1
            CTYPE(IAX)(1:4) = 'N---'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(D2R*WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'LAG-') THEN
            DIR(IAX) = - 1
            CTYPE(IAX)(1:4) = 'FREQ'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'RX--') THEN
            DIR(IAX) =  + 1
            CTYPE(IAX)(1:4) = 'KX--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'RY--') THEN
            DIR(IAX) =  + 1
            CTYPE(IAX)(1:4) = 'KY--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'RZ--') THEN
            DIR(IAX) =  + 1
            CTYPE(IAX)(1:4) = 'KZ--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'KX--') THEN
            DIR(IAX) =  - 1
            CTYPE(IAX)(1:4) = 'RX--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'KY--') THEN
            DIR(IAX) =  - 1
            CTYPE(IAX)(1:4) = 'RY--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:4).EQ.'KZ--') THEN
            DIR(IAX) =  - 1
            CTYPE(IAX)(1:4) = 'RZ--'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:3).EQ.'DTX') THEN
            DIR(IAX) =  + 1
            CTYPE(IAX)(1:4) = 'FRX-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:3).EQ.'DTY') THEN
            DIR(IAX) =  + 1
            CTYPE(IAX)(1:4) = 'FRY-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:3).EQ.'FRX') THEN
            DIR(IAX) =  - 1
            CTYPE(IAX)(1:4) = 'DTX-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE IF (TYPE(IAX)(1:3).EQ.'FRY') THEN
            DIR(IAX) =  - 1
            CTYPE(IAX)(1:4) = 'DTY-'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         ELSE
            DIR(IAX) = + 1
            CTYPE(IAX)(1:4) = 'UNKN'
            IF (WIDTH.NE.0.0) THEN
               CDELT(IAX) = 1.0/(WIDTH)
            ELSE
               CDELT(IAX) = 1.0
            END IF
         END IF
C
 20   CONTINUE
C
 999  CONTINUE
      END
      
