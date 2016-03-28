C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcwave.f	1.1    2/10/93
C
      REAL FUNCTION IPCWAVE (COL, CSIZE, WAVE)
C
CD Given the detector size and Wavelength coverage calculate Wavelength
C
C
C	COL	INTEGER         Wavelength is desired for this column
C	CSIZE	INTEGER         Size of the wavelength axis of the detector
C	WAVE	REAL(2)         Wavelength of the start and end columns
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 19 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           COL, CSIZE
      REAL              WAVE(2)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSIM')

C
C Function definitions
C

C
C Local Variables
C
      REAL              A, B, WAVE2(2)
      SAVE              A, B
      DATA              A/0/, B/0/
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Check if we need to initialise A and B
C
      IF ((ABS(A).LT.1.E-30).AND.(ABS(B).LT.1.E-30)) THEN
         WAVE2(1) = WAVE(1) ** 2
         WAVE2(2) = WAVE(2) ** 2
         B = (CSIZE - 1) * WAVE2(2)/ (WAVE2(2) - WAVE2(1))
         A = -WAVE2(1) * B
      END IF
C
C Just calculate the wavelength Ref: Tim's Thesis
C
      IF (CSIZE.GT.1) THEN
         IPCWAVE = SQRT(A/((COL-1) - B))
      ELSE
         IPCWAVE = WAVE(1)
      END IF
C
  999 CONTINUE
      END
