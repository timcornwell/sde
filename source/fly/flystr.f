C++
      SUBROUTINE FLYSTR (FS, I, NPATCH)
C
C Returns character string containing coordinates of this patch
C relative to the center.
C
C	I1	INT	input	Patch number
C	I2	INT	input	Patch number
C
C Audit trail:
C	Added outlier support
C					T.J. Cornwell 20 March 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	FS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYSTR')
      INTEGER	I, NPATCH
      INTEGER	X, Y
C==================================================================
C
      IF(I.GT.(2*NPATCH+1)**2) THEN
         WRITE(FS, 1100) I
 1100    FORMAT (I3,' (outlier)')
      ELSE
         X = MOD(I-1, (2*NPATCH+1))+1
         Y = (I-X)/(2*NPATCH+1)+1
         WRITE(FS, 1000) I, X-(NPATCH+1), Y-(NPATCH+1)
 1000    FORMAT (I3,' (',I3,',',I3,')')
      ENDIF
C
      END
