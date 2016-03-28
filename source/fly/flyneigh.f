C++
      LOGICAL FUNCTION FLYNEIGH (I1, I2, NPATCH)
C
C Check if patchs are neighbors. Note that a patch is its own
C neighbor.
C
C	I1	INT	input	Patch number
C	I2	INT	input	Patch number
C
C Audit trail:
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FLYNEIGH')
      INTEGER	I1, I2, NPATCH
      INTEGER	X1, X2, Y1, Y2
C==================================================================
C
      X1 = MOD(I1-1, (2*NPATCH+1))+1
      Y1 = (I1-X1)/(2*NPATCH+1)+1
      X2 = MOD(I2-1, (2*NPATCH+1))+1
      Y2 = (I2-X2)/(2*NPATCH+1)+1
      FLYNEIGH=(I1.NE.I2).AND.(ABS(X1-X2).LE.1).AND.(ABS(Y1-Y2).LE.1)
C
      RETURN
      END
