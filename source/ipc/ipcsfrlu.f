C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsfrlu.f	1.2    1/7/94
C
      SUBROUTINE IPCSFRLU (LOOKUP, SIZE, LAMBDA)
C
CD Make a lookup table for fringe straightening of IPCS data
C
C
C
C	LOOKUP		REAL	INPUT   Blank array to contain lookup table
C	SIZE		INT	INPUT   Size of above array
C	LAMBDA          REAL    INPUT   Wavelength range
C Audit trail:
C      Cloned from ipcssort
C                              R.G. Marson     Feb 11 1991
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER           SIZE(2)
      REAL          	LOOKUP(SIZE(1), SIZE(2))
      REAL              LAMBDA(2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSORT')
C
C Function Declerations
C
      REAL              IPCPX2FR, IPCWAVE
C
C Local Declerations
C
      REAL              LCENTER, NEWLOC, CLAMBDA
      INTEGER           I, J
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Find center wavelength and assign it to middle channel
C
      LCENTER = IPCWAVE(SIZE(1)/2, SIZE(1), LAMBDA)
C
C Fill the table with suitable numbers
C
      DO I = 1, SIZE(1)
         CLAMBDA = IPCWAVE(I, SIZE(1), LAMBDA)
         DO J = 1, SIZE(2)
            NEWLOC = IPCPX2FR(CLAMBDA, LCENTER, J-(SIZE(2)/2))
            LOOKUP(I, J) = NEWLOC + FLOAT(SIZE(2)/2)
         END DO
      END DO
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
C
C#      REAL FUNCTION IPCCH2WV(LAMBDAR, SIZE, CHANNEL)
C#C
C#C Given the wavelengths at the extremities of the IPCS detector
C#C This routine calculates the wavelength of any column (channel) of
C#C the detector. Initially this is simply linear until I know bettor.
C#C
C#      REAL LAMBDAR(2)
C#      INTEGER SIZE, CHANNEL
C#C
C#      IPCCH2WV = (LAMBDAR(2) - LAMBDAR(1))/FLOAT(SIZE-1) 
C#     $     * FLOAT(CHANNEL-1) + LAMBDAR(1)
C#      RETURN
C#      END
C#C
      REAL FUNCTION IPCPX2FR(CLAMBDA, LCENTER, FRINGE)
C
C Given the current wavelength, the center wavelength and the offset from
C the center of the detector of the current pixel calculate the 
C new location of the pixel to correct for wavelength 
C
      REAL CLAMBDA, LCENTER
      INTEGER FRINGE
C
      IPCPX2FR = LCENTER/CLAMBDA * FRINGE
      RETURN
      END

