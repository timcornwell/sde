C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcssrt.f	1.3    7/21/93
C
      SUBROUTINE SDEMAIN
CD     
C    Program to sort IPCS data over any of its random parameters
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcssum
C                                         R.G. Marson     Feb 13 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSRT')
C     
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, SORT
      INTEGER NDUMMY
C     
C==================================================================
C     
      CALL MSGWELCO ('I sort IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('Sort', SORT, 1, NDUMMY)
C
      CALL IPCSGET('input', INFILE, '*', ' ')
      IF (ERROR) GOTO 999
C
C Sort the data
C
      CALL IPCSSORT('input', SORT)
C
C Save the data
C

      CALL IPCSPUT ('input', OUTFILE)

 999  CONTINUE
      END












