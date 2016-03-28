C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C	(C)opyright 1989 by NRAO; all rights reserved
C
      SUBROUTINE SDEMAIN
C
CD Program to plot uv data
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	New task
CA				T.J. Cornwell	March 10 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ANPLT')
C
      CHARACTER*(SYSMXNAM)	ANTFILE, X, Y, PLFILE
      INTEGER		NDUMMY, SKIP
      REAL              SITELAT, FACT
C==================================================================
      CALL MSGWELCO ('I plot antenna data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('X', X, 1, NDUMMY)
      CALL USRGETC('Y', Y, 1, NDUMMY)
      CALL USRGETI('Skip', SKIP, 1, NDUMMY)
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC('Plotfile', PLFILE, 1, NDUMMY)
C
C Read the antennas file
C  
      CALL FILGETAN ('Antfile', ANTFILE)
      CALL DATGETR ('Antfile', 'SITELAT', SITELAT, 1, NDUMMY)
C
C Remove latitude effect to get onto flat plane at site
C
      FACT = 1.0/SIN(SITELAT)
      CALL ARRSCALE ('Antfile/LY', FACT, 0.0, 'Antfile/LY')
C
C Make options directory
C
      CALL DATCREAT ('Options')
      CALL DATPUTC ('Options', 'DEV', PLFILE, 1)
      CALL DATPUTI ('Options', 'PTSKIP', SKIP, 1)
      CALL DATPUTI ('Options', 'PTTYPE', 17, 1)
      CALL DATPUTI ('Options', 'SQUARE', .TRUE., 1)
      CALL DATPUTC ('Options', 'XLABEL', 'X (meters)', 1)
      CALL DATPUTC ('Options', 'YLABEL', 'Y (meters)', 1)
      CALL DATPUTC ('Options', 'PLTLABEL', 'Antenna locations', 1)
C
C Call routine to plot the data
C
      CALL ARR2DPLT ('Antfile/'//X, 'Antfile/'//Y, 'Options')
C
 999  CONTINUE
      END
