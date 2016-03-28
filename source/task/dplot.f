C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)dplot.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to plot D terms
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 24 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DSTAT')
C
      CHARACTER*1		HAND1
      CHARACTER*(SYSMXNAM) 	INFILE, OLDFILE, AMPDEV, PHADEV, HAND,
     $   			DEVR, DEVL, DEVRL
      INTEGER			NDUMMY, IANT, STYLE, NPLOTS, NPOINTS
      LOGICAL			STAT2, STAT3, STAT4
C==================================================================
C
      CALL MSGWELCO ('I calculate d term statistics')
      OLDFILE = ' '
C
 1    CONTINUE
C
      CALL USRCTL
      CALL USRGETC ('DFile', INFILE, 1, NDUMMY)
      CALL USRGETI ('Ant', IANT, 1, NDUMMY)
      CALL USRGETC ('Hand', HAND, 1, NDUMMY)
      CALL USRGETC ('DevAmp', AMPDEV, 1, NDUMMY)
      CALL USRGETC ('DevPha', PHADEV, 1, NDUMMY)
      CALL USRGETI ('Style', STYLE, 1, NDUMMY)
      CALL USRGETI ('Npoints', NPOINTS, 1, NDUMMY)
      CALL USRGETC ('DevRscat', DEVR, 1, NDUMMY)
      CALL USRGETC ('DevLscat', DEVL, 1, NDUMMY)
      CALL USRGETC ('DevRLscat', DEVRL, 1, NDUMMY)
      CALL USRGETI ('Nplots', NPLOTS, 1, NDUMMY)
      CALL USRGETL ('Stat2', STAT2, 1, NDUMMY)
      CALL USRGETL ('Stat3', STAT3, 1, NDUMMY)
      CALL USRGETL ('Stat4', STAT4, 1, NDUMMY)
C
      HAND1 = HAND(1:1)
      IF (INFILE .NE. OLDFILE) THEN
         OLDFILE = INFILE
         CALL VSDGET ('PDT', INFILE)
      ENDIF
C
C vsdstat2 is for D terms effects on RR, LL visibilities
C vsdstat3 is for the time variability of the average value of DR+DL
C
      IF (STAT2) THEN
         CALL VSDSTAT2 ('PDT')
      ENDIF
      IF (STAT3) THEN
         CALL VSDSTAT3 ('PDT')
      ENDIF
      IF (STAT4) THEN
         CALL VSDSTAT4 ('PDT')
      ENDIF
C      CALL VSDSPLOT ('PDT', DEVR, DEVL, DEVRL, NPLOTS, 0.08)
      CALL VSDSPLOT ('PDT', DEVR, DEVL, DEVRL, NPLOTS, 0.0,INFILE(1:40))
      IF (AMPDEV .NE. ' ' .OR. PHADEV .NE. ' ') THEN
         CALL VSDTPLOT ('PDT', IANT, HAND1, AMPDEV, PHADEV, STYLE, 
     $      NPOINTS)
      ENDIF
C
      IF (ERROR) GOTO 999
      GOTO 1
C
 999  CONTINUE
      END
