C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)k2p.f	1.3	 7/17/95
C
      SUBROUTINE SDEMAIN
C
CD Program to convert units from K to JY/pixel
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 22 1992
C	Also does conversion to Jy/Beam
C				M.A.Holdaway	March 17 1993
C	Reads in FREQ from inputs
C				M.A. Holdaway	July 17 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'K2P')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE , UNITS
      REAL		BEAM(4), FREQ
C==================================================================
      CALL MSGWELCO ('I convert from K to JY/PIXEL or JY/BEAM')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', SMFILE, 1, NDUMMY)
      CALL USRGETC ('Units', UNITS, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL IMGCLONE ('Image', 'SImage')
C
C Change units
C         
      CALL IMGK2P ('Image', 'SImage', FREQ)
C
C Do JY/BEAM
C
      IF (INDEX(UNITS, 'BEAM') .NE. 0) THEN
         CALL IMGP2PB ('SImage', BEAM, 'SImage')
      ENDIF
C
C Write out answer
C
      CALL FILIMGPU ('SImage', SMFILE, ' ')
C
 999  CONTINUE
      END
