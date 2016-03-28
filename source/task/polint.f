C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)polint.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to apply polarization D terms to a VIS file
C
C Audit trail:
C	New Program
C				M.A. Holdaway	April 21 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'POLINT')
C
      CHARACTER*(SYSMXNAM)	VISFILE, DTFILE, OUTFILE, INTERP
      LOGICAL		ROTATED
      INTEGER		NDUMMY
C
      REAL		SLON, SLAT
C==================================================================
      CALL MSGWELCO ('I interpolate and apply D terms')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('DTerms', DTFILE, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('Rotated', ROTATED, 1, NDUMMY)
      CALL USRGETC ('Interp', INTERP, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      CALL VSDGET ('DTerms', DTFILE)
C
C VLA site hardwired in
C
      SLON = 107.6177275
      SLAT = 34.078749167
      CALL DATPUTR ('Vis', 'SLON', SLON, 1)
      CALL DATPUTR ('Vis', 'SLAT', SLAT, 1)
C
      CALL VSDINTR ('Vis', 'DTerms', ROTATED, INTERP)
C
      IF (OUTFILE.NE.' ') THEN
         CALL VISPUT ('Vis', OUTFILE, 'OBS', 'IQUV', '*', ' ')
      END IF
C
 999  CONTINUE
      END

