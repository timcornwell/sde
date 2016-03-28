C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crddel.f	1.4    7/11/93
C
      SUBROUTINE CRDDEL (NAME)
C
CD Delete coordinate information for a directory entry
C
C
C	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Don't delete NAXIS -- it's not just a coordinate, and nearly
C	everything stuffs up if it isn't there.
C				D.S.Briggs	3 July 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDDEL')
C
      CHARACTER		STRM2*(SYSMXNAM)
C=====================================================================
      IF (ERROR) GO TO 999
C
C DELETE types of axes
C
      CALL DATDELET (STRM2(NAME, 'CTYPE'))
C
C DELETE reference value
C
      CALL DATDELET (STRM2(NAME, 'CRVAL'))
C
C DELETE reference pixel
C
      CALL DATDELET (STRM2(NAME, 'CRPIX'))
C
C DELETE coordinate increment
C
      CALL DATDELET (STRM2(NAME, 'CDELT'))
C
C DELETE coordinate rotation
C
      CALL DATDELET (STRM2(NAME, 'CROTA'))
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
