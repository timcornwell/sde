C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftsfndsc.f	1.3    11/7/90
C
      SUBROUTINE FTSFNDSC (NAME, BITPIX)
C
CD Find scaling factors for array and insert into header
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added BITPIX explicitly
C				T.J.Cornwell	April 17 1990
C
C---------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME
      INTEGER		BITPIX
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTSFNDSC')
C
      REAL		DMAX, DMIN, TAPEMAX, TAPEMIN, DATFGETR
      DOUBLE PRECISION	BSCALE, BZERO
C======================================================================
      IF (ERROR) GO TO 999
C
C Find the scaling factors
C
      CALL ARRSTAT (NAME, ' ')
      DMAX = DATFGETR (NAME, 'ARRMAX')
      DMIN = DATFGETR (NAME, 'ARRMIN')
      CALL DATPUTR (NAME, 'DATAMAX', DMAX, 1)
      CALL DATPUTR (NAME, 'DATAMIN', DMIN, 1)
      DMAX = (1+SIGN(0.01,DMAX))*DMAX
      DMIN = (1-SIGN(0.01,DMIN))*DMIN
      IF (ERROR) GO TO 990
      CALL DATPUTI (NAME, 'BITPIX', BITPIX, 1)
      TAPEMAX = 2**(BITPIX-1) - 1 - 1000
      TAPEMIN = - TAPEMAX + 1000
      BSCALE = (DMAX-DMIN)/(TAPEMAX-TAPEMIN)
      BZERO = (TAPEMAX*DMIN - TAPEMIN*DMAX)/(TAPEMAX-TAPEMIN)
      CALL DATPUTD (NAME, 'BSCALE', BSCALE, 1)
      CALL DATPUTD (NAME, 'BZERO',  BZERO, 1)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
