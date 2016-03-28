C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hedsetob.f	1.1    3/28/91
C
      SUBROUTINE HEDSETOB (IN, OUT)
C
CD Copies OBSERVATION HEADER parameters from IN to OUT: OBRA, OBDEC, TELESCOP....
C
C	OBSRA, OBSDEC, TELESCOP, TELDIAM, THROWRA, THROWDEC:
C	Anything you need to do mosaic, among other things.
C
C	NAME	CH*(*)	input	Name of 
C	IN	CH*(*)	inpiut	Name of input directory entry
C	OUT	CH*(*)	OUTpiut	Name of OUTput directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 17 1990
C	Added THROWRA and THROWDEC
C				M.A.Holdaway	March 28 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'HEDSETOB')
C
      DOUBLE PRECISION	OBSRA, OBSDEC, THROWRA, THROWDEC
      REAL		TELDIAM
      CHARACTER*(SYSMXNAM)	TELESCOP, STRM2
      LOGICAL		DATEXIST
      INTEGER		NDUMMY
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Copy Observed RA, Observed DEC
C
      CALL DATGETD (IN, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATGETD (IN, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATPUTD (OUT, 'OBSRA', OBSRA, 1)
      CALL DATPUTD (OUT, 'OBSDEC', OBSDEC, 1)
C
C Copy Observed RA, Observed DEC
C
      IF (DATEXIST ( STRM2(IN, 'THROWRA')))  THEN
         CALL DATGETD (IN, 'THROWRA', THROWRA, 1, NDUMMY)
         CALL DATPUTD (OUT, 'THROWRA', THROWRA, 1)
      ENDIF
      IF (DATEXIST ( STRM2(IN, 'THROWDEC')))  THEN
         CALL DATGETD (IN, 'THROWDEC', THROWDEC, 1, NDUMMY)
         CALL DATPUTD (OUT, 'THROWDEC', THROWDEC, 1)
      ENDIF
C
C Copy Telescop Type
C
      CALL DATGETC (IN, 'TELESCOP', TELESCOP, 1, NDUMMY)
      CALL DATPUTC (OUT, 'TELESCOP', TELESCOP, 1)
C
      IF (DATEXIST ( STRM2(IN,'TELDIAM')))  THEN
         CALL DATGETR (IN, 'TELDIAM', TELDIAM, 1, NDUMMY)
         CALL DATPUTR (OUT, 'TELDIAM', TELDIAM, 1)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

