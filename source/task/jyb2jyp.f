C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)jyb2jyp.f	1.2	 25 Jul 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to convert units from JY/BEAM to JY/PIXEL
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	July 20 1995
C	Correct BUNITS upon output
C				M.A. Holdaway	July 25 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'JYB2JYP')
C
      INTEGER 		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE, UNITSIN, UNITSOUT
      LOGICAL		DATEXIST
      REAL		BEAM(4)
C==================================================================
      CALL MSGWELCO ('I convert from K to JY/PIXEL or JY/BEAM')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('UnitsOut', UNITSOUT, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL DATGETC ('Image', 'BUNIT', UNITSIN, 1, NDUMMY)
C
C Do JY/BEAM
C
      IF (UNITSIN  .EQ. 'JY/BEAM'  .AND. 
     $     UNITSOUT .EQ.  'JY/PIXEL' ) THEN
         IF (BEAM(1) .LE. 0.0) THEN
            CALL DATGETR ('Image', 'BMAJ', BEAM(1), 1, NDUMMY)
            CALL DATGETR ('Image', 'BMIN', BEAM(2), 1, NDUMMY)
            CALL DATGETR ('Image', 'BPA',  BEAM(3), 1, NDUMMY)
            BEAM(1) = BEAM(1)*3600.0
            BEAM(2) = BEAM(2)*3600.0
            BEAM(4) = 0.0
         ENDIF
         CALL IMGPB2P ('Image', BEAM, 'Image')
         CALL DATPUTC ('Image', 'BUNIT', 'JY/PIXEL', 1)
      ELSE IF (UNITSOUT  .EQ. 'JY/BEAM'  .AND. 
     $     UNITSIN  .EQ.  'JY/PIXEL' ) THEN
         CALL IMGP2PB ('Image', BEAM, 'Image')
         CALL DATPUTC ('Image', 'BUNIT', 'JY/BEAM', 1)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Could not convert units')
      ENDIF
C
C Write out answer
C
      CALL FILIMGPU ('Image', OUTFILE, ' ')
C
 999  CONTINUE
      END
