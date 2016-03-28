C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgave.f	1.1	 9/23/92
C
      SUBROUTINE SDEMAIN
C
CD Program to make a linear combination of two images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Sept 23 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGLC')
C
      CHARACTER*(SYSMXNAM) 	INFILE(16), OUTPUT
      INTEGER		NDUMMY, NIMAGES, I
      REAL			WEIGHT(16), SUMWT
C==================================================================
C
      CALL MSGWELCO ('I add images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Images', INFILE, 16, NIMAGES)      
      CALL USRGETR ('Weights', WEIGHT, 16, NDUMMY)
      CALL USRGETC ('Output', OUTPUT, 1, NDUMMY)
C
      SUMWT = 0.0
      DO 100 I = 1, NIMAGES
         IF (INFILE(I) .NE. ' ') THEN
            IF (I.EQ. 1) THEN
               CALL FILIMGGE ('Sum', INFILE(I), '*')
               CALL ARRSCALE ('Sum', WEIGHT(I), 0.0, 'Sum')
               SUMWT = SUMWT + WEIGHT(I)
            ELSE
               CALL FILIMGGE ('Image', INFILE(I), '*')
               CALL ARRLC ('Sum', 1.0, 'Image', WEIGHT(I), 'Sum')
               SUMWT = SUMWT + WEIGHT(I)
               CALL DATDELET ('Image')
            ENDIF
         ENDIF
 100  CONTINUE
      IF (SUMWT .LE. 0.0) THEN
         CALL ERRREPOR(ROUTINE, ERRLOGIC, 'No Weight!')
         GOTO 999
      ENDIF
      SUMWT = 1./SUMWT
      CALL ARRSCALE( 'Sum', SUMWT, 0.0, 'Sum')
C
      CALL FILIMGPU('Sum',OUTPUT,' ')
C
 999  CONTINUE
      END
