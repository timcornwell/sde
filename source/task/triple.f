C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)triple.f	1.3    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to work with triple products
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Fixed VISPUT
C				T.J.Cornwell    Nov 1 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRIPLE')
C
      CHARACTER*(SYSMXNAM)	TRPFILE, MODFILE, NVISFILE, VISFILE
      INTEGER		NDUMMY
C=======================================================================
      CALL MSGWELCO ('I solve for visibilities from triple product'//
     $   ' data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Triple', TRPFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewVis', NVISFILE, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
      CALL TRPGET ('Triple', TRPFILE, 'I', '*', ' ')
C
C Now calculate model visibility
C
      CALL FILIMGGE ('Model', MODFILE, ' ')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL MSGPUT ('Transforming to get model visibilities', 'I')
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'Modvis', 'DModel')
C
C At this point, 'Vis' contains the observed amplitudes in 'OBS/I'
C and model visibilities in 'MOD/I'
C
C Do conversion from triple product data base to visibility
C
      CALL MSGPUT ('Solving for visibilities from triple products', 'I')
      CALL TRPTOVIS ('Triple', 'OBS/I', 'Vis', 'OBS/I', 'MOD/I',
     $   'OBS/I', ' ')
      IF (ERROR) GO TO 999
C
C Write answer
C
      IF (NVISFILE.NE.' ') THEN
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'I', '*', ' ')
      END IF
C
 999  CONTINUE
      END
