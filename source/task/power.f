C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)power.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to make power-law shaped images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'POWER')
C
      INTEGER 		NDUMMY, SEED
      REAL		INNER, OUTER, POWER, FLUX, AFLUX, DATFGETR,
     $			AMIN
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE
C==================================================================
      CALL MSGWELCO ('I make power law shaped images')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', SMFILE, 1, NDUMMY)
      CALL USRGETR ('Inner', INNER, 1, NDUMMY)
      CALL USRGETR ('Outer', OUTER, 1, NDUMMY)
      CALL USRGETR ('Power', POWER, 1, NDUMMY)
      CALL USRGETR ('Flux', Flux, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Make noise field
C
      CALL MSGPUT ('Making initial noise image', 'I')
      CALL ARRSETCO ('Image', 0.0, 0.0)
      CALL ARRGAUSS ('Image', 'Image', 1.0, SEED)
C
C Smooth
C         
      CALL MSGPUT ('Filtering noise image', 'I')
      CALL IMGPOWF ('Image', INNER, OUTER, POWER, 'Image', 'Swork')
C
C Adjust to have correct flux
C
      CALL MSGPUT ('Normalizing to desired flux', 'I')
      CALL ARRSTAT ('Image', ' ')
      AMIN = DATFGETR ('Image', 'ARRMIN')
      CALL ARRSCALE ('Image', 1.0, -AMIN, 'Image')
      CALL ARRSTAT ('Image', ' ')
      AFLUX = DATFGETR ('Image', 'ARRSUM')
      WRITE (MESSAGE, 1000) AFLUX
 1000 FORMAT ('Flux before scaling = ',1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
      CALL ARRSCALE ('Image', FLUX / AFLUX, 0.0, 'Image')
      CALL ARRSTAT ('Image', ' ')
      AFLUX = DATFGETR ('Image', 'ARRSUM')
      WRITE (MESSAGE, 1100) AFLUX
 1100 FORMAT ('Flux after scaling = ',1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
C
C Write out answer
C
      CALL FILIMGPU ('Image', SMFILE, ' ')
C
 999  CONTINUE
      END
