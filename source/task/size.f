C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)size.f	1.1 7/15/92
C
      SUBROUTINE SDEMAIN
C
CD Program to find characteristic size in an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	July 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIZE')
C
      INTEGER 		NDUMMY
      REAL		ITHRES, CTHRES, DATFGETR
      CHARACTER*(SYSMXNAM)	INFILE, SMFILE
C==================================================================
      CALL MSGWELCO ('I find characteristic sizes in an image')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETR ('IThreshold', ITHRES, 1, NDUMMY)
      CALL USRGETR ('CThreshold', CTHRES, 1, NDUMMY)
      CALL USRGETC ('Output', SMFILE, 1, NDUMMY)
C
C Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL IMGCLONE ('Image', 'Size')
C
      CALL IMGCURV ('Image', 'Size', 'Swork')
C
C Now find size
C
      IF(ITHRES.LE.0.0) THEN
         IF(ITHRES.EQ.0.0) ITHRES=0.1
         CALL ARRSTAT ('Image', ' ')
         ITHRES=MAX(0.0,DATFGETR('Image', 'ARRMAX')*ABS(ITHRES))
      END IF
      IF(CTHRES.LE.0.0) THEN
         IF(CTHRES.EQ.0.0) CTHRES=0.1
         CALL ARRSTAT ('Size', ' ')
         CTHRES=MAX(0.0,DATFGETR('Size', 'ARRMAX')*ABS(CTHRES))
      END IF
      WRITE (MESSAGE, 1000) ITHRES
 1000 FORMAT ('Clipping image at ',1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1000) CTHRES
 1100 FORMAT ('Clipping image at ',1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
      CALL ARRCLIP2 ('Image', 'Image', ITHRES, 0.0, 1E20, 1E20, 'Image')
      CALL ARRCLIP2 ('Size', 'Size', CTHRES, 0.0, 1E20, 1E20, 'Size')
      CALL ARRDIV ('Image', 'Size', 'Size')
      CALL ARRSQRT ('Size', 'Size')
      CALL ARRSCALE ('Size', 4.0*SQRT(LOG(2.0)), 0.0, 'Size')
      CALL DATPUTC ('Size', 'BUNIT', 'ARCSEC', 1)
C
C Write out answer
C
      CALL FILIMGPU ('Size', SMFILE, ' ')
C
 999  CONTINUE
      END
