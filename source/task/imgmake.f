C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmake.f	1.2    6/5/93
C
      SUBROUTINE SDEMAIN
C
CD Program to create an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMAKE')
C
      INTEGER		IMSIZE(3)
      REAL		CELLSIZE(3), SHIFT(2), S
      DOUBLE PRECISION	RA, DEC, FREQ
      CHARACTER*1	MATYPE
      REAL		VALUE
      CHARACTER*(SYSMXNAM) 	PGMFILE, OUTFILE, IMGFILE
C
      REAL		RRA(3), RDEC(3)
      CHARACTER*15	ARA, ADEC
      INTEGER		NDUMMY
C
      INTEGER		STRLEN
C==================================================================
C
      CALL MSGWELCO ('I create images')
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 2, NDUMMY)
      CALL USRGETR ('RA', RRA, 3, NDUMMY)
      CALL USRGETR ('Dec', RDEC, 3, NDUMMY)
      CALL USRGETD ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETC ('Matype', STRBUF, 1, NDUMMY)
      MATYPE = STRBUF(1:1)
      CALL USRGETR ('Value', VALUE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('PGM', PGMFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
C
      FREQ = FREQ * 1.D6
      CALL CRDRD2D (RRA, RDEC, RA, DEC)
      IF (MATYPE.EQ.' ') MATYPE = 'R'
C
      IF (IMGFILE.EQ.' ') THEN
         CALL IMGMAKE0 ('Img', IMSIZE, CELLSIZE, SHIFT, RA, DEC, FREQ,
     $      MATYPE)
         CALL CRDD2RD (RA, DEC, RRA, RDEC, ARA, ADEC)
         WRITE (MESSAGE, 1000) ARA(1:STRLEN(ARA)), ADEC(1:STRLEN(ADEC))
 1000    FORMAT ('Image ref is RA = ',A,'  Dec = ',A)
         CALL MSGPUT (MESSAGE,'I')
      ELSE
         CALL FILIMGGE ('Img', IMGFILE, ' ')
      END IF
C
      CALL ARRSETCO ('Img', 0.0, VALUE)
      IF (PGMFILE.NE.' ') THEN
         CALL FILPGMGE ('Img', PGMFILE)
      END IF
C
C Write result 
C
      CALL FILIMGPU ('Img', OUTFILE, ' ')
C
      END
