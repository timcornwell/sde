C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img2jy.f	1.1	 6/7/93
C
      SUBROUTINE IMG2JY (IMG, OUT)
C
CD Force units of image to Jy
C
C	IMG	CH*(*)	input	Name of image to be scaled
C	OUT	CH*(*)	input	Name of rescaled image
C
C Audit trail:
C	Original version:
C				D.S. Briggs	28 Oct 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, OUT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG2JY')
C
      INTEGER		NDUMMY
      REAL		BEAM(4)
C
      REAL		DATFGETR
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      IF (DATEXIST(STRM2(IMG,'BUNIT'))) THEN
         CALL DATGETC ('Image', 'BUNIT', STRBUF, 1, NDUMMY)
         IF (STRBUF.EQ.'JY/BEAM') THEN
            BEAM(1) = DATFGETR (IMG, 'BMAJ') * 3600.0
            BEAM(2) = DATFGETR (IMG, 'BMIN') * 3600.0
            BEAM(3) = DATFGETR (IMG, 'BPA')
            BEAM(4) = DATFGETR (IMG, 'BZ') * 3600.0
            CALL IMGPB2P (IMG, BEAM, OUT)
         END IF
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
