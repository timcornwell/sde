C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2drxl.f	1.1    1/25/93
C
      SUBROUTINE PIX2DRXL (IMG, MASK, N1, N2, RPIX1, RPIX2, DELT1,
     $   DELT2, EXCEL)
C
CD Dump a 2D real image to an Excel file, pixel level
C
C	IMG	REAL(*)	input	Image
C	MASK	REAL(*)	input	Mask
C	N1	INT	input	Size of Image and Mask
C	N2	INT	input	 "   "    "    "   "
C	RPIX1	REAL	input	Reference pixel on first axis
C	RPIX2	REAL	input	    "       "   "  second "
C	DELT1	REAL	input	Delta coord on first axis
C	DELT2	REAL	input	  "     "   "  second "
C	EXCEL	CHAR*(*) input	Handle for EXCEL file
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Apr 11 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N1, N2
      REAL		IMG(N1,N2), MASK(N1,N2), RPIX1, RPIX2,
     $   		DELT1, DELT2
      CHARACTER*(*)	EXCEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DRXL')
C
      INTEGER		I, J
      CHARACTER		TAB*1, NUMBUF*20
C
      INTEGER		STRLEN, STRSTART
C=====================================================================
      IF (ERROR) GO TO 999
C
      TAB = CHAR(9)
 1000 FORMAT (I12)
 1010 FORMAT (1PE20.9)
      DO 110 J = 1, N2
         DO 100 I = 1, N1
            IF (MASK(I,J) .NE. 0.0) THEN
               WRITE (NUMBUF, 1000) I
               MESSAGE = NUMBUF(STRSTART(NUMBUF):12)
               WRITE (NUMBUF, 1000) J
               STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):12)
               WRITE (NUMBUF, 1010) (REAL(I)-RPIX1) * DELT1 * 3600.0
               MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):20)
               WRITE (NUMBUF, 1010) (REAL(J)-RPIX2) * DELT2 * 3600.0
               STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):20)
               WRITE (NUMBUF, 1010) IMG(I,J)
               MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):20)
               CALL TXTWRITE(EXCEL, MESSAGE)
            END IF
 100     CONTINUE
 110  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
