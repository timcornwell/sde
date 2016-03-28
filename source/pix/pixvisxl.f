C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixvisxl.f	1.1    6/7/93
C
      SUBROUTINE PIXVISXL (VIS, WT, U, V, W, BASL, TIME, NVIS,
     $   EXCEL, DOALL, DOVAR)
C
CD Dump a vis file to an Excel file, pixel level
C
C	VIS	CMPLX(*) input	Visibilities
C	WT	REAL(*) input	weights
C	U	REAL(*) input	spacings
C	V	REAL(*) input	"
C	W	REAL(*) input	"
C	BASL	REAL(*) input	antennas
C	TIME	REAL(*) input	time
c	NVIS	INT	input	number of visibilities
C	EXCEL	CHAR*(*) input	Handle for EXCEL file
C	DOALL	LOG	input	Write all visibilities?
C	DOVAR	LOG	input	Write Gaussfit style variances?
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 17 1993
C	Added DOVAR option
C				D.S.Briggs	28 May 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS
      COMPLEX		VIS(NVIS)
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS),
     $   		BASL(NVIS), TIME(NVIS)
      CHARACTER*(*)	EXCEL
      LOGICAL		DOALL, DOVAR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXVISXL')
C
      INTEGER		I, IA1, IA2
      CHARACTER		TAB*1, NUMBUF*20
C
      INTEGER		STRLEN, STRSTART
C=====================================================================
      IF (ERROR) GO TO 999
C
      TAB = CHAR(9)
 1000 FORMAT (I12)
 1010 FORMAT (1PE20.9)
      DO 100 I = 1, NVIS
         IF (DOALL.OR.WT(I).GT.0) THEN
            IA1 = NINT(BASL(I)/256.0)
            IA2 = NINT(BASL(I)-FLOAT(256*IA1))

            WRITE (NUMBUF, 1000) IA1
            MESSAGE = NUMBUF(STRSTART(NUMBUF):12)

            WRITE (NUMBUF, 1000) IA2
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):12)

            WRITE (NUMBUF, 1010) REAL(VIS(I))
            MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            WRITE (NUMBUF, 1010) IMAG(VIS(I))
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            WRITE (NUMBUF, 1010) WT(I)
            MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            IF (DOVAR) THEN
               
               WRITE (NUMBUF, 1010) 0.5/WT(I)
               STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):20)
               
               WRITE (NUMBUF, 1010) 0.5/WT(I)
               MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $            NUMBUF(STRSTART(NUMBUF):20)
               
            END IF

            WRITE (NUMBUF, 1010) U(I)
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            WRITE (NUMBUF, 1010) V(I)
            MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            WRITE (NUMBUF, 1010) W(I)
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            WRITE (NUMBUF, 1010) TIME(I)
            MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $         NUMBUF(STRSTART(NUMBUF):20)

            CALL TXTWRITE(EXCEL, MESSAGE)

         END IF
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
