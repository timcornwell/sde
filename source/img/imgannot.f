C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgannot.f	1.2    11/7/94
C
      SUBROUTINE IMGANNOT (INFILE)
C
CD Annotate an imgplot image
C
C	INFILE CH*(*)	input	Name of annotation file
C
C Grandiosely speaking, this routine interprets a rock stupid
C graphics language, for the purpose of providing publication ready
C overlays on imgplot output.
C
C The assumption is that all of the graphics housekeeping has been
C done, and we simply call the appropriate PGPLOT primitives.
C
C The only lines that this routine understands are of the form
C # <comment text>
C SLS n    ( Set Line Style n = 1-5 )
C SLW n    ( Set Line Width n = 0-201 )
C SCI n    ( Set Color Index n = 0 for background, 1 for foreground )
C SCH r    ( Set Character Height )
C TEXT x y
C <text here> ( Annotation.  x, y in world coords )
C PTEXT x y angle fjust
C <text here> ( Annotation.  x, y in world coords.  angle in degrees. )
C MTEXT disp coord fjust
C side        ( B L T R LV or RV )
C <text here> ( Label annotation.  See PGPLOT docs for PGMTEXT )
C LINE
C x1 y1
C x2 y2
C  ...
C xn yn
C <blank line or EOF>
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 24 1994
C	Added PTEXT primitive
C				D.S.Briggs	Aug 29 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGANNOT')
C
      REAL	X, Y, R, CH, DISP, COORD, FJUST, ANGLE
      INTEGER	NLINES, LENGTH, XADD, YADD, N, LS, LW, CI, NPTS
      LOGICAL	EOF, INLINE, GOTLS, GOTLW, GOTCI, GOTCH
      CHARACTER	SIDE*2
C
      INTEGER	STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL PGQLS (LS)
      CALL PGQLW (LW)
      CALL PGQCI (CI)
      CALL PGQCH (CH)
      GOTLS = .FALSE.
      GOTLW = .FALSE.
      GOTCI = .FALSE.
      GOTCH = .FALSE.
C
      CALL TXTOPEN ('Annotate', INFILE, 'READ')
      IF (ERROR) GO TO 990
      NLINES = 0
 100  CONTINUE
      CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
      IF (.NOT.EOF) THEN
         NLINES = NLINES + 1
         GO TO 100
      END IF
      CALL TXTCLOSE ('Annotate')
      CALL DATMAKAR ('Ann-X', 1, NLINES, 'R', XADD)
      CALL DATMAKAR ('Ann-Y', 1, NLINES, 'R', YADD)
      CALL TXTOPEN ('Annotate', INFILE, 'READ')
      INLINE = .FALSE.
C
C Main Loop
C
 200  CONTINUE
      CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
C
      IF (STRBUF(1:1).EQ.'#') GO TO 200
C
      IF ((STRBUF.EQ.' ').OR.EOF) THEN
         IF (INLINE) THEN
            CALL PGLINE (NPTS, MEMR(XADD), MEMR(YADD))
            INLINE = .FALSE.
         END IF
         IF (EOF) GO TO 500
         GO TO 200
      END IF
C
      IF (INLINE) THEN
         READ (STRBUF, *) X, Y
         MEMR(XADD+NPTS) = X
         MEMR(YADD+NPTS) = Y
         NPTS = NPTS + 1
         GO TO 200
      END IF
C
      MESSAGE(1:5) = STRBUF(1:5)
      CALL STRUC (STRBUF(1:5), MESSAGE(1:5))
C
      IF (STRBUF(1:4).EQ.'LINE') THEN
         NPTS = 0
         INLINE = .TRUE.
         GO TO 200
      END IF
C
      IF (STRBUF(1:3).EQ.'SLS') THEN
         READ (STRBUF(4:),*) N
         CALL PGSLS (N)
         GOTLS = .TRUE.
         GO TO 200
      END IF
C
      IF (STRBUF(1:3).EQ.'SLW') THEN
         READ (STRBUF(4:),*) N
         CALL PGSLW (N)
         GOTLW = .TRUE.
         GO TO 200
      END IF
C
      IF (STRBUF(1:3).EQ.'SCI') THEN
         READ (STRBUF(4:),*) N
         CALL PGSCI (N)
         GOTCI = .TRUE.
         GO TO 200
      END IF
C
      IF (STRBUF(1:3).EQ.'SCH') THEN
         READ (STRBUF(4:),*) R
         CALL PGSCH (R)
         GOTCH = .TRUE.
         GO TO 200
      END IF
C
      IF (STRBUF(1:4).EQ.'TEXT') THEN
         READ (STRBUF(5:),*) X, Y
         CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
         CALL PGTEXT (X, Y, STRBUF(1:STRLEN(STRBUF)))
         GO TO 200
      END IF
C
      IF (STRBUF(1:5).EQ.'PTEXT') THEN
         READ (STRBUF(6:),*) X, Y, ANGLE, FJUST
         CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
         CALL PGPTEXT (X, Y, ANGLE, FJUST, STRBUF(1:STRLEN(STRBUF)))
         GO TO 200
      END IF
C
      IF (STRBUF(1:5).EQ.'MTEXT') THEN
         READ (STRBUF(6:),*) DISP, COORD, FJUST
         CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
         SIDE = STRBUF(1:2)
         CALL TXTREAD ('Annotate', STRBUF, LENGTH, EOF)
         CALL PGMTEXT (SIDE, DISP, COORD, FJUST,
     $      STRBUF(1:STRLEN(STRBUF)))
         GO TO 200
      END IF
C
      CALL MSGPUT ('Line is','E')
      CALL MSGPUT (STRBUF(1:STRLEN(STRBUF)),'E')
      CALL ERRREPOR (ERRFATAL, ROUTINE, 'Unrecognized line')
      GO TO 999
C
C End of main loop
C
 500  CONTINUE
C
      CALL TXTCLOSE ('Annotate')
      CALL DATDELET ('Ann-X')
      CALL DATDELET ('Ann-Y')
C
      IF (GOTLS) CALL PGSLS (LS)
      IF (GOTLW) CALL PGSLW (LW)
      IF (GOTCI) CALL PGSCI (CI)
      IF (GOTCH) CALL PGSCH (CH)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
