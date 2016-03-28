C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vismul.f	1.1    2/6/95
C
      SUBROUTINE SDEMAIN
C
CD Program to combine visibility data in a produce
C
C This task makes no attempt to determine if the actual visibility data
C lines up or not.  It simply combines the visibility arrays element by
C element.  The coordinate data and output weights are taken from the
C first vis file, though flagged elements from vis2 & vis3 are flagged
C in the output.
C
C Forms the product Scale * Vis1 * Vis2 / Vis3
C
C If Vis2 or Vis3 are absent, they are taken to be unity.
C
C Audit trail:
C	New task
C				D.S.Briggs	May 31 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISMUL')
C
      CHARACTER*(SYSMXNAM)	VISFILE1, VISFILE2, VISFILE3,
     $   		OUTFILE, STOKES
      REAL		SCALE
      INTEGER		NDUMMY, NSTOKES, I
C
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I combine visibility data in a product')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis1', VISFILE1, 1, NDUMMY)
      CALL USRGETC('Vis2', VISFILE2, 1, NDUMMY)
      CALL USRGETC('Vis3', VISFILE3, 1, NDUMMY)
      CALL USRGETR('Scale', SCALE, 1, NDUMMY)
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETC('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, STOKES)
C
C Get the visibility files
C
      NSTOKES = STRLEN(STOKES)
      CALL VISGET ('Vis1', VISFILE1, STOKES, '*', ' ')
      IF (VISFILE2.NE.' ')
     $   CALL VISGET ('Vis2', VISFILE2, STOKES, '*', ' ')
      IF (VISFILE3.NE.' ')
     $   CALL VISGET ('Vis3', VISFILE3, STOKES, '*', ' ')
      IF (ERROR) GO TO 999
C
      IF (SCALE.NE.1.0) THEN
         DO 100 I = 1, NSTOKES
            CALL ARRSCALE ('Vis1/OBS/'//STOKES(I:I)//'/VIS',
     $         SCALE, 0.0, 'Vis1/OBS/'//STOKES(I:I)//'/VIS')
 100     CONTINUE
      END IF
C
      IF (VISFILE2.NE.' ') THEN
         DO 200 I = 1, NSTOKES
            CALL ARRMULT ('Vis1/OBS/'//STOKES(I:I)//'/VIS',
     $         'Vis2/OBS/'//STOKES(I:I)//'/VIS',
     $         'Vis1/OBS/'//STOKES(I:I)//'/VIS')
            CALL ARRFLAG ('Vis1/OBS/'//STOKES(I:I)//'/WT',
     $         'Vis2/OBS/'//STOKES(I:I)//'/WT',
     $         'Vis1/OBS/'//STOKES(I:I)//'/WT')
 200     CONTINUE
      END IF
C
      IF (VISFILE3.NE.' ') THEN
         DO 300 I = 1, NSTOKES
            CALL ARRDIV ('Vis1/OBS/'//STOKES(I:I)//'/VIS',
     $         'Vis3/OBS/'//STOKES(I:I)//'/VIS',
     $         'Vis1/OBS/'//STOKES(I:I)//'/VIS')
            CALL ARRFLAG ('Vis1/OBS/'//STOKES(I:I)//'/WT',
     $         'Vis3/OBS/'//STOKES(I:I)//'/WT',
     $         'Vis1/OBS/'//STOKES(I:I)//'/WT')
 300     CONTINUE
      END IF
C
C Trim the output to avoid a zillion AIPS records.
C
      CALL DATRENAM ('Vis1', 'Output')
      CALL DATDELET ('Output/HII')
      CALL HISINPUT ('Output')
C
C Put the visibility file
C
      CALL VISPUT ('Output', OUTFILE, 'OBS', STOKES, '*', ' ')
C
 999  CONTINUE
      END
