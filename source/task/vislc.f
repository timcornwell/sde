C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vislc.f	1.1    6/12/94
C
      SUBROUTINE SDEMAIN
C
CD Program to combine visibility data in a linear sum
C
C This task makes no attempt to determine if the actual visibility data
C lines up or not.  It simply combines the visibility arrays element by
C element.  The coordinate data and output weights are taken from the
C first vis file.
C
C Audit trail:
C	New task
C				D.S.Briggs	May 31 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISLC')
C
      CHARACTER*(SYSMXNAM)	VISFILE1, VISFILE2, OUTFILE, STOKES
      REAL		WEIGHT1, WEIGHT2
      INTEGER		NDUMMY, NSTOKES, I
C
      INTEGER		STRLEN
C==================================================================
      CALL MSGWELCO ('I combine visibility data linearly')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Vis1', VISFILE1, 1, NDUMMY)
      CALL USRGETC('Vis2', VISFILE2, 1, NDUMMY)
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR('Weight1', WEIGHT1, 1, NDUMMY)
      CALL USRGETR('Weight2', WEIGHT2, 1, NDUMMY)
      CALL USRGETC('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, STOKES)
C
C Get the visibility files
C
      NSTOKES = STRLEN(STOKES)
      CALL VISGET ('Vis1', VISFILE1, STOKES, '*', ' ')
      CALL VISGET ('Vis2', VISFILE2, STOKES, '*', ' ')
C
      DO 100 I = 1, NSTOKES
         CALL ARRLC ('Vis1/OBS/'//STOKES(I:I)//'/VIS', WEIGHT1,
     $               'Vis2/OBS/'//STOKES(I:I)//'/VIS', WEIGHT2,
     $               'Vis1/OBS/'//STOKES(I:I)//'/VIS')
         IF (ERROR) GO TO 999
 100  CONTINUE
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
