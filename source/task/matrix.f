C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matrix.f	1.1    6/16/94
C
      SUBROUTINE SDEMAIN
C
CD Program to manipulate matricies
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	June 15 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATRIX')
C
      CHARACTER*(SYSMXNAM)	MAT1, MAT2, MATOUT, OPCODE, GOCODE
      INTEGER		NDUMMY
C==================================================================
      CALL MSGWELCO ('I manipulate matricies')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC ('MAT1', MAT1, 1, NDUMMY)
      CALL USRGETC ('MAT2', MAT2, 1, NDUMMY)
      CALL USRGETC ('Output', MATOUT, 1, NDUMMY)
      CALL USRGETC ('OpCode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, OPCODE)
      CALL USRGETGO (STRBUF)
      CALL STRUC (STRBUF, GOCODE)
C
      IF (GOCODE.NE.' ') THEN
         IF (OPCODE.NE.' ')
     $      CALL MSGPUT ('Overriding OPCODE with GOCODE','W')
         OPCODE = GOCODE
      END IF
      IF (OPCODE.EQ.' ')
     $   CALL ERRREPOR (ERRFATAL, ROUTINE, 'Null OPCODE!')
C
      IF (OPCODE(1:1).EQ.'T') THEN
         CALL MSGPUT ('Transposing MAT1 giving OUTPUT','I')
         CALL FILIMGGE ('Mat1', MAT1, ' ')
         CALL MATTRANS ('Mat1', 'Output')
      ELSE IF (OPCODE(1:1).EQ.'F') THEN
         CALL MSGPUT (
     $      'Flipping MAT1 about first axis giving OUTPUT','I')
         CALL FILIMGGE ('Mat1', MAT1, ' ')
         CALL MATFLIP ('Mat1', 'Output')
      ELSE IF (OPCODE(1:1).EQ.'M') THEN
         CALL MSGPUT ('Multiplying MAT1 by MAT2 giving OUTPUT','I')
         CALL FILIMGGE ('Mat1', MAT1, ' ')
         CALL FILIMGGE ('Mat2', MAT2, ' ')
         CALL MATMULT ('Mat1', 'Mat2', 'Output')
      ELSE
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Unrecognized OPCODE')
      END IF
C
C Output result
C
      CALL MATIMHDR ('Output')
      CALL FILIMGPU ('Output', MATOUT, ' ')
C
      END
