C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgscale.f	1.6    11/7/94
C
      SUBROUTINE SDEMAIN
C
CD Program to scale an image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 21 1992
C	Beam support stuff added
C				D.S.Briggs	Mar, 1993
C	Toss inputs into the history
C				D.S.Briggs	March 22 1994
C	And added BUNIT input
C				D.S.Briggs	March 22 1994
C	Tweaked beamfit selection
C				D.S.Briggs	June 14 1994
C	Added SAOimage style scaling
C				D.S.Briggs	Aug 4 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSCALE')
C
      CHARACTER*(SYSMXNAM) 	INFILE,
     2                          OUTFILE, BEAMFILE
      INTEGER		NDUMMY, IADD, N
      REAL		SCALE, OFFSET, MAX, SUM, BPPB, BEAM(4), SPAR,
     $   		SMIN, SMAX, PIXR(2)
      CHARACTER*(SYSMXNAM)	NORM, BUNIT, FITALG, SAOMODE
C
      INTEGER		DATADD, ARRNPIX, STRLEN
      REAL		DATFGETR
C==================================================================
C
      CALL MSGWELCO ('I scale images')
      CALL USRCTL
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETR ('Offset', OFFSET, 1, NDUMMY)
      CALL USRGETC ('Normalize', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, NORM)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('BUNIT', BUNIT, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('PixRange', PIXR, 2, NDUMMY)
      CALL USRGETC ('SAOmode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, SAOMODE)
      CALL USRGETR ('SPar', SPAR, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      IF ((NORM(1:1).NE.' ').AND.(NORM(1:1).NE.'I').AND.
     $    (NORM(1:1).NE.'P').AND.(NORM(1:1).NE.'B')) THEN
         CALL MSGPUT ('No prescaling will be used','W')
         NORM = ' '
      END IF
C
      IF ((SAOMODE(1:3).NE.'LIN').AND.(SAOMODE(1:3).NE.'LOG').AND.
     $    (SAOMODE(1:4).NE.'SQRT').AND.(SAOMODE(1:4).NE.'WRAP').AND.
     $    (SAOMODE(1:4).NE.'2LOG').AND.(SAOMODE.NE.' ')) THEN
         MESSAGE = 'Unknown SAOMODE ' // SAOMODE(1:STRLEN(SAOMODE))
         CALL MSGPUT (MESSAGE, 'E')
         GO TO 999
      END IF
C
C Get Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
C
C Dig out beam parameters if needed
C
      IF (NORM(1:1).EQ.'B') THEN
         CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'BEAM')
      END IF
C
C Call main routine
C
      CALL IMGCLONE ('Image', 'Output')
      IF (NORM(1:1).EQ.'P') THEN
         CALL MSGPUT ('Prescaling image to unit maximum','I')
         CALL ARRSTAT ('Image',' ')
         MAX = DATFGETR ('Image','ARRMAX')
         SCALE = SCALE / MAX
      ELSE IF (NORM(1:1).EQ.'I') THEN
         CALL MSGPUT ('Prescaling image to unit sum','I')
         CALL ARRSTAT ('Image',' ')
         SUM = DATFGETR ('Image','ARRSUM')
         SCALE = SCALE / SUM
      ELSE IF (NORM(1:1).EQ.'B') THEN
         CALL IMGP2PB ('Image', BEAM, 'Image')
         BPPB = DATFGETR ('Image', 'BPPB')
 1000    FORMAT ('Prescaling image by',F9.4,' pixels per beam')
         WRITE (MESSAGE, 1000) BPPB
         CALL MSGPUT (MESSAGE,'I')
      END IF
C
C SAOimage style scaling
C
      IF (SAOMODE.NE.' ') THEN
         CALL ARRCVTR ('Image', 'Image')
         IADD = DATADD ('Image')
         N = ARRNPIX ('Image')
         IF (SAOMODE(1:4).EQ.'2LOG') THEN
            CALL ARRABS ('Image','AbsImage')
            CALL ARRSTAT ('AbsImage',' ')
            IF ((PIXR(1).EQ.0.0).AND.(PIXR(2).EQ.0.0)) THEN
               SMIN = DATFGETR ('AbsImage','ARRMIN')
               SMAX = DATFGETR ('AbsImage','ARRMAX')
            ELSE
               SMIN = PIXR(1)
               SMAX = PIXR(2)
               CALL ARRCLP2Z ('Image', SMIN, SMAX, 'Image')
            END IF
            CALL DATDELET ('AbsImage')
         ELSE
            CALL ARRSTAT ('Image',' ')
            IF ((PIXR(1).EQ.0.0).AND.(PIXR(2).EQ.0.0)) THEN
               SMIN = DATFGETR ('Image','ARRMIN')
               SMAX = DATFGETR ('Image','ARRMAX')
            ELSE
               SMIN = PIXR(1)
               SMAX = PIXR(2)
               CALL ARRCLIP ('Image', SMIN, SMAX, 'Image')
            END IF
         END IF
         CALL PIXSAOSCL (MEMR(IADD), MEMR(IADD), N, SMIN, SMAX,
     $      SAOMODE, SPAR)
      END IF
C
C Normal arithmatic scaling
C
      CALL ARRSCALE ('Image', SCALE, OFFSET, 'Output')
C
      IF (BUNIT.NE.' ') CALL DATPUTC ('Output', 'BUNIT', BUNIT, 1)
      CALL HISINPUT ('Output')
C
C Write result 
C
      CALL FILIMGPU('Output', OUTFILE, ' ')
C
  999 CONTINUE
      END
