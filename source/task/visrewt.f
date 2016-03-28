C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrewt.f	1.5    2/21/95
C
      SUBROUTINE SDEMAIN
C
CD Program to reweight visibility data
C
C Audit trail:
C	Cloned from uvmap
C				D.S.Briggs	Mar 3 1993
C	Added NEGATIVE option
C				D.S.Briggs	Sept 8 1993
C	Added DTScale option.  (Set the weights according to diameter
C	and integration time.
C				D.S.Briggs	Nov 2 1993
C	Very slight reorganization.  Add inputs to output file.
C				D.S.Briggs	Oct 7 1994
C	Added minimum allowed weight.  (Thresholding)
C				D.S.Briggs	Feb 21 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISREWT')
C
      REAL		TIME(2), UVLIMITS(2), SCALE, OFFSET, RANDOM,
     $   		WTMIN, WTMAX, WTAVE, WTSUM, DT(3)
      INTEGER 		NDUMMY, NSEL, NTOT, TIMR(8), ANT(8), BAS(8),
     $   		NANT, NBAS, I, SEED, NPOSWT, DTNANT, DTMXANT,
     $   		WTTHRSH
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE, STOKES, SUBCLASS,
     $   		TABLES, FILESYS, WTARR, ANTFILE
      LOGICAL		DONEG
C
      CHARACTER*(SYSMXNAM)	STRM3
      INTEGER		DATFGETI, STRLEN
      REAL		DATFGETR
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I reweight visibility data')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Tables', TABLES, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETI ('Antennas', ANT, 8, NDUMMY)
      CALL USRGETI ('Baseline', BAS, 8, NDUMMY)
      CALL USRGETL ('Negative', DONEG, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETR ('Offset', OFFSET, 1, NDUMMY)
      CALL USRGETR ('DTScale', DT, 3, NDUMMY)
      CALL USRGETC ('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETR ('Random', RANDOM, 1, NDUMMY)
      CALL USRGETR ('WtThresh', WTTHRSH, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Subclass to be modified
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
      WTARR = STRM3('Vis',SUBCLASS,'WT')
C
C Now get vis data
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', TABLES)
      IF (ERROR) GO TO 999
C
      CALL FILSYSEX (OUTFILE, FILESYS)
      IF ((FILESYS.EQ.'FTS').AND.(TABLES.EQ.'*')) THEN
         CALL MSGPUT
     $      ('Sorry -- I don''t accept * for FITS table output','E')
         CALL MSGPUT ('Rerun with an explicit table list','E')
         GO TO 999
      END IF
C
C Print statistics of weights
C
      CALL ARRSTAT(WTARR, ' ')
      IF (ERROR) GO TO 999
C
      WTMIN = DATFGETR(WTARR, 'ARRMIN')
      WTMAX = DATFGETR(WTARR, 'ARRMAX')
      WTAVE = DATFGETR(WTARR, 'ARRAVE')
      WTSUM = DATFGETR(WTARR, 'ARRSUM')
C
      CALL MSGPUT ('Input weights  min, max, avg, sum','I')
      WRITE (MESSAGE, 1000) WTMIN, WTMAX, WTAVE, WTSUM
 1000 FORMAT (3F10.4,F12.2)
      CALL MSGPUT (MESSAGE,'I')
C
      CALL ARRCLIP(STRM3('Vis',SUBCLASS,'WT'), 0.0, 1.E10, 'TMP-CLIP')
      CALL ARRDIV('TMP-CLIP', 'TMP-CLIP', 'TMP-WINDOW')
      CALL ARRSCOPY('TMP-CLIP', 'TMP-WINDOW', 'POS-WT')
      CALL ARRSTAT ('POS-WT', ' ')
      IF (ERROR) GO TO 999
C
      NPOSWT = DATFGETI ('POS-WT', 'ARRNLOC')
      WRITE (MESSAGE, 1010) NPOSWT
 1010 FORMAT (I7, ' strictly positive weights')
      CALL MSGPUT (MESSAGE,'I')
C
      WTMIN = DATFGETR('POS-WT','ARRMIN')
      WTMAX = DATFGETR('POS-WT','ARRMAX')
      WTAVE = DATFGETR('POS-WT','ARRAVE')
      WTSUM = DATFGETR('POS-WT','ARRSUM')
C
      CALL MSGPUT ('Input positive weights  min, max, avg, sum','I')
      WRITE (MESSAGE, 1000) WTMIN, WTMAX, WTAVE, WTSUM
      CALL MSGPUT (MESSAGE,'I')
      CALL MSGPUT (' ','I')
C
      CALL DATDELET ('TMP-CLIP')
      CALL DATDELET ('TMP-WINDOW')
      CALL DATDELET ('POS-WT')
C
C When passed to the subroutines, a negative value for NANT or NBAS will
C  mean to reverse the matching of the set.  That is, NANT < 0 will mean
C  match all antennas *not* in that set, and similarly for NBAS.
C
      NANT = 0
      NBAS = 0
      DO 10 I = 1, 8
         IF (ANT(I).NE.0) NANT = I
         IF (BAS(I).NE.0) NBAS = I
 10   CONTINUE
      DO 20 I = 1, 8
         IF (ANT(I).LT.0) THEN
            ANT(I) = ABS(ANT(I))
            NANT = -ABS(NANT)
         END IF
         IF (BAS(I).LT.0) THEN
            BAS(I) = ABS(BAS(I))
            NBAS = -ABS(NBAS)
         END IF
 20   CONTINUE
C
      IF(DATEXIST('Vis/TIME').AND.DATEXIST('Vis/BASELINE')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISRWT ('Vis', SUBCLASS, TIME, UVLIMITS, ANT, NANT,
     $      BAS, NBAS, DONEG, SCALE, OFFSET, NSEL, NTOT, RANDOM, SEED,
     $      WTTHRSH)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1100) NSEL, NTOT
 1100    FORMAT ('Reweighted',I7,' of',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME or BASELINE information', 'E')
         GO TO 999
      END IF
C
C Reweight by integration time and antenna diameter if requested
C
      IF (ANTFILE.NE.' ') THEN
         CALL MSGPUT (' ', 'I')
         WRITE (MESSAGE, 1105) ANTFILE(1:STRLEN(ANTFILE))
 1105    FORMAT ('Reading antenna diameters from file ',A)
         CALL MSGPUT (MESSAGE, 'I')
         CALL FILGETAN ('Antennas', ANTFILE)
         DTNANT = DATFGETI('Antennas','NANT')
         WRITE (MESSAGE, 1110) DTNANT
 1110    FORMAT (I4, ' antennas in antenna file')
         CALL MSGPUT (MESSAGE, 'I')
         CALL VISMXANT ('Vis', NDUMMY, DTMXANT)
         WRITE (MESSAGE, 1120) DTMXANT
 1120    FORMAT ('Max antenna number in vis file is ',I3)
         CALL MSGPUT (MESSAGE, 'I')
         IF (DTMXANT.GT.DTNANT) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE,
     $         'Not enough antenna diameters!')
            GO TO 999
         END IF
         CALL VISRWTDT ('Vis', 'OBS/I', 'Antennas', DT)
      END IF
C
C Print statistics of output weights
C
      CALL ARRSTAT(WTARR, ' ')
      IF (ERROR) GO TO 999
C
      WTMIN = DATFGETR(WTARR, 'ARRMIN')
      WTMAX = DATFGETR(WTARR, 'ARRMAX')
      WTAVE = DATFGETR(WTARR, 'ARRAVE')
      WTSUM = DATFGETR(WTARR, 'ARRSUM')
C
      CALL MSGPUT (' ','I')
      CALL MSGPUT ('Output weights  min, max, avg, sum','I')
      WRITE (MESSAGE, 1200) WTMIN, WTMAX, WTAVE, WTSUM
 1200 FORMAT (3F10.4,F12.2)
      CALL MSGPUT (MESSAGE,'I')
C
      CALL ARRCLIP(STRM3('Vis',SUBCLASS,'WT'), 0.0, 1.E10, 'TMP-CLIP')
      CALL ARRDIV('TMP-CLIP', 'TMP-CLIP', 'TMP-WINDOW')
      CALL ARRSCOPY('TMP-CLIP', 'TMP-WINDOW', 'POS-WT')
      CALL ARRSTAT ('POS-WT', ' ')
      IF (ERROR) GO TO 999
C
      NPOSWT = DATFGETI ('POS-WT', 'ARRNLOC')
      WRITE (MESSAGE, 1210) NPOSWT
 1210 FORMAT (I7, ' strictly positive output weights')
      CALL MSGPUT (MESSAGE,'I')
C
      WTMIN = DATFGETR('POS-WT','ARRMIN')
      WTMAX = DATFGETR('POS-WT','ARRMAX')
      WTAVE = DATFGETR('POS-WT','ARRAVE')
      WTSUM = DATFGETR('POS-WT','ARRSUM')
C
      CALL MSGPUT ('Output positive weights  min, max, avg, sum','I')
      WRITE (MESSAGE, 1000) WTMIN, WTMAX, WTAVE, WTSUM
      CALL MSGPUT (MESSAGE,'I')
C
      CALL DATDELET ('TMP-CLIP')
      CALL DATDELET ('TMP-WINDOW')
      CALL DATDELET ('POS-WT')
C
C History
C
      CALL HISINPUT ('Vis')
C
C Write output
C
      IF (OUTFILE.NE.' ')
     $   CALL VISPUT ('Vis', OUTFILE, 'OBS', STOKES(1:1), '*', TABLES)
C
 999  CONTINUE
      END



