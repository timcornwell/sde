C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvrot.f	1.2    6/5/93
C
      SUBROUTINE SDEMAIN
C
CD Program to rotate position angle of u's and v's
C
C Audit trail:
C	Cloned from visrewt
C				D.S.Briggs	Apr 3 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVROT')
C
      REAL		TIME(2), UVLIMITS(2), THETA, RANDOM
      INTEGER 		NDUMMY, NSEL, NTOT, TIMR(8), ANT(8), BAS(8),
     $   		NANT, NBAS, I, SEED
      CHARACTER*(SYSMXNAM)	VISFILE, OUTFILE, STOKES, SUBCLASS,
     $   		TABLES, FILESYS, WTARR
C
      CHARACTER*(SYSMXNAM)	STRM3
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I rotate uv position angles')
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
      CALL USRGETR ('Theta', THETA, 1, NDUMMY)
      CALL USRGETR ('Random', RANDOM, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Subclass to be modified
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
      WTARR = STRM3('Vis',SUBCLASS,'WT')
C
C Now get relevant files
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
         CALL UVROTA ('Vis', SUBCLASS, TIME, UVLIMITS, ANT, NANT,
     $     BAS, NBAS, THETA, NSEL, NTOT, RANDOM, SEED)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1100) NSEL, NTOT
 1100    FORMAT ('Rotated',I7,' of',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME or BASELINE information', 'E')
         GO TO 999
      END IF
C
C Write output
C
      IF (OUTFILE.NE.' ')
     $   CALL VISPUT ('Vis', OUTFILE, 'OBS', STOKES(1:1), '*', TABLES)
C
 999  CONTINUE
      END

C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvrot.f	1.2    6/5/93
C
       SUBROUTINE UVROTA (NAME, SUB, TIMERANG, UVLIMITS,
     $   ANT, NANT, BAS, NBAS, THETA, NSEL, NTOT,
     $   RANDOM, SEED)
C
CD Rotate position angle of selected u's and u's by theta  [array level]
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	TIMERANG REAL(*) input	starttime, endtime
C	UVLIMITS REAL(*) input	umin, umax
C	ANT	INT(*)	input	Antennas to select
C	NANT	INT	input	Number of valid elements in ANT
C	BAS	INT(*)	input	Baselines with ANT
C	NBAS	INT	input	Number of valid elements in BAS
C	THETA	REAL	input	Rotation angle in degrees
C	OFFSET	REAL	input	Offset for selected weights
C	NSEL	INT	output	Number of vis. selected
C	NTOT	INT	output	Total number of vis examined
C	RANDOM	REAL	input	Random probability of selection 0 => 1
C	SEED	INT	input	Integer seed for the generator
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Mar 24 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB
      REAL		TIMERANG(*), UVLIMITS(*), THETA, RANDOM
      INTEGER		NSEL, NTOT, ANT(*), BAS(*), NANT, NBAS, SEED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRWT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, TADD, UADD, VADD, WADD, BADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      NTOT = NAXIS(1)
      UADD = DATADD(STRM2(NAME, 'UU'))
      VADD = DATADD(STRM2(NAME, 'VV'))
      WADD = DATADD(STRM2(NAME, 'WW'))
      TADD = DATADD(STRM2(NAME, 'TIME'))
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
C
      CALL UVROTP (MEMR(WTADD), NTOT, MEMR(TADD), MEMR(BADD),
     $   MEMR(UADD), MEMR(VADD), MEMR(WADD), TIMERANG, UVLIMITS,
     $   ANT, NANT, BAS, NBAS, THETA, RANDOM, SEED,
     $   MEMR(UADD), MEMR(VADD), NSEL)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)uvrot.f	1.2    6/5/93
C
      SUBROUTINE UVROTP (WT, NVIS, TIME, BASL, U, V, W, TIMERANG,
     $   UVLIMITS, ANT, NANT, BAS, NBAS, THETA, RANDOM, SEED,
     $   NEWU, NEWV, NSEL)
C
CD Rotate position angle of selected u's and u's by theta  [pixel level]
C
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	TIME	REAL	input	Time of each visibility
C	U,V,W	REAL	input	U,V,W in wavelengths
C	TIMERANG	INT	input	TIMERANG of allowed data
C	UVLIMITS	REAL	input	Allowed range of u,v radius
C	NEWU, NEWV	REAL	output	New u's and v's
C	NSEL	INT	output	Number selected
C Audit trail:
C	Cloned from VISSELPI
C				D.S.Briggs	Mar 23 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NSEL, ANT(*), NANT, BAS(*), NBAS, SEED
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS),
     $   		NEWU(NVIS), NEWV(NVIS), TIME(NVIS), UVLIMITS(*),
     $   		TIMERANG(*), THETA, BASL(*), RANDOM, R
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'UVROTP')
C
      REAL		PI, DTOR
      PARAMETER		(PI=3.141592265359)
      PARAMETER		(DTOR=PI/180.0)
C
      INTEGER		I, IVIS, IA1, IA2, ANANT, ANBAS
      REAL		RSQ, RSQMIN, RSQMAX, TMIN, TMAX, CTH, STH,
     $   		TU, TV
C=====================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CTH = COS(THETA*DTOR)
      STH = SIN(THETA*DTOR)
      NSEL = 0
      ANANT = ABS(NANT)
      ANBAS = ABS(NBAS)
      IF ((TIMERANG(1).EQ.0.0).AND.(TIMERANG(2).EQ.0.0)) THEN
         TMIN = -1E20
         TMAX = 1E20
      ELSE
         TMIN = TIMERANG(1)
         TMAX = TIMERANG(2)
      END IF
C
      RSQMIN = UVLIMITS(1)**2
      RSQMAX = UVLIMITS(2)**2
      DO 100 IVIS = 1, NVIS
C
         NEWU(IVIS) = U(IVIS)
         NEWV(IVIS) = V(IVIS)
C
         IF (WT(IVIS).LT.0.0) GO TO 100
         IF ((TIME(IVIS).LT.TMIN).OR.(TIME(IVIS).GT.TMAX))
     $      GO TO 100
         RSQ = U(IVIS)**2+V(IVIS)**2+W(IVIS)**2
         IF ((RSQ.LT.RSQMIN).OR.(RSQ.GT.RSQMAX))
     $      GO TO 100
C
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         IF (IA1.GT.IA2) THEN
            I = IA1
            IA1 = IA2
            IA2 = I
         END IF
         IF (ANANT.GT.0) THEN
            IF (ANBAS.EQ.0) THEN
C					All baselines with a given antenna
               IF (NANT.LT.0) THEN
                  DO 5 I = 1, ANANT
                     IF (IA1.EQ.ANT(I)) GO TO 100
                     IF (IA2.EQ.ANT(I)) GO TO 100
 5                CONTINUE
               ELSE
                  DO 10 I = 1, ANANT
                     IF (IA1.EQ.ANT(I)) GO TO 25
                     IF (IA2.EQ.ANT(I)) GO TO 25
 10               CONTINUE
                  GO TO 100
               END IF
            ELSE
C					Individual baselines selected
               IF (NANT.LT.0) THEN
                  DO 15 I = 1, ANANT
                     IF (IA1.EQ.ANT(I)) GO TO 100
 15               CONTINUE
               ELSE
                  DO 20 I = 1, ANANT
                     IF (IA1.EQ.ANT(I)) GO TO 25
 20               CONTINUE
                  GO TO 100
               END IF
            END IF
         END IF
 25      CONTINUE
         IF (ANBAS.GT.0) THEN
            IF (NBAS.LT.0) THEN
               DO 30 I = 1, ANBAS
                  IF (IA2.EQ.BAS(I)) GO TO 100
 30            CONTINUE
            ELSE
               DO 40 I = 1, ANBAS
                  IF (IA2.EQ.BAS(I)) GO TO 45
 40            CONTINUE
               GO TO 100
            END IF
         END IF
 45      CONTINUE
C
         IF (RANDOM.GT.0) THEN
            CALL UTLRAND (R, SEED)
            IF (R.GE.RANDOM) GO TO 100
         END IF
C
C Here's the actual rotation
C
         NSEL = NSEL + 1
         TU =  CTH * U(IVIS) + STH * V(IVIS)
         TV = -STH * U(IVIS) + CTH * V(IVIS)
         NEWU(IVIS) = TU
         NEWV(IVIS) = TV
C
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

