C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrwtp.f	1.3    2/21/95
C
      SUBROUTINE VISRWTP (WT, NVIS, TIME, BASL, U, V, W, TIMERANG,
     $   UVLIMITS, ANT, NANT, BAS, NBAS, DONEG, SCALE, OFFSET, RANDOM,
     $   SEED, WTTHRSH, NEWWT, NSEL)
C
CD Reweight visibility data.
C
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	TIME	REAL	input	Time of each visibility
C	U,V,W	REAL	input	U,V,W in wavelengths
C	TIMERANG INT	input	TIMERANG of allowed data
C	UVLIMITS REAL	input	Allowed range of u,v radius
C	ANT	INT(*)	input	Antennas to select
C	NANT	INT	input	Number of antenna IDs in ANT
C	BAS	INT(*)	input	Baselines with Antennas
C	NBAS	INT	input	Number of Baseline IDs in BAS
C	DONEG	LOG	input	Can negative weights be selected?
C	SCALE	REAL	input	Scale factor for selected weights
C	OFFSET	REAL	input	Offset for selected weights
C	RANDOM	REAL	input	Prob (0<=p<=1) for selection
C	SEED	REAL	input	seed for RNG
C	WTTHRSH REAL	input	Weight threshold
C	NEWWT	REAL	output	Output weights
C	NSEL	INT	output	Number selected
C Audit trail:
C	Cloned from VISSELPI
C				D.S.Briggs	Mar 23 1993
C	Added DONEG option
C				D.S.Briggs	Sept 8 1993
C	Force weights negative, if NEWWT(i) < WTTHRSH
C				D.S.Briggs	Feb 21 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NSEL, ANT(*), NANT, BAS(*), NBAS, SEED
      REAL		WT(NVIS), U(NVIS), V(NVIS), W(NVIS),
     $   		NEWWT(NVIS), TIME(NVIS), UVLIMITS(*),
     $   		TIMERANG(*), SCALE, OFFSET, BASL(*), RANDOM,
     $   		R, WTTHRSH, NEW
      LOGICAL		DONEG
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRWTP')
C
      INTEGER		I, IVIS, IA1, IA2, ANANT, ANBAS
      REAL		RSQ, RSQMIN, RSQMAX, TMIN, TMAX
C=====================================================================
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
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
         NEWWT(IVIS) = WT(IVIS)
C
         IF ((.NOT.DONEG).AND.(WT(IVIS).LT.0.0)) GO TO 100
C
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
         NEW = SCALE * WT(IVIS) + OFFSET
         IF (WTTHRSH.GT.0) THEN
            IF (NEW.LT.WTTHRSH) THEN
               NEW = -ABS(NEW)
            END IF
         END IF
C
         NSEL = NSEL + 1
         NEWWT(IVIS) = NEW
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

