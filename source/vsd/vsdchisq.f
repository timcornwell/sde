C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdchisq.f	1.1    5/4/93
C
      SUBROUTINE VSDCHISQ (I, V, M, WT, BL, NVIS, DR, DL,
     $   DRWT, DLWT, NANT, A, X2)
C
CD We calculate the chi squared in the vis due to D term offset A
C  Only handles one D term interval at a time
C
C	I	X(*)	inp	I Vis
C	V	X(*)	inp	V Vis
C	M	X(*)	inp	Model I Vis
C	WT	R(*)	inp	I Vis Wt
C	BL	R(*)	inp	Baseline code
C	NVIS	INT	inp	Number of Vis
C	DR	X(*)	inp	DR Pol Leakage, relative
C	DL	X(*)	inp	DL Pol Leakage, relative
C	DRWT	R(*)	inp	DR Weight (0 or 1)
C	DLWT	R(*)	inp	DL Weight (0 or 1)
C	NANT	INT	inp	Number of Ants
C	A	X	inp	Complex Offset in D terms
C	X2	REAL	out	Resulting X squared
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 31 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      COMPLEX	I(*), V(*), M(*), DR(*), DL(*), A
      REAL	WT(*), BL(*), DRWT(*), DLWT(*), X2
      INTEGER	NVIS, NANT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDCHISQ')
C
      LOGICAL	DORR, DOLL
      COMPLEX	DRA(40), DLA(40), DEL, RR, LL, DR1, DR2, DL1, DL2
      INTEGER	IA, IA1, IA2, IV
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
      DORR = .TRUE.
      DOLL = .TRUE.
C
      DO 20 IA = 1, NANT
         DRA(IA) = DR(IA) + A
         DLA(IA) = DL(IA) - CONJG(A)
 20   CONTINUE
C
      X2 = 0
      DO 100 IV = 1, NVIS
         IF (WT(IV) .LE. 0.0) GOTO 100
         IA1 = NINT(BL(IV)/256.0)
         IA2 = NINT(BL(IV)-FLOAT(256*IA1))
         IF (DRWT(IA1) .LE. 0.0) GOTO 100
         IF (DRWT(IA2) .LE. 0.0) GOTO 100
         IF (IA1 .EQ. 2) GOTO 100
         DR1 = DRA(IA1)
         DR2 = DRA(IA2)
         DL1 = DLA(IA1)
         DL2 = DLA(IA2)
         RR = I(IV) + V(IV)
         LL = I(IV) - V(IV)
C
         IF (DORR) THEN
            DEL = RR - M(IV)*(1.0 + DR1 * CONJG(DR2))
            X2 = X2 + DEL * CONJG(DEL)
         ENDIF
         IF (DOLL) THEN
            DEL = LL - M(IV)*(1.0 + DL1 * CONJG(DL2))
            X2 = X2 + DEL * CONJG(DEL)
         ENDIF
 100  CONTINUE

C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
