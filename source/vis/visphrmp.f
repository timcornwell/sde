C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visphrmp.f	1.4	 1/27/92
C
      SUBROUTINE VISPHRMP (T, U, V, PH, NP, TAVE, UVAVE, 
     $   PHAVE)
C
#define nave 1024
C
CD Calculates the RMS and the deviation from straight line of PH
C  over periods of time TAVE
C
C	T	REAL(*)	in	Time of vis measurement
C	U	REAL(*)	in	value of U
C	V	REAL(*)	in	value of V
C	PH	REAL(*)	in	phase(U,V,T) (assumed NO DISCONTINUITIES!)
C	NP	INT	in	number of phases
C	TAVE	REAL	in	Average time
C	UVAVE	REAL	out	average value of SQRT(U^2+V^2)
C	PHCAV	REAL	out	dispersion of PH about average value
C				of endpoint phases at TAVE
C	PHLIN	REAL	out	dispersion of PH about a straight line
C				over TAVE
C	PHAVE	REAL	out	dispersion of PH about the average value
C				of ALL phases in the TAVE interval
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 8 1991
C	Currently disabled all but AVE options
C				M.A.Holdaway	Aug 7 1991
C	Cleaned up some stuff, no change in output
C				M.A.Holdaway	Nov 15 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL		T(*), U(*), V(*), PH(*), TAVE
      INTEGER		NP
      REAL		UVAVE, PHAVE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPHRMP')
C
      REAL		TCAL(nave), AVE(nave)
     $   		
      INTEGER		NCAL, I, N, IP, IC, NPMAX, NPH
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 20 I = 1, nave
         TCAL(I) = 0.0
         AVE(I) = 0.0
 20   CONTINUE
C
      IF (nave .LT. T(NP)/TAVE) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Need to increase "nave"')
         GOTO 990
      ENDIF
C
C
C Fill in the "calibration" times and phases      
C
      IF (TAVE .NE. 0.0) THEN
         NCAL = MAX (1.0, T(NP)/TAVE)
      ELSE
         NCAL = 1
         TAVE = T(NP)
      ENDIF
C
      IC = 1
      TCAL(1) = T(1)
      DO 200 I = 2, NCAL+1
         TCAL(I) = TCAL(I-1) + 1.01 * TAVE
 200  CONTINUE
C
      NPMAX = 0
      DO 350 I = 1, NCAL
         NPH = 0
         AVE(I) = 0.0
         DO 300 IP = 1, NP
            IF (T(IP) .GE. TCAL(I)  .AND.
     $          T(IP) .LT. TCAL(I+1)) THEN
               AVE(I) = AVE(I) + PH(IP)
               NPH = NPH + 1
            ENDIF
 300     CONTINUE
         IF (NPH .NE. 0) THEN
            AVE(I) = AVE(I) / FLOAT(NPH)
         ELSE
            AVE(I) = 0.0
         ENDIF
 350  CONTINUE
C
C calculate everything
C      
      UVAVE = 0.0
      PHAVE = 0.0
      IC = 1
      N = 0
      DO 400 I = 1, NP
         UVAVE = UVAVE + SQRT ( U(I)*U(I) + V(I)*V(I) )
         IF (T(I) .GT. TCAL(IC+1)) THEN
            IC = IC + 1
            IF (IC .GT. NCAL) GOTO 500
         ENDIF
         PHAVE = PHAVE + (PH(I) - AVE(IC))*(PH(I) - AVE(IC))
         N = N + 1
 400  CONTINUE
 500  CONTINUE
      UVAVE = UVAVE / FLOAT(N)
      PHAVE = SQRT (PHAVE / FLOAT(N - 1) )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
