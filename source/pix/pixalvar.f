C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixalvar.f	1.2    12/28/92
C
      SUBROUTINE PIXALVAR (A, AVE, NA, TA, ASD, AV, TINT, NASD)
C
CD Calculate the Allan Variance for a number of integration times
C
C	A	REAL(*)	in	data array
C	AVE	REAL(*)	scratch	scratch array as large as A
C	NA	INT	in	size of data array
C	TA	REAL	in	time increment for A
C	ASD	REAL(*)	out	Allan Standard Deviation array =SQRT(AV)
C	AV	REAL(*)	out	Allan Variance Array
C	TINT	INT(*)	out	Integration time used for ASD,AV calcs
C	NASD	INT	in	Size of the output arrays
C	
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 27 1991
C	No real changes, just changes in NOMENCLATURE (ASD and AV reversed!)
C				M.A.Holdaway	28 Dec 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	NA, TINT(*), NASD
      REAL	A(*), TA, ASD(*), AV(*), AVE(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXALVAR')
C
      REAL	DIFF, SIGMA, AVG
      INTEGER	I, J, NAVE, NPER, NAVE2, JMIN, JMAX, IASD
      CHARACTER*6	STRINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NAVE = NA
      NPER = 1
      IASD  = 1
C
 1000 CONTINUE
C
C Average each NPER elements of the array
C
         IF (SYSDEBUG) THEN
            CALL MSGPUT ('Averaging for '//STRINT(IASD), 'D')
         ENDIF
         JMIN = 1
         JMAX = NPER
         DO 100 I = 1, NAVE
            AVG = 0.0
            DO 50 J = JMIN, JMAX
               AVG = AVG + A(J)
 50         CONTINUE
            AVE(I) = AVG/FLOAT(NPER)
C
C set up for next average; do not average if there are fewer than
C NPER elements left in A
C
            JMIN = JMIN + NPER
            JMAX = JMAX + NPER
            IF (JMAX .GT. NA) THEN
               NAVE2 = I
               GOTO 200
            ENDIF
 100     CONTINUE
         NAVE2 = NAVE
 200     CONTINUE
C
C Calculate the Allan Variance
C         
         SIGMA = 0.0
         DO 300 I = 1, NAVE2 - 2
            DIFF = (AVE(I) + AVE(I+2))/2 - AVE(I+1)
            SIGMA = SIGMA + DIFF*DIFF
 300     CONTINUE
         SIGMA = 2 * SIGMA/(3*(NAVE2 - 3))
C
C The Big Moment! Fill the output arrays
C         
         AV(IASD) = SIGMA
         IF (SIGMA .GT. 0.0) THEN
            ASD(IASD) = SQRT(SIGMA)
         ELSE
            ASD(IASD) = 0.0
         ENDIF
         TINT(IASD) = NPER * TA
C
C set up for next integration size
C
         IASD = IASD + 1
         IF (IASD .GT. NASD) THEN
            GOTO 2000
         ENDIF
         NAVE = NAVE/2
         NPER = NPER * 2
      IF (NAVE .GE. 4) GOTO 1000
 2000 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
