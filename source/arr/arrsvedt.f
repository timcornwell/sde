C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsvedt.f	1.1    6/8/94
C
      SUBROUTINE ARRSVEDT (AIN, RAT, AOUT, NSV)
C
CD Edit a SV array
C
C	AIN	CH*(*)	input	Name of array
C	RAT	REAL	input	Minimum ratio allowed
C	AOUT	CH*(*)	input	Name of array
C	NSV	INT	input	Number of SVs used
C Audit trail:
C	Original version: Cloned from arrclip
C				D.S.Briggs	Dec 6 1992
C	RAT may now be the (integral) number of SVs to keep
C	The array must be in sorted order for this option to
C	to work properly!
C				D.S.Briggs	Jan 25 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	AIN, AOUT
      REAL		RAT
      INTEGER		NSV
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSVEDT')
C
      CHARACTER*1	T1, T2
      INTEGER		I, N1, N2, NAXIS1(SYSMXDIM), NAXIS2(SYSMXDIM),
     $   		ADD1, ADD2, NT, NEDIT
      REAL		MAXSV
C
      REAL		DATFGETR
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (AIN, N1, NAXIS1, T1, ADD1)
      IF (N1.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'SV array not 1D')
         GO TO 999
      END IF
      IF (DATEXIST (AOUT)) THEN
         CALL DATGETAR (AOUT, N2, NAXIS2, T2, ADD2)
         IF (N1.NE.N2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
               GO TO 999
         END IF
         NT = 1
         DO 10 I = 1, N1
            IF (NAXIS1(I).NE.NAXIS2(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes do not match')
               GO TO 999
            ELSE
               NT = NT * NAXIS1(I)
            END IF
  10     CONTINUE
C
C Check types of arrays
C
         IF (T1.NE.T2) THEN
            WRITE (MESSAGE, 1000) T1, T2
 1000       FORMAT ('Array types for images 1 and 2 disagree : ',
     1         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
            GO TO 999
         END IF
      ELSE
C
C Make output array if it does not exist
C
         T2 = T1
	 N2 = N1
         NT = 1
         DO 20 I = 1, N1
            NAXIS2(I) = NAXIS1(I)
            NT = NT * NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (AOUT, N2, NAXIS2, T2, ADD2)
      END IF
C
C Actually do it
C
      NSV = NAXIS1(1)
      NEDIT = 0
      CALL ARRSTAT (AIN,' ')
      MAXSV = ABS(DATFGETR(AIN,'ARRMAX'))
      IF (ABS(DATFGETR(AIN,'ARRMIN')).GT.MAXSV)
     $   MAXSV = ABS(DATFGETR(AIN,'ARRMIN'))
      WRITE (MESSAGE,1005) MAXSV
 1005 FORMAT ('Max Singular Value is',1PE14.6)
      CALL MSGPUT(MESSAGE, 'I')
      DO 100 I = 1, NSV
         IF (T1.EQ.'R') THEN
            IF (RAT.GE.1.0) THEN
               IF (I.GT.NINT(RAT)) THEN
                  MEMR(ADD1+I-1) = 0.0
                  NEDIT = NEDIT + 1
               END IF
            ELSE
               IF (ABS(MEMR(ADD1+I-1)/RAT).LT.MAXSV) THEN
                  MEMR(ADD1+I-1) = 0.0
                  NEDIT = NEDIT + 1
               END IF
            END IF
         ELSE IF (T1.EQ.'D') THEN
            IF (RAT.GE.1.0) THEN
               IF (I.GT.NINT(RAT)) THEN
                  MEMD(ADD1+I-1) = 0.0
                  NEDIT = NEDIT + 1
               END IF
            ELSE
               IF (ABS(MEMD(ADD1+I-1)/RAT).LT.MAXSV) THEN
                  MEMD(ADD1+I-1) = 0.0
                  NEDIT = NEDIT + 1
               END IF
            END IF
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'SV array is funky type')
            GO TO 999
         END IF
 100  CONTINUE
      WRITE (MESSAGE, 1010) NEDIT
 1010 FORMAT ('Edited',I5,' singular values')
      CALL MSGPUT(MESSAGE, 'I')
      NSV = NSV - NEDIT
      WRITE (MESSAGE, 1020) NSV
 1020 FORMAT ('Using',I5,' singular values')
      CALL MSGPUT(MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
