C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrlog.f	1.1	 7/9/91
C
      SUBROUTINE ARRLOG (IN, BLANK, OUT)
C
CD take the LOG of an array
C
C
C	IN	CH*(*)	input	Name of array
C	BLANK	REAL	input	What to do with array if .LE. 0.0
C	OUT	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 9 1991
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      REAL		BLANK
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRLOG')
C
      CHARACTER*1	INT, OUTT
      INTEGER		I, INN, OUTN, INAXIS(SYSMXDIM),
     1			ONAXIS(SYSMXDIM)
      INTEGER		IADD, OADD, NT
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, INN, INAXIS, INT, IADD)
      NT = 1
      DO 10 I = 1, INN
         NT = NT * INAXIS(I)
  10  CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(OUT)) THEN
         OUTT = INT
	 OUTN = INN
         DO 20 I = 1, OUTN
            ONAXIS(I) = INAXIS(I)
  20     CONTINUE
         CALL DATMAKAR (OUT, OUTN, ONAXIS, OUTT, OADD)
      ELSE
         CALL DATGETAR (OUT, OUTN, ONAXIS, OUTT, OADD)
         IF (INN.NE.OUTN) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         IF (INT.NE.OUTT) THEN
            WRITE (MESSAGE, 1100) INT, OUTT
 1100       FORMAT ('Array types for images disagree : ',
     1         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         DO 30 I = 1,OUTN
            IF (ONAXIS(I).NE.INAXIS(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
C
C Call appropriate routine
C
      IF (INT.EQ.'R') THEN
         CALL PIXRLOG (MEMR(IADD), BLANK, 
     1      MEMR(OADD), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//INT//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

