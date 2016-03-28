C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrscale.f	1.3    11/7/90
C
      SUBROUTINE ARRSCALE (IN, SCALE, OFFSET, OUT)
C
CD Scale an array
C
C
C	IN	CH*(*)	input	Name of array
C	SCALE	REAL	input	Scale factor
C	OFFSET	REAL	input	Offset factor
C	OUT	CH*(*)	input	Name of array
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IN, OUT
      REAL		SCALE, OFFSET
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSCALE')
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
         CALL PIXRSCAL (MEMR(IADD), SCALE, OFFSET, 
     1      MEMR(OADD), NT)
      ELSE IF (INT.EQ.'D') THEN
         CALL PIXDSCAL (MEMD(IADD), SCALE, OFFSET, 
     1      MEMD(OADD), NT)
      ELSE IF (INT.EQ.'X') THEN
         CALL PIXXSCAL (MEMX(IADD), SCALE, OFFSET, 
     1      MEMX(OADD), NT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//INT//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
