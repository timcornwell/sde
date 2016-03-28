C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsflip.f	1.3    11/7/90
C
      SUBROUTINE CALSFLIP (HANDLE, SF, NANT, NLAG)
C
CD Print calibration structure function.
C
C
C	HANDLE	CH*(*)	input	Handle of file
C	SF	REAL	input	Structure function
C	NANT	INT	input	Number of antennas
C	NLAG	INT	input	Number of correlations
C Audit trail:
C	New version
C				T.J.Cornwell	Jan 16 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NANT, NLAG
      REAL		SF(NANT, NANT, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALSFLIP')
C
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT=28)
      LOGICAL		ANTPRES (MAXNANT)
C
      INTEGER		IA1, IA2, ILAG, ISTART
C
      DATA		ANTPRES	/MAXNANT * .FALSE./
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 3 ILAG = 1, NLAG
         DO 2 IA2 = 1, NANT
            DO 1 IA1 = 1, NANT
               ANTPRES(IA1) = SF(IA1,IA2,ILAG).GT.0.0
 1          CONTINUE
 2       CONTINUE
 3    CONTINUE
C
      DO 10 ILAG = 1, NLAG
C
         WRITE (MESSAGE, 1000) ILAG-1
 1000    FORMAT ('Antenna Gain structure function for lag ',I2)
         CALL TXTWRITE (HANDLE, MESSAGE)
         CALL TXTWRITE (HANDLE, ' ')
         STRBUF = ' '
         ISTART = 2
         DO 5 IA1 = 1, NANT
            IF (.NOT.ANTPRES(IA1)) GO TO 5
            WRITE (STRBUF(ISTART:ISTART+4), 1100) IA1
 1100       FORMAT (2X,I3)
            ISTART = ISTART + 5
   5     CONTINUE
         CALL TXTWRITE (HANDLE, STRBUF)
         DO 20 IA2 = 1, NANT
            IF (.NOT.ANTPRES(IA2)) GO TO 20
            WRITE (STRBUF, 1200) IA2
 1200       FORMAT (I2)
            ISTART = 3
            DO 15 IA1 = 1, NANT
               IF (.NOT.ANTPRES(IA1)) GO TO 15
               WRITE (STRBUF(ISTART:ISTART+4), 1300) SF(IA1,IA2,ILAG)
 1300          FORMAT(1X,F4.2)
               ISTART = ISTART + 5
  15        CONTINUE
            CALL TXTWRITE (HANDLE, STRBUF)
  20     CONTINUE
         CALL TXTWRITE (HANDLE, '')
         IF (ERROR) GO TO 990
  10  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

