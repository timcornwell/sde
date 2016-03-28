C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)callist.f	1.4    5/15/92
C
      SUBROUTINE CALLIST (HANDLE, NAME, SUB)
C
CD List antenna gains, and residuals from self-cal.
C
C
C	HANDLE	CH*(*)	input	Name of output stream
C	NAME	CH*(*)	input	Name of visibility file
C	SUB	CH*(*)	input	Name of subclass e.g. 'OBS/I'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	HANDLE, NAME, SUB
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CALLIST')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NUMINT, NANT
      INTEGER		GADD, TGADD, I, IANT, ORADD, NRADD, OFFSET
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      REAL		R, PHI
      COMPLEX		ANTGAIN, ROT
      CHARACTER*(SYSMXNAM)	STRM3
      CHARACTER*12	STRTIMC, DSTRING
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAIN'), NAX, NAXIS, 
     1   ATYPE, GADD)
      NANT = NAXIS(1)
      NUMINT = NAXIS(2)
      CALL DATGETAR (STRM3(NAME, SUB, 'GAINTIME'), NAX, NAXIS, 
     1   ATYPE, TGADD)
      CALL DATGETAR (STRM3(NAME, SUB, 'ORES'), NAX, NAXIS, 
     1   ATYPE, ORADD)
      CALL DATGETAR (STRM3(NAME, SUB, 'NRES'), NAX, NAXIS, 
     1   ATYPE, NRADD)
      IF (ERROR) GO TO 990
C
      DO 10 I = 1, NUMINT
         DSTRING = STRTIMC(MEMR(TGADD+I-1))
         IF (ERROR) GO TO 990
         WRITE (MESSAGE, 1000) DSTRING , I
 1000    FORMAT ('Gains for time: ',A,', integration ',I6)
         CALL TXTWRITE (HANDLE, MESSAGE)
         WRITE (MESSAGE, 1050) MEMR(ORADD+I-1), MEMR(NRADD+I-1)
 1050    FORMAT ('Residual before fitting = ',1PE12.4,', after = ',
     1      1PE12.4 , 'Jy')
         CALL TXTWRITE (HANDLE, MESSAGE)
         MESSAGE = '    Antenna          Amplitude        Phase'
         CALL TXTWRITE (HANDLE, MESSAGE)
         ROT = 0
         DO 20 IANT = 1, NANT
            OFFSET = (IANT-1) + NANT * (I - 1)
            ANTGAIN = MEMX(GADD + OFFSET)
            R = ABS(ANTGAIN)
            IF (R.GT.0.0) THEN
               IF (ABS(ROT).EQ.0.0) THEN
                  ROT = CONJG(ANTGAIN) / R
               END IF
               ANTGAIN = ROT * ANTGAIN
               PHI = 180.0 * ATAN2(AIMAG(ANTGAIN),REAL(ANTGAIN)) / PI
               WRITE (MESSAGE, 1100) IANT, R, PHI
 1100          FORMAT ('      ',I3,'          ',F9.3,'       ',F9.3)
               CALL TXTWRITE (HANDLE, MESSAGE)
            END IF
  20     CONTINUE
         CALL TXTWRITE (HANDLE, ' ')
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
