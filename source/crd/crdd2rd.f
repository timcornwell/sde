C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdd2rd.f	1.2    7/11/93
C
      SUBROUTINE CRDD2RD (RAD, DECD, RRA, RDEC, ARA, ADEC)
C
CD Convert from real(3) sky coordinates to degrees.
C
C	RAD	DBLE	input	RA in degrees
C	DECD	DBLE	input	DEC in degrees
C	RRA	REAL(3)	output	HMS
C	RDEC	REAL(3)	output	DMS
C	ARA	CH*(*)	output	ASCII RA
C	ADEC	CH*(*)	output	ASCII Dec
C
C	If LEN(ARA) < 15 or LEN(ADEC) < 15, then the ASCII output for
C	that coordinate will not be calculated.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	May 10 1993
C	Deal with rounding problems when trailing fraction field rounds
C	to unity.  Field no longer overflows, and rounding done properly.
C				D.S.Briggs	9 July 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDD2RD')
C
      REAL		RRA(3), RDEC(3)
      DOUBLE PRECISION	RAD, DECD
      CHARACTER*(*)	ARA, ADEC
C
      CHARACTER*20	TMPBUF
      INTEGER		I, IRA(4), IDEC(4)
      DOUBLE PRECISION	S, D
C=====================================================================
      IF (ERROR) GO TO 999
C
      S = SIGN(1.0D0, RAD)
      D = ABS(RAD) / 15.D0
      RRA(1) = INT(D)
      D = (D - DBLE(RRA(1))) * 60.D0
      RRA(2) = INT(D)
      RRA(3) = (D - DBLE(RRA(2))) * 60.D0
      IF (S.LT.0.0) RRA(1) = -RRA(1)
C
      S = SIGN(1.0D0, DECD)
      D = ABS(DECD)
      RDEC(1) = INT(D)
      D = (D - DBLE(RDEC(1))) * 60.D0
      RDEC(2) = INT(D)
      RDEC(3) = (D - DBLE(RDEC(2))) * 60.D0
      IF (S.LT.0.0) RDEC(1) = -RDEC(1)
C
C ASCII output
C
      IF (LEN(ARA).GE.15) THEN
         IRA(1) = NINT(RRA(1))
         IRA(2) = NINT(RRA(2))
         IRA(3) = INT(RRA(3))
         IF ((RRA(3)-IRA(3)).GE. 0.9995) THEN
            IRA(3) = IRA(3) + 1
            IRA(4) = 0
         ELSE
            IRA(4) = INT(1000.0*(RRA(3)-IRA(3)))
         END IF
         WRITE (TMPBUF,1000) IRA
 1000    FORMAT (BZ,I4,'h',I2.2,'m',I2.2,'.',I3.3,'s')
         DO 100 I = 1, LEN(TMPBUF)
            IF (TMPBUF(I:I).NE.' ') GO TO 101
 100     CONTINUE
 101     CONTINUE
         ARA = TMPBUF(I:)
      END IF
C
      IF (LEN(ADEC).GE.15) THEN
         IDEC(1) = NINT(RDEC(1))
         IDEC(2) = NINT(RDEC(2))
         IDEC(3) = INT(RDEC(3))
         IF ((RDEC(3)-IDEC(3)).GE. 0.9995) THEN
            IDEC(3) = IDEC(3) + 1
            IDEC(4) = 0
         ELSE
            IDEC(4) = INT(1000.0*(RDEC(3)-IDEC(3)))
         END IF
         WRITE (TMPBUF,1100) IDEC
 1100    FORMAT (I4,'d',I2.2,'''',I2.2,'.',I3.3,'"')
         DO 200 I = 1, LEN(TMPBUF)
            IF (TMPBUF(I:I).NE.' ') GO TO 201
 200     CONTINUE
 201     CONTINUE
         ADEC = TMPBUF(I:)
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
