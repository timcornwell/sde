C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gailistp.f	1.2    5/15/92
C
      SUBROUTINE GAILISTP (HANDLE, GAIN, TIME, NUMANT, NUMINT,
     $   TMIN, TMAX, IANT, NIANT)
C
CD List Antenna Gains (pixel level)
C
C	HANDLE	CH*(*)		input	Handle of output file for listing
C	GAIN	CMPLX(NUMANT,NUMINT)	input	Complex gains
C	TIME	REAL(NUMINT)		input	Gain times
C	NUMANT  INT		input	Number of antennas
C	NUMINT	INT		input	Number of integrations in file
C	TMIN	REAL		input	Minimum time
C	TMAX	REAL		input	Maximum time
C	IANT	INT(NIANT)	input	Requested antennas for listing
C	NIANT	INT		input	Number of antennas in IANT
C
C NIANT == 0 will list all antennas in the gain array -- typically 28
C or 40.  IF (NIANT == 1) & (IANT(1) < 0) then all antennas from 1 to
C ABS(IANT(1)) are listed.
C                                       0 => all antennas
C Audit trail:
C	Cloned (loosely) from VISLISTP
C				D.S.Briggs	4 Sept 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NUMANT, NUMINT, NIANT, IANT(NIANT)
      COMPLEX		GAIN(NUMANT,NUMINT)
      REAL		TIME(NUMINT), TMIN, TMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAILISTP')
C
      CHARACTER		FORMFEED*1
      INTEGER		I, J, MLEN, GROUP, NGROUP, NREQ
      REAL		AMP, PHASE
C
      INTEGER		GRPSIZ
      PARAMETER		(GRPSIZ = 3)
      INTEGER		AN(GRPSIZ)
C
      REAL		D2R, PI
      PARAMETER		(PI = 3.141592654)
      PARAMETER		(D2R = PI/180.0)
C
      CHARACTER		STRTIMC*12
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Initialization
C
      FORMFEED = CHAR(12)
      GROUP = 0
      IF (NIANT.EQ.0) THEN
         NREQ = NUMANT
      ELSE IF ((NIANT.EQ.1).AND.(IANT(1).LT.0)) THEN
         NREQ = ABS(IANT(1))
         NIANT = 0
      ELSE
         NREQ = NIANT
      END IF
C
C Target for main loop over groups of antennas.
C The loop is essentially a DO WHILE ...
C
 100  CONTINUE
C
C Sort out the current group of antennas
C
      GROUP = GROUP + 1
      J = (GROUP-1)*GRPSIZ + 1
      NGROUP = MIN (J+GRPSIZ-1, NREQ) - J + 1
      IF (NIANT.EQ.0) THEN
         DO 110 I = 1, NGROUP
            AN(I) = J + I - 1
 110     CONTINUE
      ELSE
         DO 120 I = 1, NGROUP
            AN(I) = IANT(J+I-1)
 120     CONTINUE
      END IF
C
C Construct a suitable header
C
      MESSAGE = '    Time'
      MLEN = 12
      DO 130 I = 1, NGROUP
         WRITE(MESSAGE(MLEN+1:MLEN+19),1130) AN(I)
 1130    FORMAT('   Amp (',I2,') Phase  ')
         MLEN = MLEN + 19
 130  CONTINUE
      CALL TXTWRITE (HANDLE, MESSAGE)
C
C Loop over data
C
      DO 210 I = 1, NUMINT
         IF ((TIME(I).GE.TMIN).AND.(TIME(I).LE.TMAX)) THEN
            MESSAGE = ' '
            MESSAGE(1:11) = STRTIMC(TIME(I))
            MLEN = 12
            DO 200 J = 1, NGROUP
               AMP = ABS(GAIN(AN(J),I))
               IF (AMP.GT.0) THEN
                  PHASE = ATAN2(AIMAG(GAIN(AN(J),I)),
     $                           REAL(GAIN(AN(J),I))) / D2R
               ELSE
                  PHASE = 0.0
               END IF
               WRITE (MESSAGE(MLEN+1:MLEN+19), 2000) AMP, PHASE
 2000          FORMAT (1X,0PG10.3E2,1X,F7.2)
               MLEN = MLEN + 19
 200        CONTINUE
            CALL TXTWRITE (HANDLE, MESSAGE)
         END IF
 210  CONTINUE
C
C Loopback, if needed
C
      IF (GROUP*GRPSIZ.LT.NREQ) THEN
         CALL TXTWRITE (HANDLE, FORMFEED)
         CALL TXTWRITE (HANDLE, ' ')
         CALL TXTWRITE (HANDLE, ' ')
         GO TO 100
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
