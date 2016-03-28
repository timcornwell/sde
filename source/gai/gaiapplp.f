C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gaiapplp.f	1.1    12/26/91
C
      SUBROUTINE GAIAPPLP (IVIS, OVIS, IWT, OWT, VTIM, BAS, GAIN,
     $   GTIM, NVIS, NANT, NGAIN, MODE)
C
CD Apply gains to visibility data.  (pixel level)
C
C	IVIS	CMPLX(NVIS)	input	Input visibilities
C	OVIS	CMPLX(NVIS)	output	Output visibilities
C	IWT	REAL(NVIS)	input	Input visibilities
C	OWT	REAL(NVIS)	output	Output visibilities
C	VTIM	REAL(NVIS)	input	Visibility times
C	BAS	REAL(NVIS)	input	Visibility baselines
C	GAIN	CMPLX(NANT,NGAIN) input	Complex antenna gains
C	GTIM	REAL(NGAIN)	input	Gain times
C	NVIS	INT		input	Number of visibilities
C	NANT	INT		input	Number of antennas in gain aray
C	NGAIN	INT		input	Number of antenna gains
C	MODE	CH*(*)		input	'2PT' or 'BOXCAR'
C
C In this context, BOXCAR integration mode mererly means to select the
C gain that is closest to the visibility sample.  2PT is a seperate
C two point interpolation in amplitude and phase, for each antenna.
C The direction of phase drift is chosen to minimize distance between
C adjacent points.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	11 Sept 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NANT, NGAIN
      COMPLEX		IVIS(NVIS), OVIS(NVIS), GAIN(NANT,NGAIN)
      REAL		VTIM(NVIS), GTIM(NGAIN), BAS(NVIS),
     $   		IWT(NVIS), OWT(NVIS)
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GAIAPPLP')
C
      INTEGER		IT, IG, IA1, IA2
      REAL		GT1, GT2, GTMID, T
      REAL		AMP, AMP1, AMP2, PHS, PHS1, PHS2
      COMPLEX		GA1, GA2, CORGAIN
C
      REAL		PI
      PARAMETER		(PI = 3.141592654)
C
      INTEGER		STRLEN
      LOGICAL		IS2PT, ISBOXCAR
C=====================================================================
      IF (ERROR) GO TO 999
C
      IS2PT = MODE.EQ.'2PT'
      ISBOXCAR = MODE.EQ.'BOXCAR'
      IF (IS2PT.OR.ISBOXCAR) THEN
         MESSAGE = 'Using ' // MODE(1:STRLEN(MODE)) //
     $      ' interpolation mode'
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         MESSAGE = 'Unrecognized mode ' // MODE(1:STRLEN(MODE))
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IG = 1
      GT1 = GTIM(IG)
      GT2 = GTIM(IG+1)
      GTMID = (GT1 + GT2) / 2.0 - (GT2 - GT1)/1.E5
      DO 500 IT = 1, NVIS
         OVIS(IT) = IVIS(IT)
         OWT(IT) = IWT(IT)
         IF (IWT(IT).LT.0.0) GO TO 100
         T = VTIM(IT)
         IA1 = NINT(BAS(IT)) / 256
         IA2 = NINT(BAS(IT)) - 256 * IA1
C
         IF (ISBOXCAR) THEN
 100        CONTINUE
            IF ((T.GT.GTMID).AND.(IG.LT.NGAIN-1)) THEN
               IG = IG + 1
               GTMID = (GTIM(IG) + GTIM(IG+1)) / 2.0 - (GT2 - GT1)/1.E5
               GO TO 100
            END IF
            IF (T.LE.GTMID) THEN
               CORGAIN = GAIN(IA1,IG) * CONJG(GAIN(IA2,IG))
            ELSE
               CORGAIN = GAIN(IA1,IG+1) * CONJG(GAIN(IA2,IG+1))
            END IF
C
         ELSE IF (IS2PT) THEN
            IF (T.LE.GT1) THEN
               CORGAIN = GAIN(IA1,IG) * CONJG(GAIN(IA2,IG))
            ELSE
 200           CONTINUE
               IF ((T.GT.GT2).AND.(IG.LT.NGAIN-1)) THEN
                  IG = IG + 1
                  GT1 = GTIM(IG)
                  GT2 = GTIM(IG+1)
                  GO TO 200
               END IF
               IF (T.GE.GT2) THEN
                  CORGAIN = GAIN(IA1,IG+1) * CONJG(GAIN(IA2,IG+1))
               ELSE
C
C First antenna 1
C
                  AMP1 = ABS(GAIN(IA1,IG))
                  AMP2 = ABS(GAIN(IA1,IG+1))
                  PHS1 = ATAN2(AIMAG(GAIN(IA1,IG)),
     $                         REAL(GAIN(IA1,IG)))
                  PHS2 = ATAN2(AIMAG(GAIN(IA1,IG+1)),
     $                         REAL(GAIN(IA1,IG+1)))
                  IF (ABS(PHS2-PHS1).GT.PI) THEN
                     IF (PHS1.LT.0) THEN
                        PHS2 = PHS2 - 2.0 * PI
                     ELSE
                        PHS2 = PHS2 + 2.0 * PI
                     END IF
                  END IF
                  AMP = (AMP1*GT2 - AMP2*GT1 + (AMP2-AMP1)*T)
     $                  / (GT2 - GT1)
                  PHS = (PHS1*GT2 - PHS2*GT1 + (PHS2-PHS1)*T)
     $                  / (GT2 - GT1)
                  GA1 = AMP*CMPLX(COS(PHS),SIN(PHS))
C
C Now antenna 2
C
                  AMP1 = ABS(GAIN(IA2,IG))
                  AMP2 = ABS(GAIN(IA2,IG+1))
                  PHS1 = ATAN2(AIMAG(GAIN(IA2,IG)),
     $                         REAL(GAIN(IA2,IG)))
                  PHS2 = ATAN2(AIMAG(GAIN(IA2,IG+1)),
     $                         REAL(GAIN(IA2,IG+1)))
                  IF (ABS(PHS2-PHS1).GT.PI) THEN
                     IF (PHS1.LT.0) THEN
                        PHS2 = PHS2 - 2.0 * PI
                     ELSE
                        PHS2 = PHS2 + 2.0 * PI
                     END IF
                  END IF
                  AMP = (AMP1*GT2 - AMP2*GT1 + (AMP2-AMP1)*T)
     $                  / (GT2 - GT1)
                  PHS = (PHS1*GT2 - PHS2*GT1 + (PHS2-PHS1)*T)
     $                  / (GT2 - GT1)
                  GA2 = AMP*CMPLX(COS(PHS),SIN(PHS))
C
                  CORGAIN = GA1 * CONJG(GA2)
               END IF
            END IF
         END IF
C
         IF (ABS(CORGAIN).NE.0.0) THEN
            OVIS(IT) = IVIS(IT) / CORGAIN
            OWT(IT) = MAX(ABS(CORGAIN)**2, 1.E-6) * ABS(OWT(IT))
         ELSE
            OWT(IT) = SIGN(IWT(IT),-1.0)
         END IF
 500  CONTINUE
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
