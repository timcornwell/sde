C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrpsdf.f	1.1    6/12/94
C
      SUBROUTINE PIXRPSDF (PSF, BEAM, N1, N2, RMAX, NP, HANDLE, DOCURVE)
C
CD Determine mismatch between PSF and fitted BEAM
C
C	PSF	R(N1,N2)	input	PSF
C	BEAM	R(N1,N2)	input	Gaussian fitted beam
C	N1,N2	INT		input	Dimensions of arrays
C	RMAX	REAL		input	Max abscissa of Mismatch curve
C	NP	INT		input	Number of points in curve
C	HANDLE	CH*(*)		input	Handle for curve output
C	DOCURVE	LOGICAL		input	Do curve, or only last point?
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Feb 28 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      INTEGER		N1, N2
      REAL		PSF(N1,N2), BEAM(N1,N2), RMAX
      INTEGER		NP
      CHARACTER*(*)	HANDLE
      LOGICAL		DOCURVE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRPSDF')
C
      INTEGER	I, J, K, NIN
      REAL	R, DR, THRESH, ACC0, ACC1, ACC2, DIFF, BTOT, ACCB, ACCP
C=======================================================================
      IF (ERROR) GO TO 999
C
      DR = RMAX/NP
      IF (DOCURVE) THEN
         R = DR
         MESSAGE = 'Pt, R, Beam, Int(Beam), Int(PSF), Npix, ' //
     $      '<|Diff|>, <Diff>, <Diff^2>'
         CALL TXTWRITE (HANDLE, MESSAGE)
      ELSE
         R = RMAX
      END IF
C
      BTOT = 0.0
      DO 60 J = 1, N2
         DO 50 I = 1, N1
            BTOT = BTOT + BEAM(I,J)
 50      CONTINUE
 60   CONTINUE   
C
      DO 200 K = 1, NP
         THRESH = EXP(-LOG(2.0)*R**2)
         NIN = 0
         ACC0 = 0.0
         ACC1 = 0.0
         ACC2 = 0.0
         DO 110 J = 1, N2
            DO 100 I = 1, N1
               IF (BEAM(I,J).GT.THRESH) THEN
                  IF ((I.EQ.1).OR.(I.EQ.N1)
     $               .OR.(J.EQ.1).OR.(J.EQ.N2)) THEN
                     CALL MSGPUT ('Input PSF not large enough','E')
                     GO TO 990
                  END IF
                  DIFF = PSF(I,J) - BEAM(I,J)
                  ACCB = ACCB + BEAM(I,J)
                  ACCP = ACCP + PSF(I,J)
                  ACC0 = ACC0 + ABS(DIFF)
                  ACC1 = ACC1 + DIFF
                  ACC2 = ACC2 + DIFF**2
                  NIN = NIN + 1
               END IF
 100        CONTINUE
 110     CONTINUE
C
         IF (DOCURVE) THEN
            WRITE (MESSAGE, 1110) K, R, THRESH, ACCB/BTOT, ACCP/BTOT,
     $         NIN, ACC0/NIN, ACC1/NIN, ACC2/NIN
 1110       FORMAT (I5, F8.4, F10.5, 2F12.5, I8, 1P3E14.5)
            CALL TXTWRITE (HANDLE, MESSAGE)
         ELSE
            MESSAGE = 'R, Beam, Int(Beam), Int(PSF)'
            CALL MSGPUT (MESSAGE,'I')
            WRITE (MESSAGE, 1120) R, THRESH, ACCB/BTOT, ACCP/BTOT
 1120       FORMAT (F8.4, F10.5, 2F12.5)
            CALL MSGPUT (MESSAGE, 'I')
            MESSAGE = 'Npix, <|Diff|>, <Diff>, <Diff^2>'
            CALL MSGPUT (MESSAGE,'I')
            WRITE (MESSAGE, 1130) NIN, ACC0/NIN, ACC1/NIN, ACC2/NIN
 1130       FORMAT (I8,3E15.5)
            CALL MSGPUT (MESSAGE,'I')
            GO TO 990
         END IF
         R = R + DR
 200  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
