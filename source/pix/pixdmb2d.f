C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixdmb2d.f	1.2    2/8/95
C
      SUBROUTINE PIXDMB2D (BEAM, INWIN, OUTWIN, PSF, WDELT,
     $   NIWIN, NOWIN, NPX, NPY, NIX, NIY, NOX, NOY, IBBLO, IBBHI,
     $   OBBLO, OBBHI)
C
CD Make a 2-d windowed beam matrix from a 2-d double PSF
C
C	BEAM	D(*,*)	output	Beam matrix
C	INWIN   R(*,*)	input	Input window
C	OUTWIN	R(*,*)	input	Output window
C	PSF	D(*,*)	input	PSF
C	WDELT	I(*)	input	Delta between INWIN & OUTWIN
C	NIWIN	INT	input	Nrows in beam matrix
C	NOWIN	INT	input	Ncols in beam matrix
C	NPX,NPY	INT	input	Number of pixels in PSF
C	NIX,NIY	INT	input	Number of pixels in INWIN
C	NOX,NOY	INT	input	Number of pixels in OUTWIN
C	IBBLO	INT(2)	input	Bounding box low for INWIN
C	IBBHI	INT(2)	input	Bounding box hi for INWIN
C	OBBLO	INT(2)	input	Bounding box low for OUTWIN
C	OBBHI	INT(2)	input	Bounding box hi for OUTWIN
C
C This beam maps the dirty pixels in the input window to the components
C in the output window
C
C Pixel I+WDELT of the input window should correspond to the same position
C on the sky as pixel I of the output window.
C
C  Audit trail:
C	Remade from PIXRMB2D, as the easiest way to track the new
C	changes
C				D.S.Briggs	Feb 8 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NIWIN, NOWIN, NPX, NPY, NIX, NIY, NOX, NOY,
     $  		IBBLO(2), IBBHI(2), OBBLO(2), OBBHI(2),
     $   		WDELT(2)
      DOUBLE PRECISION	BEAM(NIWIN,NOWIN), PSF(NPX,NPY)
      REAL		INWIN(NIX,NIY), OUTWIN(NOX,NOY)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXDMB2D')
C
      INTEGER		IX, IY, JX, JY, KXP, KYP, WI, WJ
C=====================================================================
      IF (ERROR) GO TO 999
C
      WI = 1
      DO 140 IY = IBBLO(2), IBBHI(2)
         DO 130 IX = IBBLO(1), IBBHI(1)
            IF (INWIN(IX,IY).GT.0.0) THEN
               WJ = 1
               DO 110 JY = OBBLO(2), OBBHI(2)
                  DO 100 JX = OBBLO(1), OBBHI(1)
                     IF (OUTWIN(JX,JY).GT.0.0) THEN
                        KXP = (IX - WDELT(1) - JX) + (NPX+1)/2
                        KYP = (IY - WDELT(2) - JY) + (NPY+1)/2
                        IF (KXP.GT.NPX) THEN
                           KXP = KXP - NPX
                        ELSE IF (KXP.LT.1) THEN
                           KXP = KXP + NPX
                        END IF
                        IF (KYP.GT.NPY) THEN
                           KYP = KYP - NPY
                        ELSE IF (KYP.LT.1) THEN
                           KYP = KYP + NPY
                        END IF
                        IF ((WI.GT.NIWIN).OR.(WJ.GT.NOWIN)) THEN
                           CALL ERRREPOR (ERRFATAL, ROUTINE,
     $                        'Window array is wrong size')
                           GO TO 999
                        END IF
                        IF ((KXP.LT.1).OR.(KXP.GT.NPX).OR.
     $                      (KYP.LT.1).OR.(KYP.GT.NPY)) THEN
                           CALL ERRREPOR (ERRFATAL, ROUTINE,
     $                        'PSF not large enough for windows')
                           GO TO 999
                        END IF
                        BEAM(WI,WJ) = PSF(KXP,KYP)
                        WJ = WJ + 1
                     END IF
 100              CONTINUE
 110           CONTINUE
               IF (WJ.NE.NOWIN+1) THEN
                  CALL ERRREPOR (ERRFATAL, ROUTINE,
     $               'Window array is wrong size')
                  GO TO 999
               END IF
               WI = WI + 1
            END IF
 130     CONTINUE
 140  CONTINUE
C
      IF (WI.NE.NIWIN+1) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Window array is wrong size')
         GO TO 999
      END IF
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END

