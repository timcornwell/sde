C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix2dpxa.f	1.3	5/29/91
C
      SUBROUTINE PIX2DPXA (A, NX, NY, DX, DY, INNER, MIDDLE, OUTER, 
     $   POWER1, POWER2, AVX, AVY, NSINC, SINC, RADMAX, BCENX, BCENY, B)
C
CD Taper an image with a kinked anisotropic power law. Pixel level routine. 
C  2-D complex only.
C
C	A	CMPLX	input	Image
C	NX	INT	input	Size of X-axis of A
C	NY	INT	input	Size of Y-axis of A
C	DX	REAL	input	DELTX
C	DY	REAL	input	DELTY
C	INNER	REAL	input	Inner Scale (in true units, NOT PIXELS)
C	MIDDLE	REAL	input	Middle Scale
C	OUTER	REAL	input	Outer Scale (in true units, NOT PIXELS)
C	POWER1	REAL	input	Power law index between INNER + MIDDLE
C	POWER2	REAL	input	Power law index between MIDDLE + OUTER
C	AVX	REAL	input	Averaging Scale, X direction
C	AVY	REAL	input	Averaging scale, Y direction
C	NSINC	INT	input	Size of SINC array
C	RADMAX	REAL	input	Maximum allowed value for X in SINC(X)
C	SINC	REAL(*)	input	SINC array
C	BCENX	INT	input	Center of taper in X
C	BCENY	INT	input	Center of taper in Y
C	B	CMPLX	out	Tapered image
C
C Audit trail:
C	Original Version
C	This does a few things differently from its cousin PIX2DPXT
C	1) for R < INNER, we have a flat power spectrum
C	2) for power = 0, we have a flat spectrum
C	3) we have TWO power laws seperated by MIDDLE
C	4) we have independent X and Y outer limits
C				M.A.Holdaway	May 15 1991
C	Added the SINC to properly deal with averaging
C				M.A.Holdaway	May 28 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, NY, BCENX, BCENY, NSINC
      COMPLEX		A(NX,*), B(NX,*)
      REAL		INNER, MIDDLE, OUTER, AVX, AVY, RADMAX, SINC(*),
     $   		POWER1, POWER2, DY, DX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIX2DPXA')
C
      INTEGER		IX, IY, IND
      REAL		R, INNERSQ, OUTERSQ, POW1D2, POW2D2, RSCALE,
     $   		CONT, RX, RY, MIDDLESQ, STHETA, CTHETA, AVR, 
     $   		RXR, RX2, RY2
      LOGICAL		AVERAGE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      INNERSQ = INNER**2
      MIDDLESQ = MIDDLE**2
      OUTERSQ = OUTER**2
      POW1D2 = ABS(POWER1/2.0)
      POW2D2 = ABS(POWER2/2.0)
      CONT = (MIDDLESQ/INNERSQ)**POW1D2
      IF (AVX .NE. 0.0 .OR. AVY .NE. 0.0) THEN
         AVR = SQRT(AVX*AVX + AVY*AVY)
         STHETA = AVY/AVR
         CTHETA = AVX/AVR
         AVERAGE = .TRUE.
         RSCALE  = FLOAT(NSINC-1)/RADMAX
      ELSE
         AVERAGE = .FALSE.         
      ENDIF
C
      DO 2 IY = 1, NY
         RY = DY * FLOAT(IY-BCENY)
         RY2 = RY*RY
         DO 1 IX = 1, NX
            RX = DX * FLOAT(IX-BCENX)
            RX2 = RX*RX
            R = RX2 + RY2
            IF (R.GT.INNERSQ) THEN
               IF (R .GE. OUTERSQ) THEN
                  B(IX, IY) = 0.0
               ELSE IF(R.LT.MIDDLESQ) THEN
                  IF (POWER1 .GT. 0.0) THEN
                     B(IX,IY) = A(IX,IY)*(R/INNERSQ)**POW1D2
                  ELSE IF (POWER1 .LT. 0.0) THEN
                     B(IX,IY) = A(IX,IY)/((R/INNERSQ)**POW1D2)
                  ELSE IF (POWER1 .EQ. 0.0) THEN
                     B(IX,IY) = A(IX,IY)
                  ENDIF
               ELSE IF (R.LT.OUTERSQ) THEN
                  IF (POWER2 .GT. 0.0) THEN
                     B(IX,IY) = A(IX,IY)*CONT*(R/MIDDLESQ)**POW2D2
                  ELSE IF (POWER2 .LE. 0.0) THEN
                     B(IX,IY) = A(IX,IY)/(CONT*(R/MIDDLESQ)**POW2D2)
                  ELSE
                     B(IX,IY) = A(IX,IY)
                  ENDIF
               ENDIF
            ELSE
               B(IX, IY) = A(IX, IY)
            ENDIF
C
            IF (AVERAGE) THEN
               RXR = RX * CTHETA + RY * STHETA
               IF (ABS(RXR) * AVR .LT. RADMAX) THEN
                  IND = 1 + NINT (ABS(RXR) * AVR * RSCALE)
                  B(IX, IY) = B(IX, IY) * AVR * SINC (IND)
               ELSE
                  B(IX, IY) = 0.0
               ENDIF
            ENDIF               
 1       CONTINUE
 2    CONTINUE
C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
