C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftx1d2.f	1.1	 2/14/91
C
      SUBROUTINE FFTX1D2 (IARRAY, VARRAY, VWORK, VWORK2,
     1      IWORK, NX, NU, IDIR)
C
CD Interface routine for X-to-X FFT, 
C
C Arguments: CALL FFTX1D2 (IARRAY, VARRAY, VWORK, VWORK2
C     1      IWORK, NX, NU, IDIR)
C
C	IARRAY	X	i/o	image array
C	VARRAY	X	i/o	visibility array
C	VWORK	X		work array
C	VWORK2	X		work array
C	IWORK	X		work array
C	NX	I	inp	size of image
C	NU	I	inp	size of vis
C etc...
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	This now does X->X FFT, the X direction half of a 2-D FFT
C				M.A.Holdaway	Feb 17 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      COMPLEX		IARRAY(*), VARRAY(*), VWORK(*), IWORK(*)
      COMPLEX		VWORK2(*)
      INTEGER		NX, NU, IDIR
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTX1D2')
C
      INTEGER		IX, IOFF
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (IDIR .GT. 0) THEN
         IF (NU .GT. NX) THEN
            IOFF = (NU - NX) / 2
            DO 6 IX = 1, NU
               VWORK2(IX) = (0., 0.)
 6          CONTINUE
            DO 8 IX = 1, NX
               VWORK2(IOFF +  IX) = IARRAY(IX)
 8          CONTINUE
         ELSE IF (NU .EQ. NX) THEN
            DO 10 IX = 1, NX
               VWORK2(IX) = IARRAY(IX)
 10         CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Something is messed up?')
            GOTO 990
         ENDIF
         CALL FFTX1D (VWORK2, VWORK, IWORK, NU, IDIR)
         DO 15 IX = 1, NU
            VARRAY(IX) = VWORK2(IX)
 15      CONTINUE
      ELSE IF (IDIR .LT. 0) THEN
         DO 20 IX = 1, NX
            VWORK2(IX) = VARRAY(IX)
 20      CONTINUE
         CALL FFTX1D (VWORK2, VWORK, IWORK, NU, IDIR)
         IF (NU .EQ. NX) THEN
            DO 25 IX = 1, NX
               IARRAY(IX) = VWORK2(IX)
 25         CONTINUE
         ELSE IF (NU .GT. NX) THEN
            IOFF = (NU - NX) / 2
            DO 30 IX = 1, NX
               IARRAY(IX) = VWORK2(IOFF + IX)
 30         CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Something is messed up?')
            GOTO 990
         ENDIF
       ENDIF
C
C Can jump to here if an error found
C
 990   IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
