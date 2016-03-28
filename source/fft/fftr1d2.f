C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftr1d2.f	1.1    2/18/92
C
      SUBROUTINE FFTR1D2 (RARRAY, XARRAY, XWORK, XWORK2,
     1      RWORK, NX, FNU, DIR)
C
CD Interface routine for R-to-X FFT, handles padding.
C
C	RARRAY	X	i/o	image array
C	XARRAY	X	i/o	visibility array
C	XWORK	X		work array
C	XWORK2	X		work array
C	RWORK	X		work array
C	NX	I	inp	size of image
C	FNU	I	inp	size of vis (full axis)
C	DIR	I	inp	direction of transform
C
C Audit trail:
C	Original version: Broken out of FFTR1D.  NU is now the FULL
C	size of the U axis, after reflection.
C					D.S.Briggs	20 Jan 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, FNU, DIR
      REAL		RARRAY(*), RWORK (*)
      COMPLEX		XARRAY(*), XWORK(*), XWORK2(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTR1D2')
C
      INTEGER		IX, NU, ICEN, IOFF
C====================================================================
      IF (ERROR) GO TO 999
C
C Find actual length of u axis and center of transform
C
      NU = FNU/2 + 1
      ICEN = (NX+1) / 2
C
C Direction of transform ?
C
      IF (DIR.GT.0) THEN
C
C Pad with zeros, if needed.
C
         IF (FNU .GT. NX) THEN
            IOFF = (FNU - NX) / 2
            DO 6 IX = 1, FNU
               RWORK(IX) = 0.
 6          CONTINUE
            DO 8 IX = 1, NX
               RWORK(IOFF +  IX) = RARRAY(IX)
 8          CONTINUE
         ELSE IF (FNU .EQ. NX) THEN
            DO 10 IX = 1, NX
               RWORK(IX) = RARRAY(IX)
 10         CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Something is messed up?')
            GOTO 990
         ENDIF
         CALL FFTR1D (RWORK, XARRAY, XWORK, XWORK2, FNU, DIR)
      ELSE IF (DIR .LT. 0) THEN
         CALL FFTR1D (RWORK, XARRAY, XWORK, XWORK2, FNU, DIR)
         IF (FNU .EQ. NX) THEN
            DO 25 IX = 1, NX
               RARRAY(IX) = RWORK(IX)
 25         CONTINUE
         ELSE IF (FNU .GT. NX) THEN
            IOFF = (FNU - NX) / 2
            DO 30 IX = 1, NX
               RARRAY(IX) = RWORK(IOFF + IX)
 30         CONTINUE
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Something is messed up?')
            GOTO 990
         ENDIF
      ENDIF
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      
