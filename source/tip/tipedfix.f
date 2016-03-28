C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipedfix.f	1.1    3/26/93
C
      SUBROUTINE TIPEDFIX (RAWDAT, THRESH, NFIX)
C
CD Gut level routine to edit and fix raw tipper data
C
C	RAWDAT	CH*(*)	inp	Raw Tipper Data
C	THRESH	REAL	inp	N * Sigma limit
C	NFIX	INT	out	How many points did we fix?
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	23 Dec 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	RAWDAT
      REAL		THRESH
      INTEGER		NFIX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPEDFIX')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD, ANADD, IN, I
      CHARACTER*1	TYPE
      INTEGER		BLOCK, NB, N
      REAL		AVE, DISP, TEST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      BLOCK = 128
      NFIX = 0
      CALL DATGETAR (RAWDAT, NAX, NAXIS, TYPE, ADD)
      N = NAXIS(1) 
      NB = N / BLOCK
C
C Look at the data in blocks of BLOCK
C
      DO 500 IN = 1, NB
         AVE = 0.0
         DO 100 I = 1, BLOCK
            AVE = AVE + MEMR(ADD + BLOCK*(IN-1) + I-1)
 100     CONTINUE
         AVE = AVE/FLOAT(BLOCK)
         DISP = 0.0
         DO 200 I = 1, BLOCK
            DISP = DISP + 
     $         ( MEMR(ADD + BLOCK*(IN-1) + I-1) - AVE )**2
 200     CONTINUE
         DISP = SQRT(DISP/FLOAT(BLOCK-1))
C
C Now edit data;  we assume that there is only 1 bad point in a row,
C except at the beginning, in which case we know there may be 2 bad points
C
         TEST = ABS(THRESH * DISP)
         DO 300 I = 1, BLOCK
            ANADD = ADD + BLOCK*(IN-1) + I-1
            IF ( ABS(MEMR(ANADD) -AVE)
     $         .GT. TEST) THEN
               NFIX = NFIX + 1
               IF (ANADD .EQ. ADD) THEN
                  MEMR(ANADD) = MEMR(ANADD+2)
               ELSE IF (ANADD .EQ. (ADD+N-1)) THEN
                  MEMR(ANADD) = MEMR(ANADD-1)
               ELSE
                  MEMR(ANADD) = (MEMR(ANADD-1)+MEMR(ANADD+1))/2.0
               ENDIF
            ENDIF
 300     CONTINUE
 500  CONTINUE


C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
