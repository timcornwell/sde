C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcwin.f	1.1    2/10/93
C
      SUBROUTINE IPCWIN (FRAMES, FSIZE, FRSTART, FREND, INDEX, NPHOTONS)
C
CD Extract the window of all Frames from FRSTART to FREND
C
C Given a sorted array containing the frame numbers of all the photons
C  find the index that points to the frame number FSTART. ALSO Return how
C  many photons between this point and the frame starting at FEND.
C  Example to extract frame 100 set FRSTART=100 and FEND to 101.
C
C	FRAMES	REAL(FSIZE)     Sorted array containing Frame numbers
C	FSIZE	INTEGER         Size of the above array
C       FRSTART INTEGER         Starting Frame
C       FREND   INTEGER         Ending Frame
C       INDEX   INTEGER         Index to start of window
C       NPHOTONS INTEGER       Size of window
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 9 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input Variables
C
      INTEGER           FSIZE, FRSTART, FREND, INDEX, NPHOTONS
      REAL              FRAMES(FSIZE)
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCWIN')

C
C Function definitions
C

C
C Local Variables
C
      INTEGER          INDEX1
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Begin gode here
C
      CALL PIXRBSCH(FRAMES, FSIZE, FLOAT(FRSTART), INDEX)
      IF (INDEX.EQ.-1) THEN
         INDEX = FSIZE
         NPHOTONS = 0
      ELSE
         CALL PIXRBSCH(FRAMES, FSIZE, FLOAT(FREND), INDEX1)
         IF (INDEX1.EQ.-1) THEN
            NPHOTONS = FSIZE + 1 - INDEX
         ELSE
            NPHOTONS = INDEX1 - INDEX
         END IF
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
