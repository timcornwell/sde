C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W%    %G%
C
      SUBROUTINE CLSNAME (NAME)
C
CD One line description of this routine for extraction into doc/oneline
C
C Description of routine in as many lines as you like. CLS is
C a code for the type of routine e.g. FFT, IMG, CRD etc.
C
C	NAME	CH*(*)	input	Name of directory entry
C etc...
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'CLSNAME')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Code goes here
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
