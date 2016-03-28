C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrvalu.f	1.1    12/7/90
C
      REAL FUNCTION ARRRVALU (NAME, CRDS)
C
CD Returns the REAL value of the pixel specified by CRDS in array NAME
C
C If an error occurs 0.0 is returned. If the array not real type conversion
C  is done. If CRDS(I) is zero then the center pixel of that axis is used
C
C	NAME	CH*(*)	input	Name of directory entry
C       CRDS    INT*(SYSMXDIM)  Array stating which pixel to return
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 25 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Input declarations
C
      CHARACTER*(*)	NAME
      INTEGER           CRDS(SYSMXDIM)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRVALU')
C
C Function Declarations
C
      REAL              PIXRVALU
C
C Local Variables
C
      CHARACTER*1       ATYPE
      INTEGER           ANAX, ANAXIS(SYSMXDIM), AADD, I
C
C=======================================================================
C
C If an error on input then exit immediately
C
      ARRRVALU = 0.0
      IF (ERROR) GO TO 999
C
C Get the array
C
      CALL DATGETAR (NAME, ANAX, ANAXIS, ATYPE, AADD)
      IF (ERROR) GOTO 990
C
C DO somethin sensible about the CRDS
C
      DO I = 1, ANAX
         IF (CRDS(I).EQ.0) THEN
            CRDS(I) = (ANAXIS(I) + 1) / 2          
         ELSE
            CRDS(I) = MIN(ANAXIS(I), MAX(1, CRDS(I)))
         END IF
      END DO
      DO I = ANAX + 1, SYSMXDIM
         CRDS(I) = 1
         ANAXIS(I) = 1
      END DO
C
C Call the routine
C
      IF (ATYPE.EQ.'R') THEN
         ARRRVALU = PIXRVALU(MEMR(AADD), CRDS, ANAXIS)
      ELSE
         MESSAGE = 'Array Type '//ATYPE//' Not supported yet'
         CALL ERRREPOR(ERRWRGTP, ROUTINE, MESSAGE)
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
