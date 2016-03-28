C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrrms.f	1.1 9/29/92
C
      REAL FUNCTION ARRRRMS (ANAME, SIGMA)
C
CD Calculate robust rms
C
C
C	ANAME	CH*(*)	input	Directory name of array
C
C Audit trail:
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ANAME
      REAL              SIGMA
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRRRMS')
C
C Local variables
C
      CHARACTER 	ATYPE*1
      CHARACTER*(SYSMXNAM)  STRM2
      INTEGER 		ANAX, ANAXIS(SYSMXDIM)
      INTEGER 		AADD, IAX, NDUMMY, NPIXEL
      REAL 		RMS
      LOGICAL		DATEXIST
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
      DO 2 IAX = 1, SYSMXDIM
         ANAXIS(IAX) = 1
 2    CONTINUE
C
C Get array attributes
C
      CALL DATGETAR (ANAME, ANAX, ANAXIS, ATYPE, AADD)
      NPIXEL=1
      DO 1 IAX = 1, ANAX
         ANAXIS (IAX) = MAX(1, ANAXIS(IAX))
         NPIXEL=NPIXEL*ANAXIS(IAX)
  1   CONTINUE
C
C
C Now actually call routine which does the work on the pixels. Branch
C here on data type of the array.
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRRRMS (MEMR(AADD), NPIXEL, SIGMA, RMS)
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $      'CANNOT SENSIBLY FIND STATISTICS FOR ARRAY ')
         GO TO 999
      END IF
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
      ARRRRMS=RMS
 999  CONTINUE
      END
