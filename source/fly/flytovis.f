C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)flytovis.f	1.8 2/11/93
C
      SUBROUTINE FLYTOVIS(VIS, CLASS, TCLASS, IMAGE, WRK, FT, FLUX)
C
CD Do optimum transform using DFT for high points
C
C Audit trail:
C
C	Bypass empty image
C				T.J.Cornwell	Feb 10 1993
C
C-----------------------------------------------------------------------
#include "stdinc.h"
C
      CHARACTER*(*)	VIS, CLASS, TCLASS, IMAGE, WRK, FT
      REAL		FLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER (ROUTINE='FLYTOVIS')
C
      INTEGER		NDUMMY
      REAL		ARRRMS
      CHARACTER*(SYSMXNAM)	STRM3
C=======================================================================
      IF(ERROR) GO TO 999 
C
C Return if image is empty
C
      CALL ARRSTAT (IMAGE, ' ')
      CALL DATGETR (IMAGE, 'ARRRMS', ARRRMS, 1, NDUMMY)
      IF(ARRRMS.EQ.0.0) GO TO 999
C
      IF (FLUX.NE.0.0) THEN
         CALL ARREDIT (IMAGE, FLUX, 1E20, WRK)
         CALL IMG3FT (VIS, TCLASS, WRK)
         CALL ARRADD (STRM3(VIS, CLASS, 'VIS'), 
     &      STRM3(VIS, TCLASS, 'VIS'), STRM3(VIS, CLASS, 'VIS'))
         CALL ARREDIT (IMAGE, -1E20, FLUX, WRK)
         CALL IMGGRIDC (WRK, WRK, 'CORRECT')
         CALL IMGFFT (WRK, FT)
         CALL VISDEGRI (VIS, TCLASS, FT)
         CALL ARRADD (STRM3(VIS, CLASS, 'VIS'), 
     &      STRM3(VIS, TCLASS, 'VIS'), STRM3(VIS, CLASS, 'VIS'))
      ELSE
         CALL IMGGRIDC (IMAGE, WRK, 'CORRECT')
         CALL IMGFFT (WRK, FT)
         CALL VISDEGRI (VIS, TCLASS, FT)
         CALL ARRADD (STRM3(VIS, CLASS, 'VIS'), 
     &      STRM3(VIS, TCLASS, 'VIS'), STRM3(VIS, CLASS, 'VIS'))
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
