C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arruvplt.f	1.5    4/18/91
C
      SUBROUTINE ARRUVPLT (VIS, CLASS, DEVNAME, NSKIP, AUTOS, XLIM,
     $   YLIM)
C
CD Plot u,v points
C
C
C	VIS		CH*(*)	input	Directory name of visibility
C	CLASS		CH*(*)	input	Class e.g. 'OBS/I'
C	DEVNAME		CH*(*)	input	Device name
C Audit trail:
C	Added scaling
C				T.J. Cornwell April 18 1991
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	VIS, CLASS, DEVNAME
      INTEGER		NSKIP
      LOGICAL		AUTOS
      REAL		XLIM(2), YLIM(2)
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRUVPLT')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE
      CHARACTER		STRM2*(SYSMXNAM), STRM3*(SYSMXNAM)
      INTEGER 		XNAX, XNAXIS(SYSMXDIM)
      INTEGER 		UADDR, VADDR, WTADDR
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get x-array attributes
C
      CALL DATGETAR (STRM2(VIS, 'UU'), XNAX, XNAXIS, ATYPE, UADDR)
      CALL DATGETAR (STRM2(VIS, 'VV'), XNAX, XNAXIS, ATYPE, VADDR)
      CALL DATGETAR (STRM3(VIS, CLASS, 'WT'), XNAX, XNAXIS, ATYPE,
     $   WTADDR)
C
C  Do your thing
C
      CALL PLTUVPTS(XNAXIS(1), MEMR(UADDR), MEMR(VADDR), MEMR(WTADDR), 
     $   DEVNAME, NSKIP, AUTOS, XLIM, YLIM)
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
