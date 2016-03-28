C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arranplt.f	1.6	 3/26/93
C
      SUBROUTINE ARRANPLT (ANTFILE, DEVNAME, AUTOS, XLIM, YLIM, POPEN,
     $   PNUMB)
C
CD Plot antenna locations, Always in local coordinates
C
C
C	ANTFILE		CH*(*)	input	Directory name of antenna file
C	DEVNAME		CH*(*)	input	Device name
C	AUTOS		L	input	Do Autoscaling?
C	XLIM		R(*)	input	Xlimits for display
C	YLIM		R(*)	input	Ylimits for display
C	POPEN		L	input	Open PG Device or use what is open now?
C	PNUMB		L	input	Plot Numbers?
C
C Audit trail:
C	Added scaling
C				T.J. Cornwell April 18 1991
C
C	Cleaned up some Jing Ping Things, merged with standard ARRANPLT
C				M.A.Holdaway	March 26 1993
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ANTFILE, DEVNAME
      LOGICAL		AUTOS, POPEN, PNUMB
      REAL		XLIM(*), YLIM(*)
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRANPLT')
C
C Local variables
C
      CHARACTER*(1) 	ATYPE
      CHARACTER		STRM2*(SYSMXNAM)
      DOUBLE PRECISION  SITELAT
      INTEGER 		XNAX, XNAXIS(SYSMXDIM)
      INTEGER 		XADDR, YADDR, ZADDR, DMADDR, NDUMMY
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
C Get x-array attributes
C
      CALL DATGETD  (ANTFILE, 'SITELAT', SITELAT, 1, NDUMMY)
      CALL DATGETAR (STRM2(ANTFILE, 'LX'), XNAX, XNAXIS, ATYPE, XADDR)
      CALL DATGETAR (STRM2(ANTFILE, 'LY'), XNAX, XNAXIS, ATYPE, YADDR)
      CALL DATGETAR (STRM2(ANTFILE, 'LZ'), XNAX, XNAXIS, ATYPE, ZADDR)
      CALL DATGETAR (STRM2(ANTFILE, 'DIAM'), XNAX, XNAXIS, ATYPE,
     $   DMADDR)
C
C  Do your thing
C
      CALL PLTANPTS(XNAXIS(1), MEMD(XADDR), MEMD(YADDR), MEMD(ZADDR), 
     $   MEMD(DMADDR), DEVNAME, SITELAT, AUTOS, XLIM, YLIM, POPEN,
     $   PNUMB)
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
