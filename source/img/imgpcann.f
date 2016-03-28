C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpcann.f	1.1	5/1/95
C
      SUBROUTINE IMGPCANN (IN, RAD1, RAD2)
C
CD Make an annulus of RAD1 to RAD2 arcseconds about the pointing center
C  specified by OBSRA, OBSDEC.  Overwrites input image.
C  Within Annulus is 1.0, outside is 0.0
C
C	IN	CH*(*)	input	Name of input image
C	RAD1	REAL	input	Inner radius
C	RAD2	REAL	input	Outer radius
C
C Audit trail:
C	New
C				M.A.Holdaway	April 30 1995
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IN
      REAL		RAD1, RAD2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPCANN')
C
      CHARACTER*(SYSMXNAM)	TELESCOP
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		RNAX, CRDRNAX
      INTEGER		INADD, PBADD, NPB
      LOGICAL		BEAMTHRO
      REAL		BMX, BMY, RCONST, TELDIAM
      CHARACTER*1	TYPE
C
      REAL		RADMAX
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       CTYPE(SYSMXDIM)
C
      REAL		C(SYSMXDIM), PC(SYSMXDIM), CBT(SYSMXDIM)
C
      REAL		PBLEVEL
      CHARACTER*(SYSMXNAM)	AROUTINE, PROUTINE
      DATA	C	/SYSMXDIM*0.0/
      DATA	CBT	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get all relevent PB information
C
      CALL HEDGETPB (IN, IN, IN, .TRUE., TELESCOP, TELDIAM, PBLEVEL, 
     $   PROUTINE, AROUTINE, RADMAX, NPB, PBADD, RCONST, 
     $   C, BEAMTHRO, CBT, BMX, BMY)
C
      CALL DATGETAR (IN, NAX, NAXIS, TYPE, INADD)
      CALL CRDGET (IN, NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RNAX = CRDRNAX(NAX, NAXIS)
C
C Make annulus
C
      IF (RNAX.EQ.2) THEN
C
         CALL IMGPCAN2 (MEMR(INADD), NAXIS(1), NAXIS(2), 
     1        C(1), C(2), DELT(1), DELT(2), RAD1, RAD2)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Can only treat 2D images')
         GO TO 999
      END IF
C
      IF (ERROR) GOTO 999
C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
