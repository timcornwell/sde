C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imghermi.f	1.2	 7/18/97
C
      SUBROUTINE IMGHERMI (IN, HERMIT)
C
CD Turn IN half plane into a HERMITian image
C
C	HALF	CH*(*)	input	input image
C	HERMIT	CH*(*)	input	output image (should not exist prior)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 30, 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGHERMI')
C
      INTEGER           NAX, NAXIS(SYSMXDIM)
      INTEGER		PNAXIS(SYSMXDIM), I
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(*) 	IN, HERMIT
C
      DATA		PNAXIS / SYSMXDIM * 1 /
C==================================================================
      IF (ERROR) GOTO 999
C
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      IF (ERROR) GOTO 990
      IF (RPIX(1) .NE. 1) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $        'Image breaks assumption of RPIX(1) = 1')
         GOTO 990
      ENDIF
C
      DO 100 I = 1, NAX
         PNAXIS(I) = NAXIS(I)
 100  CONTINUE
      PNAXIS(1) = PNAXIS(2)
      CALL IMGPAD   (IN, HERMIT, PNAXIS, 0.0)
      CALL CRDLIST (HERMIT)
      CALL ARRHERMI (IN, HERMIT)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
