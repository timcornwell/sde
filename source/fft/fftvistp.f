C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftvistp.f	1.1	 2/14/91
C
      SUBROUTINE FFTVISTP (VIS, VISTYPE)
C
CD Determines if gridded visibility is hermitian (H) or not (N)
C
C	VIS	CH(*)	in	Name of vis directory
C	VISTYPE	CH*(*)	out	Type of vis: H=hermitian, N=Non-hermitian
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Nov 30 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, VISTYPE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTVISTP')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)

C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (VIS, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GOTO 990
C
      IF (RPIX(1) .EQ. 1) THEN
         VISTYPE = 'H'
      ELSE
         VISTYPE = 'N'
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
