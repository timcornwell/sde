C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdrp2pc.f	1.1	 8/9/91
C
      SUBROUTINE CRDRP2PC (IN)
C
CD Will shift the reference pixel to the pointing center
C
C	IN	CH*(*)	input	Name of directory entry
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 8 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IN
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDRP2PC')
C
      DOUBLE PRECISION	RVAL(SYSMXDIM), DTMP
      REAL		RPIX(SYSMXDIM), ROTA(SYSMXDIM), DELT(SYSMXDIM)
      INTEGER		NAXIS(SYSMXDIM), NAX, NDUMMY
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		PC(SYSMXDIM), PIXEL(SYSMXDIM)
      DATA		PC	/SYSMXDIM*0.0/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      CALL DATGETD (IN, 'OBSRA', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(1) = RVAL(1)
         CALL DATPUTD (IN, 'OBSRA', RVAL(1), 1)
      ELSE
         PC(1) = DTMP
      END IF
      CALL DATGETD (IN, 'OBSDEC', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(2) = RVAL(2)
         CALL DATPUTD (IN, 'OBSDEC', RVAL(2), 1)
      ELSE
         PC(2) = DTMP
      END IF
      CALL CRDWTOP (IN, PC, PIXEL)
C
      RPIX(1) = PIXEL(1)
      RPIX(2) = PIXEL(2)
C
      IF (RPIX(1) .GT. 1.5*NAXIS(1) .OR. RPIX(1) .LT. -.5*NAXIS(1))
     $   THEN
         CALL MSGPUT ('OBSRA is not near RVAL', 'W')
      ENDIF
      IF (RPIX(2) .GT. 1.5*NAXIS(2) .OR. RPIX(2) .LT. -.5*NAXIS(2))
     $   THEN
         CALL MSGPUT ('OBSDEC is not near RVAL', 'W')
      ENDIF
      CALL CRDPUT (IN, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
