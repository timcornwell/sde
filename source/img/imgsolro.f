C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsolro.f	1.2	 7/20/92
C
      SUBROUTINE IMGSOLRO (IN, OUT, PHI)
C
C Rotate SUN through small angle
C
C
C	IN	CH*(*)	input	Name of directory entry
C	OUT	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IN, OUT
      REAL		PHI
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSOLRO')
C
      INTEGER		NDUMMY
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		RNAX, CRDRNAX
      INTEGER		INADD, OUTADD, DATADD
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CDELT(SYSMXDIM), RAD
      DOUBLE PRECISION	CRVAL(SYSMXDIM), DTMP
      CHARACTER*1	TYPE
      REAL		C(SYSMXDIM), PC(SYSMXDIM)
      DATA	C	/SYSMXDIM*0.0/
      DATA	PC	/SYSMXDIM*0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, TYPE, INADD)
      OUTADD = DATADD (OUT)
      RNAX = CRDRNAX(NAX, NAXIS)
C
      CALL DATGETD (IN, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (IN, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
C
      IF (ERROR) GO TO 990
      CALL DATGETD (IN, 'SOLRA', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(1) = CRVAL(1)
         CALL DATPUTD (IN, 'SOLRA', CRVAL(1), 1)
      ELSE
         PC(1) = DTMP
      END IF
      CALL DATGETD (IN, 'SOLDEC', DTMP, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         PC(2) = CRVAL(2)
         CALL DATPUTD (IN, 'SOLDEC', CRVAL(2), 1)
      ELSE
         PC(2) = DTMP
      END IF
      CALL CRDWTOP (IN, PC, C)
C
      CALL DATGETR (IN, 'SOLRAD', RAD, 1, NDUMMY)
      CALL DATGETR (IN, 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      RAD = ABS(RAD/(3600.0*CDELT(1)))
      IF (RNAX.EQ.2) THEN
         CALL PIX2DSLR (MEMR(INADD), NAXIS(1), NAXIS(2), 
     1      C(1), C(2), RAD, PHI, MEMR(OUTADD))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Can only treat 2D images')
         GO TO 999
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

