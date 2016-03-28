C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrnsprt.f	1.2	 7/20/92
C
      SUBROUTINE ARRNSPRT (NAME, NSPRT)
C
C Find the number of nonzero values in the array, NAME, (the number
C of pixels in the support of NAME).
C
C	NAME	CH*(*)	input	Name of directory entry
C	NSPRT	INT	output	Number of boxs
C
C Audit trail:
C	New subroutine.  Based on ARRNBOX.
C				D.S.Briggs	Oct 22 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
      INTEGER		NSPRT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRNSPRT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD
      INTEGER		RNAX, CRDRNAX, NPIX, I
      CHARACTER*1	ATYPE
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX (NAX, NAXIS)
      NPIX = 1
      DO 100 I = 1, RNAX
         NPIX = NPIX * NAXIS(I)
 100  CONTINUE
C
      NSPRT = 0
      IF (ATYPE.EQ.'I') THEN
         DO 110 I = 0, NPIX-1
            IF (MEMI(ADD+I).NE.0) NSPRT = NSPRT + 1
 110     CONTINUE
      ELSE IF (ATYPE.EQ.'R') THEN
         DO 120 I = 0, NPIX-1
            IF (MEMR(ADD+I).NE.0.0) NSPRT = NSPRT + 1
 120     CONTINUE
      ELSE IF (ATYPE.EQ.'X') THEN
         DO 130 I = 0, NPIX-1
            IF (MEMX(ADD+I).NE.(0.0,0.0)) NSPRT = NSPRT + 1
 130     CONTINUE
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type not supported')
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
