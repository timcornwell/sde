C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrdmrg.f	1.3	 7/18/97
C
      SUBROUTINE ARRRDMRG (UV1, UV2, R1, R2, AVETYPE, UV3)
C
CD Merges two uv data sets
C
C	UV1	CHR*(*)	input	Name of High res UV image
C	UV2	CHR*(*)	input	Name of Low res UV image
C	R1	REAL	input	Minumum sensitivity for UV1
C	R2	REAL	input	Maximum sensitivity for UV2
C	AVETYPE	CH	input	How to mix: 'AVE' or 'LIN' or 'GAU'
C	UV3	CH*(*)	input	Output UV image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 10 1991
C	Added AVETYPE = 'GAU' for Gaussian weighted average
C				M.A.Holdaway	Dec 27 1991
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	UV1, UV2, UV3, AVETYPE
      REAL		R1, R2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRRDMRG')
C
      INTEGER		NAX1, NAX2, NAX3, NAXIS1(SYSMXDIM), 
     $   		NAXIS2(SYSMXDIM), ADD1, ADD2, ADD3, I,
     $   		NAXIS3(SYSMXDIM), NDUMMY, RNAX
      REAL		DELT(SYSMXDIM), RPIX(SYSMXDIM)
      CHARACTER*1	ATYPE1, ATYPE2, ATYPE3
C=======================================================================
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
      DATA		NAXIS1 /SYSMXDIM * 1/
      DATA		NAXIS2 /SYSMXDIM * 1/
      DATA		NAXIS3 /SYSMXDIM * 1/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (UV1, NAX1, NAXIS1, ATYPE1, ADD1)
      CALL DATGETAR (UV2, NAX2, NAXIS2, ATYPE2, ADD2)
      DO 10 I = 1, MAX(NAX1, NAX2)
         IF (NAXIS1(I) .NE. NAXIS2(I)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Axes are different for input arrays')
            GOTO 990
         ENDIF
 10   CONTINUE
      IF (ATYPE1 .NE. ATYPE2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different type arrays')
         GOTO 990
      ENDIF
C
      IF (DATEXIST (UV3)) THEN
         CALL DATGETAR (UV3, NAX3, NAXIS3, ATYPE3, ADD3)
         DO 20 I = 1, MAX (MAX(NAX1, NAX2), NAX3)
            IF (NAXIS1(I) .NE. NAXIS3(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     $            'Axes are different for in/out arrays')
               GOTO 990
            ENDIF
 20      CONTINUE
         IF (ATYPE1 .NE. ATYPE3) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Different type arrays')
            GOTO 990
         ENDIF
      ELSE
         CALL DATMAKAR (UV3, NAX1, NAXIS1, ATYPE1, ADD3)
      ENDIF
C
      CALL DATGETR (UV1, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL DATGETR (UV1, 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      RNAX = CRDRNAX (NAX1, NAXIS1)
C
      IF ( AVETYPE(1:1) .NE. 'A' .AND. AVETYPE(1:1) .NE. 'L'
     $   .AND. AVETYPE(1:1) .NE. 'G') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Unknown AVETYPE')
         GOTO 990
      ENDIF
      IF (R1 .GT. R2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Inner radius is larger than outer radius')
         GOTO 990
      ENDIF
C
      IF (RNAX .NE. 2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Need to write some code for other than 2-D')
         GOTO 990
      ENDIF
      IF (ATYPE1 .EQ. 'X') THEN
         CALL PIX2DXMR (MEMX (ADD1), MEMX(ADD2), NAXIS1(1), NAXIS1(2),
     $      RPIX(1), RPIX(2), DELT(1), DELT(2), R1, R2, AVETYPE,
     $      MEMX(ADD3))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Need to write some code for other data types')
         GOTO 990
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
