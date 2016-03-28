C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matsvd.f	1.3    2/8/95
C
      SUBROUTINE MATSVD (A, U, W, V)
C
CD Singular value decomposition of a matrix
C
C	A	CH*(*)	input	Name of input array.
C	U	CH*(*)	input	Name of first output array.
C                                May be same as A.
C	W	CH*(*)	input	Name of output diagonal vector.
C	V	CH*(*)	input	Name of third output array.
C
C See Numerical Recipes, section 2.9
C
C The matrix A is decomposed into U . W . Vt
C If A has M rows & N columns, then U is a unitary matrix of the same
C size, (MXN), W is diagonal matrix of size NxN, and V is a unitary
C matrix of size NxN.  (W is actually returned in a one dimensional
C vector of length N.)  Vt is the transpose of V, which is also it's
C inverse.
C
C Audit trail:
C	Initial version
C				D.S. Briggs	5 Nov 1992
C	Fix namespace collision with modified version of NR code
C				D.S.Briggs	March 29 1994
C	Convert to LAPACK SVD routine -- more robust, better quality
C	output and faster to boot, (at cost in a huge increase in
C	source size).
C				D.S.Briggs	Feb 7 1995
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, U, W, V
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATSVD')
C
      CHARACTER*1	ATYPE
      INTEGER		INFO, NAX, RNAX, NAXIS(SYSMXDIM), M, N,
     $			AADD, VADD, WADD, UADD, TADD, NWORK
C
      INTEGER		CRDRNAX, DATADD
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NAX, NAXIS, ATYPE, AADD)
      RNAX = CRDRNAX(NAX,NAXIS)
C
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input array has bad number of axes')
         GO TO 999
      END IF
C
C Make output arrays as needed
C
      IF (U.NE.A) CALL ARRCOPY (A, U)
      UADD = DATADD(U)
      IF (ERROR) GO TO 999
C
      M = NAXIS(1)
      N = NAXIS(2)
      NAXIS(1) = N
      CALL DATMAKAR (W, 1, NAXIS, ATYPE, WADD)
      CALL DATMAKAR (V, 2, NAXIS, ATYPE, VADD)
C
C Temporary array
C
      NWORK = 3 * MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)-4)
      CALL DATMAKAR ('SVD-TEMP', 1, NWORK, ATYPE, TADD)
C
C Now call the LAPACK code
C
      IF (ATYPE.EQ.'R') THEN
         CALL SGESVD ('O', 'A', M, N, MEMR(UADD), M, MEMR(WADD),
     $      MEMR(UADD), M, MEMR(VADD), N, MEMR(TADD), NWORK, INFO)
      ELSE IF (ATYPE.EQ.'D') THEN
         CALL DGESVD ('O', 'A', M, N, MEMD(UADD), M, MEMD(WADD),
     $      MEMD(UADD), M, MEMD(VADD), N, MEMD(TADD), NWORK, INFO)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
      IF (INFO.LT.0) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE,
     $      'Bad argument passed to SGESVD')
         GO TO 999
      ELSE IF (INFO.GT.0) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Algorithm not converging')
         GO TO 999
      END IF
C
      CALL DATDELET ('SVD-TEMP')
C
C SGESVD returns the transpose of V
C
      CALL MATTRANS (V,V)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
