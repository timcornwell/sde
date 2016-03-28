C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matnnls.f	1.3    12/1/94
C
      SUBROUTINE MATNNLS (A, X, B)
C
CD LS solution to A X = B, subject to Xi >= 0
C
C	A	CH*(*)	input	Name of input array.
C	X	CH*(*)	input	Name of solution vector.
C	B	CH*(*)	input	Name of constant vector.
C
C	A/ITMAX REAL	input	Scaled maximum number of iterations
C
C See Lawson & Hanson, _Solving Least Squares Problems_, Chap 23
C
C A contains the product Q*R on exit, where Q is an M by M orthogonal
C matrix generated implicitly by the subroutine.  B receives Q*B.
C (That is, both are effectively destroyed.)
C
C If ITMAX is not given, it defaults to the value hard coded into the
C original Lawson & Hanson routine, 3*N, where N is the size of the
C X vector.
C
C Audit trail:
C	Initial version
C				D.S. Briggs	22 Feb 1993
C	Slight fix to debugging code.
C				D.S.Briggs	July 13 1994
C	Added ITMAX optional argument
C				D.S.Briggs	Nov 29 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, X, B
      INTEGER		ITMAX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATNNLS')
C
      CHARACTER*1	ATYPE, BTYPE, XTYPE
      INTEGER		IERR, NAX, RNAX, NAXIS(SYSMXDIM), M, N, I,
     $			AADD, BADD, XADD, WADD, ZADD, IADD, PADD
      REAL		RNORM
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		CRDRNAX, DATADD, DATFGETI
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NAX, NAXIS, ATYPE, AADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      M = NAXIS(1)
      N = NAXIS(2)
C
      IF (DATEXIST(STRM2(A,'ITMAX'))) THEN
         ITMAX = DATFGETI(A,'ITMAX')
      ELSE
         ITMAX = 3 * N
      END IF
C
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input array has bad number of axes')
         GO TO 999
      END IF
C
      CALL DATGETAR (B, NAX, NAXIS, BTYPE, BADD)
      RNAX = CRDRNAX(NAX,NAXIS)
C
      IF (RNAX.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input constraint vector has bad number of axes')
         GO TO 999
      END IF
C
      IF (NAXIS(1).NE.M) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Input constraint vector has bad size')
         GO TO 999
      END IF
C     
C Make output arrays as needed
C
      IF (.NOT.DATEXIST(X)) THEN
         NAXIS(1) = N
         XTYPE = BTYPE
         CALL DATMAKAR (X, NAX, NAXIS, XTYPE, XADD)
      ELSE
         CALL DATGETAR (X, NAX, NAXIS, XTYPE, XADD)
      END IF
      RNAX = CRDRNAX(NAX,NAXIS)
C
      IF (RNAX.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input solution vector has bad number of axes')
         GO TO 999
      END IF
C
      IF (NAXIS(1).NE.N) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Input solution vector has bad size')
         GO TO 999
      END IF
C
      IF ((ATYPE.NE.BTYPE).OR.(ATYPE.NE.XTYPE)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Array type mismatch A B X = ' // ATYPE // ' ' //
     $        BTYPE // ' ' // XTYPE)
         GO TO 999
      END IF
C
C Temporary arrays
C
      CALL DATMAKAR ('NNLS-WTMP', 1, N, ATYPE, WADD)
      CALL DATMAKAR ('NNLS-ZTMP', 1, M, ATYPE, ZADD)
      CALL DATMAKAR ('NNLS-ITMP', 1, N, 'I', IADD)
C
      IF (SYSDEBUG) THEN
         CALL ARRCOPY (A, 'NNLS-A0')
         CALL ARRCOPY (B, 'NNLS-B0')
      END IF
C
C Now call the Lawson & Hanson code
C
      IF (ATYPE.EQ.'R') THEN
         CALL SDENNLS (MEMR(AADD), M, M, N, MEMR(BADD), MEMR(XADD),
     $        RNORM, MEMR(WADD), MEMR(ZADD), MEMI(IADD), ITMAX, IERR)
      ELSE IF (ATYPE.EQ.'D') THEN
         CALL SDEDNNLS (MEMD(AADD), M, M, N, MEMD(BADD), MEMD(XADD),
     $        RNORM, MEMD(WADD), MEMD(ZADD), MEMI(IADD), ITMAX, IERR)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
      IF (IERR.EQ.2) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad dimensions')
         GO TO 999
      ELSE IF (IERR.EQ.3) THEN
         CALL MSGPUT ('Maximum iteration count exceeded in NNLS','W')
      END IF
C
      IF (SYSDEBUG) THEN
         CALL MATMULT ('NNLS-A0', X, 'NNLS-R')
         CALL ARRLC ('NNLS-R', 1.0, 'NNLS-B0', -1.0, 'NNLS-R')
         CALL MATTRANS ('NNLS-A0', 'NNLS-A0T')
         CALL MATMULT ('NNLS-A0T', 'NNLS-R', 'NNLS-P')
         PADD = DATADD ('NNLS-P')
         CALL MSGPUT ('I, X(I), W(I), P(I)', 'D')
         DO 50 I = 1, N
            WRITE (MESSAGE, 1050) I, MEMR(XADD+I-1), MEMR(WADD+I-1),
     $           MEMR(PADD+I-1)
 1050       FORMAT (I5,3E15.5)
            CALL MSGPUT (MESSAGE, 'D')
 50      CONTINUE
         CALL DATDELET ('NNLS-A0')
         CALL DATDELET ('NNLS-A0T')
         CALL DATDELET ('NNLS-B0')
         CALL DATDELET ('NNLS-P')
         CALL DATDELET ('NNLS-R')
      END IF
C
      CALL DATDELET ('NNLS-WTMP')
      CALL DATDELET ('NNLS-ZTMP')
      CALL DATDELET ('NNLS-ITMP')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
