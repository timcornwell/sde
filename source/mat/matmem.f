C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matmem.f	1.2 12 Feb 1995
C
      SUBROUTINE MATMEM (A, AT, X, XDEF, B, BSIGMA)
C
CD Find solution to A X = B using MEM algorithm. 
C
C	A	CH*(*)	input	Name of input array.
C	AT	CH*(*)	input	Name of input transpose array.
C	X	CH*(*)	input	Name of solution vector.
C	XDEF	CH*(*)	input	Name of default for solution vector.
C	B	CH*(*)	input	Name of data vector.
C	BSIGMA	CH*(*)	input	Name of data sigma vector.
C
C Audit trail:
C	Initial version
C				T.J. Cornwell   Jan 8 1995
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, AT, X, XDEF, B, BSIGMA
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATMEM')
C
      CHARACTER*1	ATYPE, BTYPE, XTYPE
      INTEGER		NAX, RNAX, NAXIS(SYSMXDIM), M, N, NCONJ,
     $			AADD, ATADD, BADD, XADD, NITER, DATFGETI
      REAL		TFLUX, TOL, LENGTH, MU, Z, DATFGETR,
     $			TCHISQ, CHISQ, SUMWT
C
      INTEGER		CRDRNAX, DATADD, ITER,
     $			XDEFADD, BSIGMAADD, BPREDADD, 
     $  		WORK1ADD, WORK2ADD, WORK3ADD, LAMBDAADD

      CHARACTER*(SYSMXNAM)	STRM2, BPRED, WORK1, WORK2, WORK3,
     $   		LAMBDA
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (AT, NAX, NAXIS, ATYPE, ATADD)
      CALL DATGETAR (A, NAX, NAXIS, ATYPE, AADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      M = NAXIS(1)
      N = NAXIS(2)
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
C Now call the deconvolution code
C
      IF (DATEXIST(STRM2(A,'NITER'))) THEN
         NITER = DATFGETI(A,'NITER')
      ELSE
         NITER = 3 * M
      END IF
      IF (DATEXIST(STRM2(A,'TFLUX'))) THEN
         TFLUX = DATFGETR(A,'TFLUX')
      ELSE
         CALL ARRSTAT (X, ' ')          
         TFLUX = DATFGETR (X, 'ARRSUM')
      END IF
C
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
C Make work arrays
C
      WORK1 = STRM2(ROUTINE, 'WORK1')
      WORK2 = STRM2(ROUTINE, 'WORK2')
      WORK3 = STRM2(ROUTINE, 'WORK3')
      BPRED = STRM2(ROUTINE, 'BPRED')
      LAMBDA = STRM2(ROUTINE, 'LAMBDA')
      CALL ARRCOPY (B, WORK1)
      CALL ARRCOPY (B, WORK2)
      CALL ARRCOPY (B, WORK3)
      CALL ARRCOPY (B, BPRED)
      CALL ARRCOPY (B, LAMBDA)
      XADD = DATADD(X)
      XDEFADD = DATADD(XDEF)
      BSIGMAADD = DATADD(BSIGMA)
      BADD = DATADD(B)
      BPREDADD = DATADD(BPRED)
      WORK1ADD = DATADD(WORK1)
      WORK2ADD = DATADD(WORK2)
      WORK3ADD = DATADD(WORK3)
      LAMBDAADD = DATADD(LAMBDA)
      IF (ERROR) GO TO 990
C
C ************************ Perform iterations ***********************
C
      NCONJ = 10
      TCHISQ = M
      TOL = 0.01
      CALL MSGPUT (
     $   ' Iteration         Potential  Gradient       Flux       Fit',
     $   'I')
      DO 200 ITER = 1, NITER
         IF(ERROR) GOTO 999
         CALL MAWONEIT (ITER.EQ.1, M, N, NCONJ, TFLUX,
     $      MEMR(XADD),
     $      MEMR(XDEFADD),
     $      MEMR(BSIGMAADD),
     $      MEMR(BADD),
     $      MEMR(BPREDADD),
     $      MEMR(WORK1ADD),
     $      MEMR(WORK2ADD),
     $      MEMR(WORK3ADD),
     $      TCHISQ, TOL, LENGTH,
     $      MEMR(LAMBDAADD),
     $      MU, Z, MEMR(AADD), MEMR(ATADD))
         IF (ERROR) GO TO 999
         CALL ARRSTAT (X, ' ')
         CALL PIXRSCHI (MEMR(BADD), MEMR(BPREDADD), MEMR(BSIGMAADD), M,
     $      CHISQ, SUMWT)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1100) ITER, Z, LENGTH, DATFGETR(X,'ARRSUM'),
     $      SQRT(CHISQ/FLOAT(M))
 1100    FORMAT (1X,I5,5X,F16.3,1X,3(F10.3,1X))
         CALL MSGPUT (MESSAGE, 'I')
         IF (SYSINTRP) THEN
            SYSINTRP = .FALSE.
            IF (SYSINTAC.EQ.'QUIT') THEN
               GO TO 300
            END IF
         END IF
 200  CONTINUE
 300  CONTINUE
C
      CALL DATDELET (WORK1)
      CALL DATDELET (WORK2)
      CALL DATDELET (WORK3)
      CALL DATDELET (BPRED)
      CALL DATDELET (LAMBDA)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE PIXRSCHI (A, APRED, SIGMA, N, CHISQ, SUMWT)
C
CD Find chisq.
C
C	A	REAL	input	Input array
C	APRED	REAL	input	Input array
C	SIGMA	REAL	input	Sigma per point
C	N	INT	input	Number of elements
C	CHISQ	REAL	output	Chi-squared
C	SUMWT	REAL	output	Sum of weights
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSRCHI')
C
      REAL		SIGMA(*), CHISQ, SUMWT
      REAL		A(*), APRED(*)
      INTEGER		N
C
      INTEGER		I
C=====================================================================
      IF (ERROR) GO TO 999
      CHISQ = 0.0
      SUMWT = 0.0
      DO 10 I = 1, N
         IF (SIGMA(I).GT.0.0) THEN
            CHISQ = CHISQ + ((A(I)-APRED(I))/SIGMA(I))**2
            SUMWT = SUMWT + 1.0/SIGMA(I)**2
         END IF
 10   CONTINUE
C
      IF(SUMWT.LE.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Sumwt<0.0')
         GO TO 999
      END IF
      IF(CHISQ.LE.0.0) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Chisq<0.0')
         GO TO 999
      END IF
C
 999  CONTINUE
      END
