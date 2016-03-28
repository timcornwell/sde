C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matclean.f	1.3 1/4/95
C
      SUBROUTINE MATCLEAN (A, X, B)
C
CD Find solution to A X = B using Clean algorithm. A *must* be square!
C
C	A	CH*(*)	input	Name of input array.
C	X	CH*(*)	input	Name of solution vector.
C	B	CH*(*)	input	Name of constant vector.
C
C Audit trail:
C	Initial version
C				T.J. Cornwell   November 22 1994
C	Can pass number of iterations
C				T.J. Cornwell   January 4 1995
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A, X, B
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATCLEAN')
C
      CHARACTER*1	ATYPE, BTYPE, XTYPE
      INTEGER		NAX, RNAX, NAXIS(SYSMXDIM), M, N,
     $			AADD, BADD, XADD, NITER, DATFGETI
      REAL		GAIN, LIM
C
      INTEGER		CRDRNAX, NDUMMY
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (A, NAX, NAXIS, ATYPE, AADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      M = NAXIS(1)
      N = NAXIS(2)
C
      IF (N.NE.M) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'A Matrix must be square')
         GO TO 999
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
C Now call the deconvolution code
C
      IF (DATEXIST(STRM2(A,'NITER'))) THEN
         NITER = DATFGETI(A,'NITER')
      ELSE
         NITER = 3 * M
      END IF
      IF (DATEXIST(STRM2(A, 'GAIN'))) THEN
         CALL DATGETR(A, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.1
      END IF
      IF (DATEXIST(STRM2(A, 'FLUX'))) THEN
         CALL DATGETR(A, 'FLUX', LIM, 1, NDUMMY)
      ELSE
         LIM = 0.0
      END IF
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXCLEAN (MEMR(AADD), M, MEMR(BADD), MEMR(XADD),
     $      NITER, GAIN, LIM)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
