C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matsvbks.f	1.3    3/29/94
C
      SUBROUTINE MATSVBKS (U, W, V, B, X)
C
CD Back substitute a system of linear equations which has been processed
CD  by MATSVD
C
C	U	CH*(*)	input	Name of array from MATSVD
C	W	CH*(*)	input     "  "    "    "     "
C	V	CH*(*)	input     "  "    "    "     "
C	B	CH*(*)	input	Vector of constant values
C	X	CH*(*)	input	Output solution vector
C
C See Numerical Recipes, section 2.9
C
C The system solved is A . x = b, where A has been decomposed into
C U, W, and V.
C
C Audit trail:
C	Initial version
C				D.S. Briggs	5 Nov 1992
C	Was trying to create X when it already existed
C				D.S.Briggs	Sept 17 1993
C	Fixed namespace collision with modified NR code
C				D.S.Briggs	March 29 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	U, W, V, B, X
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATSVBKS')
C
      CHARACTER*1	ATYPE, ATYPE0
      INTEGER		NAX, RNAX, NAXIS(SYSMXDIM), M, N,
     $			UADD, WADD, VADD, BADD, XADD, TADD
C
      INTEGER		CRDRNAX, DATADD
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (U, NAX, NAXIS, ATYPE, UADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,

     $      'Input array U has bad number of axes')
         GO TO 999
      END IF
      ATYPE0 = ATYPE
      M = NAXIS(1)
      N = NAXIS(2)
C
      CALL DATGETAR (W, NAX, NAXIS, ATYPE, WADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF (RNAX.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input vector W has bad number of axes')
         GO TO 999
      END IF
      IF (NAXIS(1).NE.N) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array size inconsistency between U & W')
         GO TO 999
      END IF
      IF (ATYPE.NE.ATYPE0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type inconsistency between U & W')
         GO TO 999
      END IF
C
      CALL DATGETAR (V, NAX, NAXIS, ATYPE, VADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input array V has bad number of axes')
         GO TO 999
      END IF
      IF ((NAXIS(1).NE.N).OR.(NAXIS(2).NE.N)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array size inconsistency between U & V')
         GO TO 999
      END IF
      IF (ATYPE.NE.ATYPE0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type inconsistency between U & V')
         GO TO 999
      END IF
C
      CALL DATGETAR (B, NAX, NAXIS, ATYPE, BADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF (RNAX.NE.1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input vector B has bad number of axes')
         GO TO 999
      END IF
      IF (NAXIS(1).NE.M) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array size inconsistency between U & B')
         GO TO 999
      END IF
      IF (ATYPE.NE.ATYPE0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Array type inconsistency between U & B')
         GO TO 999
      END IF
C
C Make output array as needed
C
      IF (.NOT.DATEXIST(X)) THEN
         IF (X.NE.B) CALL ARRCOPY (B,X)
      END IF
      XADD = DATADD (X)
      IF (ERROR) GO TO 999
C
C Temporary Array
C
      CALL DATMAKAR ('SVBKS-TEMP', 1, N, ATYPE0, TADD)
C
C Now call the NR code
C
      IF (ATYPE0.EQ.'R') THEN
         CALL MSVBKSB (MEMR(UADD), MEMR(WADD), MEMR(VADD),
     $      M, N, M, N, MEMR(BADD), MEMR(XADD), MEMR(TADD))
      ELSE IF (ATYPE0.EQ.'D') THEN
         CALL MDSVBKSB (MEMD(UADD), MEMD(WADD), MEMD(VADD),
     $      M, N, M, N, MEMD(BADD), MEMD(XADD), MEMD(TADD))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // ATYPE)
         GO TO 999
      END IF
C
      CALL DATDELET ('SVBKS-TEMP')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
C Code from Numerical Recipes
C
      SUBROUTINE MSVBKSB(U,W,V,M,N,MP,NP,B,X,TMP)
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(N)
C
      DO 12 J=1,N
        S=0.
        IF(W(J).NE.0.)THEN
          DO 11 I=1,M
            S=S+U(I,J)*B(I)
11        CONTINUE
          S=S/W(J)
        ENDIF
        TMP(J)=S
12    CONTINUE
      DO 14 J=1,N
        S=0.
        DO 13 JJ=1,N
          S=S+V(J,JJ)*TMP(JJ)
13      CONTINUE
        X(J)=S
14    CONTINUE
 999  RETURN
      END
C
C Code from Numerical Recipes, tweaked to double precision
C
      SUBROUTINE MDSVBKSB(U,W,V,M,N,MP,NP,B,X,TMP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(N)
C
      DO 12 J=1,N
        S=0.
        IF(W(J).NE.0.)THEN
          DO 11 I=1,M
            S=S+U(I,J)*B(I)
11        CONTINUE
          S=S/W(J)
        ENDIF
        TMP(J)=S
12    CONTINUE
      DO 14 J=1,N
        S=0.
        DO 13 JJ=1,N
          S=S+V(J,JJ)*TMP(JJ)
13      CONTINUE
        X(J)=S
14    CONTINUE
 999  RETURN
      END
