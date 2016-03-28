C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matldp.f	1.1    6/8/94
C
      SUBROUTINE MATLDP (G, X, H)
C
CD Minimize || X ||,  subject to G X >= H
C
C	G	CH*(*)	input	Name of constraint array.
C	X	CH*(*)	input	Name of solution vector.
C	H	CH*(*)	input	Name of constraint vector.
C
C See Lawson & Hanson, _Solving Least Squares Problems_, Chap 23
C
C This L&H routine seems to be misbehaving in at least some cases.
C This envelope does solve the example problems correctly, but the
C the matrix returned in some actual deconvolution applications does
C not fulfill the derivative criteria for a mimumum.  It's not clear
C what's going on.  matldp.f.sde is a drop in replacement for this
C routine allowing SDE style access to the intermediate matricies that
C can be used to investigate this problem further.  (Both fail in the
C same way, so the difficulty seems to be algorithmic.)
C
C Audit trail:
C	Initial version
C				D.S. Briggs	2 Mar 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	G, X, H
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MATLDP')
C
      CHARACTER*1	GTYPE, HTYPE, XTYPE
      INTEGER		IERR, NAX, RNAX, NAXIS(SYSMXDIM), M, N,
     $			GADD, HADD, XADD, WADD, IADD
      REAL		XNORM
C
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (G, NAX, NAXIS, GTYPE, GADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      M = NAXIS(1)
      N = NAXIS(2)
C
      IF (RNAX.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Constraint array has bad number of axes')
         GO TO 999
      END IF
C
      CALL DATGETAR (H, NAX, NAXIS, HTYPE, HADD)
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
         XTYPE = HTYPE
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
      IF ((GTYPE.NE.HTYPE).OR.(GTYPE.NE.XTYPE)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $        'Array type mismatch G H X = ' // GTYPE // ' ' //
     $        HTYPE // ' ' // XTYPE)
         GO TO 999
      END IF
C
C Temporary arrays
C
      CALL DATMAKAR ('LDP-WTMP', 1, (M+2)*(N+1)+2*M, GTYPE, WADD)
      CALL DATMAKAR ('LDP-ITMP', 1, M, 'I', IADD)
C
C Now call the Lawson & Hanson code
C
      IF (GTYPE.EQ.'R') THEN
         CALL LDP (MEMR(GADD), M, M, N, MEMR(HADD), MEMR(XADD),
     $        XNORM, MEMR(WADD), MEMI(IADD), IERR)
      ELSE IF (GTYPE.EQ.'D') THEN
         CALL DLDP (MEMD(GADD), M, M, N, MEMD(HADD), MEMD(XADD),
     $        XNORM, MEMD(WADD), MEMI(IADD), IERR)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Do not support array type ' // GTYPE)
         GO TO 999
      END IF
C
      IF (IERR.EQ.2) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad dimensions')
         GO TO 999
      ELSE IF (IERR.EQ.3) THEN
         CALL MSGPUT ('Maximum iteration count exceeded in LDP','W')
      ELSE IF (IERR.EQ.4) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'No solution exists')
         GO TO 999
      END IF
C
      CALL DATDELET ('LDP-WTMP')
      CALL DATDELET ('LDP-ITMP')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
