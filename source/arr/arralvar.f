C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arralvar.f	1.1    7/1/91
C
      SUBROUTINE ARRALVAR (INARR, TINT, ALVAR)
C
CD Calculates the Allan Variance for INARR
C
C	NAME	CH*(*)	input	Name of directory entry
C	INARR	CH*(*)	in	array to look at
C	TINT	REAL	in	time between samples of INARR
C	ALVAR	CH*(*)	in	Allan Variance series
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 26 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INARR, ALVAR
      REAL		TINT
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRALVAR')
C
      INTEGER		INADD, RMSADD, TADD, AVADD, SADD
      INTEGER		NAX, NAXIS(SYSMXDIM), N
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*6		STRINT
      LOGICAL		DATEXIST
      INTEGER		GINT
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (INARR, NAX, NAXIS, ATYPE, INADD)
      IF (NAX .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $      'Allan Variance routines want only one axis')
         GOTO 990
      ENDIF
C
      N = GINT (ALOG10 (FLOAT(NAXIS(1))) / ALOG10(2.0))
      IF (SYSDEBUG) THEN
         CALL MSGPUT ('Will calculate '//STRINT(N)//' AV''s', 'D')
      ENDIF
      IF (N .LE. 0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No Allan Variance Possible')
         GOTO 990
      ENDIF
      IF (DATEXIST(ALVAR)) CALL DATDELET (ALVAR)
      CALL DATCREAT (ALVAR)
      CALL DATMAKAR (STRM2(ALVAR, 'RMS'), 1, N, 'R', RMSADD)
      CALL DATMAKAR (STRM2(ALVAR, 'ALVARIANCE'), 1, N, 'R', AVADD)
      CALL DATMAKAR (STRM2(ALVAR, 'TINT'), 1, N, 'I', TADD)
      CALL DATMAKAR (STRM2(ALVAR, 'SCRATCH'), 1, NAXIS(1), 'R', SADD)
C
      CALL ARRSETCO (STRM2(ALVAR, 'RMS'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(ALVAR, 'ALVARIANCE'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(ALVAR, 'TINT'), 0.0, 0.0)
      CALL ARRSETCO (STRM2(ALVAR, 'SCRATCH'), 0.0, 0.0)
C
      CALL PIXALVAR (MEMR(INADD), MEMR(SADD), NAXIS(1), TINT, 
     $   MEMR(AVADD), MEMR(RMSADD), MEMI(TADD), N)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
