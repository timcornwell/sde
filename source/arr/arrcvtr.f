C
C       National Radio Astronomy Observatory, Socorro, NM 87801
C       Software Development Environment (SDE)
C++
C @(#)arrcvtr.f	1.4    5/18/94
C
      SUBROUTINE ARRCVTR (IN, OUT)
C
CD Convert array to real
C
C	IN	CH*(*)	input	Name of input array
C	OUT	CH*(*)	input	Name of output array
C
C IN may be the same as OUT
C
C Audit trail:
C       Original version:  Only D->R implemented.
C                               D.S.Briggs      Nov 24 1992
C	X->R added.  (It takes the real part)
C				D.S.Briggs	Jun 7 1993
C	I->R added.  Bugfix (it was trashing the input array, when IN != OUT)
C				D.S.Briggs	Sept 7 1993
C-----------------------------------------------------------------------
#include        "stdinc.h"
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ARRCVTR')
C
      CHARACTER*(*)     IN, OUT
C
      CHARACTER*(*)	TEMPNAME
      PARAMETER		(TEMPNAME = ROUTINE//'-TMP')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), RADD, IADD, NT
      CHARACTER*1	ATYPE
C
      INTEGER		DATADD, ARRNPIX
      LOGICAL		DATEXIAR
C=======================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IN, NAX, NAXIS, ATYPE, IADD)
      NT = ARRNPIX(IN)
      IF (ATYPE.EQ.'R') GO TO 990
      IF (ERROR) GO TO 990
C
      IF ((ATYPE.NE.'D').AND.(ATYPE.NE.'X').AND.(ATYPE.NE.'I')) THEN
         MESSAGE = 'Conversion of type ' // ATYPE //
     $      ' to R not implemented'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      CALL ARRCOPY (IN, TEMPNAME)
      IF (DATEXIAR(OUT)) CALL DATDELAR (OUT)
      IADD = DATADD(TEMPNAME)
      CALL DATMAKAR (OUT, NAX, NAXIS, 'R', RADD)
C      
      IF (ATYPE.EQ.'D') THEN
         CALL PIXDRCPY (MEMD(IADD), 1, MEMR(RADD), 1, NT)
      ELSE IF (ATYPE.EQ.'I') THEN
         CALL PIXIRCPY (MEMI(IADD), 1, MEMR(RADD), 1, NT)
      ELSE IF (ATYPE.EQ.'X') THEN
         CALL PIXXRCPY (MEMX(IADD), 1, MEMR(RADD), 1, NT)
      END IF
C
      CALL DATDELET (TEMPNAME)
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
