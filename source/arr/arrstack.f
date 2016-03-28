C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrstack.f	1.1    11/21/94
C
      SUBROUTINE ARRSTACK (ARRAYS, STACK, ILO, IHI, INC, DELETE)
C
CD Stack images together into simple N+1 dimensional cube
C
C	ARRAYS		CH*(*)	Name template for arrays
C	STACK		CH*(*)	Output name for stack
C	ILO, IHI	INT	First and last index
C	INC		INT	Index increment
C	DELETE		LOG	Delete individual arrays after use?
C
C Audit trail:
C	Original version: A more restricted version of the task stack
C				D.S.Briggs	Nov 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ARRAYS, STACK
      INTEGER		ILO, IHI, INC
      LOGICAL		DELETE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSTACK')
C
      INTEGER		NDUMMY, I, NAX, NAXIS(SYSMXDIM),
     $   		ISLOT, RNAX, NSTACK
      CHARACTER		ATYPE*1, TYPE(SYSMXDIM)*8
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	ANAME
C
      INTEGER		CRDRNAX, STRLEN
      LOGICAL		DATEXIST
C==================================================================
C
      NSTACK = 0
      DO 10 I = ILO, IHI, INC
         CALL STRNUMFL (ARRAYS, I, ANAME)
         IF (.NOT.DATEXIST(ANAME)) THEN
            MESSAGE = 'Array ' // ANAME(1:STRLEN(ANAME)) //
     $         ' not found!'
            CALL ERRREPOR (ERRFATAL, ROUTINE, MESSAGE)
            GO TO 999
         END IF
         NSTACK = NSTACK + 1
 10   CONTINUE
C
      CALL STRNUMFL (ARRAYS, ILO, ANAME)
      CALL DATGETAR (ANAME, NAX, NAXIS, ATYPE, NDUMMY)
      CALL CRDGET (ANAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RNAX = CRDRNAX(NAX, NAXIS)
C
C Make the output array, by messing with input image
C
      IF (NAX.EQ.SYSMXDIM) THEN
         CALL ERRREPOR(ERRNOSLT, ROUTINE, 'Can''t expand image header')
         GO TO 999
      END IF
      DO 200 I = NAX, RNAX+1, -1
         TYPE(I+1) = TYPE(I)
         NAXIS(I+1) = NAXIS(I)
         RVAL(I+1) = RVAL(I)
         RPIX(I+1) = RPIX(I)
         DELT(I+1) = DELT(I)
         ROTA(I+1) = ROTA(I)
 200  CONTINUE
      NAX = NAX + 1
      RNAX = RNAX + 1
      NAXIS(RNAX) = NSTACK
      TYPE(RNAX) = 'INDEX'
      RVAL(RNAX) = ILO
      DELT(RNAX) = INC
      RPIX(RNAX) = 1
      CALL DATMAKAR (STACK, NAX, NAXIS, ATYPE, NDUMMY)
      CALL HEDCOPY (ANAME, STACK)
      CALL CRDPUT (STACK, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 999
C
C Now the main loop to fill it with stuff
C
      ISLOT = 1
      DO 300 I = ILO, IHI, INC
         CALL STRNUMFL (ARRAYS, I, ANAME)
         CALL ARRCPPL (ANAME, STACK, RNAX, ISLOT)
         ISLOT = ISLOT + 1
 300  CONTINUE
      IF (ERROR) GO TO 999
C
C Delete input arrays if requested
C
      IF (DELETE) THEN
         DO 500 I = ILO, IHI, INC
            CALL STRNUMFL (ARRAYS, I, ANAME)
            CALL DATDELET (ANAME)
 500     CONTINUE
      END IF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
