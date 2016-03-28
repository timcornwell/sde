C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdlist.f	1.5	 8/5/91
C
      SUBROUTINE CRDLIST (NAME)
C
CD Standard listing of coordinate system
C
C
C	NAME	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	A really weird error: ROTA was being writen as ROTA*10.
C	Now writes correctly, but in exponential format.
C				M.A.Holdaway	Feb 27 1991
C	Added one more collumn to RPIX format: now displays in the 1000's
C				M.A.Holdaway	Aug 5 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	NAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDLIST')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), 
     1   		DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
C
      INTEGER		NDUMMY, STRINT, IAX
      CHARACTER*(SYSMXNAM)	STRTMP
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL CRDGET (NAME, NAX, TYPE, NAXIS, RVAL, RPIX, DELT,
     1   ROTA)
C
      CALL MSGPUT (
     1   'Axis    Name   Pixels  Ref. Pix  Ref. Value  Increment  '//
     2   'Rotation', 'I')
C
      DO 10 IAX = 1, NAX
C
         WRITE (MESSAGE, 200) IAX, TYPE(IAX)(1:8), NAXIS(IAX), 
     1      RPIX(IAX), RVAL(IAX), DELT(IAX), ROTA(IAX)
  200    FORMAT (3X,I1,3X,A8,1X,I4,2X,F7.2,2X,1PE11.3,1X,
     $      1PE11.3,1X,1PE10.2)
         CALL MSGPUT (MESSAGE, 'I')
C
  10  CONTINUE
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
