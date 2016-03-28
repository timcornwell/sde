C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trplist.f	1.2	 7/20/92
C
       SUBROUTINE TRPLIST (HANDLE, NAME, SUB)
C
C List visibility data
C
C
C	HANDLE	CH*(*)	input	HANDLE of output stream
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C
C Audit trail:
C	New routine
C				T.J.Cornwell	Jan 24 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, HANDLE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPLIST')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, GADD,
     1			TTADD, WTADD, TADD, TRPADD, U1ADD,
     $                  V1ADD, U2ADD, V2ADD, NTRP, NDUMMY
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
C Check for type of structure
C
      CALL DATCHKTP (NAME, 'TRP')
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'TRP'), NAX, NAXIS, ATYPE, TRPADD)
      NTRP = NAXIS(1)
      WRITE (MESSAGE, 1000) NTRP
 1000 FORMAT ('There are ',I6,' triple-product points')
      CALL MSGPUT (MESSAGE, 'I')
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      U1ADD = DATADD (STRM2(NAME, 'UU1'))
      V1ADD = DATADD (STRM2(NAME, 'VV1'))
      U2ADD = DATADD (STRM2(NAME, 'UU2'))
      V2ADD = DATADD (STRM2(NAME, 'VV2'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      TTADD = DATADD (STRM2(NAME, 'TRIPLE'))
C
      CALL TRPLISTP (HANDLE, MEMX(TRPADD), MEMR(TTADD),
     $   MEMR(TADD), MEMR(WTADD), MEMR(U1ADD), MEMR(V1ADD),
     $   MEMR(U2ADD), MEMR(V2ADD), NTRP)
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
