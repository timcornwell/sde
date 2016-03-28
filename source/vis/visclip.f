C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visclip.f	1.1	 5/13/93
C
       SUBROUTINE VISCLIP (NAME, SUB, MSUB, EDT, UVLIM, THRES, MODE)
C
CD edit visibility data on the basis on discrepancy of OBS/MOD from 1.0
C  If MSUB = ' ', then just CLIP on basis of SUB amplitude
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	SMUB	CH*(*)	input	Name of sub-class e.g. MOD/I
C	EDT	CH*(*)	input	Name of Edited data
C	UVLIM	REAL(2)	input	UV range to apply clipping
C       THRES   REAL(2) input   Min and Max for VIS or VIS RATIO
C	MODE	CH*(*)	input	Mode of op: [DIV or CLIP]
C				CLIP ignores MSUB
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	April 9 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, MSUB, EDT, MODE
       REAL		THRES(2), UVLIM(2)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCLIP')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, VSADD, MVSADD, VSNADD, 
     2			MWTADD, WTNADD, NVIS, NFLAG, 
     3			UADD, VADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      IF (MODE .NE. 'CLIP' .AND. MSUB .EQ. ' ') THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'MSUB is required for MODE=CLIP')
         GOTO 990
      ENDIF
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      IF (MODE .EQ. 'DIV') THEN
         MVSADD = DATADD (STRM3(NAME, MSUB, 'VIS'))
         MWTADD = DATADD (STRM3(NAME, MSUB, 'WT'))
      ELSE
         MVSADD = VSADD
         MWTADD = WTADD
      ENDIF
      VSNADD = DATADD (STRM3(NAME, EDT, 'VIS'))
      WTNADD = DATADD (STRM3(NAME, EDT, 'WT'))
      UADD = DATADD (STRM2(NAME, 'UU'))
      VADD = DATADD (STRM2(NAME, 'VV'))
C
      CALL VISCLIPP (MEMX(VSADD), MEMR(WTADD), MEMX(MVSADD),
     1   MEMR(MWTADD), MEMX(VSNADD), MEMR(WTNADD), MEMR(UADD), 
     $   MEMR(VADD), NVIS, UVLIM, THRES, NFLAG, MODE)
      IF (ERROR) GO TO 990
C
      WRITE (MESSAGE, 1100) NFLAG
 1100 FORMAT ('Clipped ',I8,' records')
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
