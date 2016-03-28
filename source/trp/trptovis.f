C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trptovis.f	1.2	 7/20/92
C
       SUBROUTINE TRPTOVIS (TRP, TSUB, NAME, SUB, MSUB, NSUB, MODE)
C
C Derive a visibility data set from a triple product data set. The
C triple product data base must already exist since it does not make
C sense otherwise.
C
C
C      TRP     CH*(*)  input   Name of triple product entry
C	TSUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C      MSUB    CH*(*)  input   Name of model sub-class
C      NSUB    CH*(*)  input   Name of output sub-class
C      MODE    CH*(*)  input   Mode 'AMPPHI' | ' '
C
C Audit trail:
C      New routine
C				T.J.Cornwell	April 3 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, TRP, TSUB, MSUB, NSUB, MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPTOVIS')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, NTRP,
     1			BADD, WTADD, TADD, VSADD, MVSADD, VSNADD, 
     2			MWTADD, WTNADD, NVIS, NANT, NUMINT, NFLAG, 
     3			TGADD, ORADD, NRADD, TRPADD, WTTADD, TTADD,
     $                  NVSADD, NWTADD, TTTADD, NDUMMY
      REAL		ORESID, NRESID
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
C Check type of file
C
      CALL DATCHKTP (NAME, 'VIS')
      CALL DATCHKTP (TRP, 'TRP')
C
C Get addresses of all the relevant parts
C
      CALL VISNANT(NAME, NANT)
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      MVSADD = DATADD (STRM3(NAME, MSUB, 'VIS'))
      MWTADD = DATADD (STRM3(NAME, MSUB, 'WT'))
      NVSADD = DATADD (STRM3(NAME, NSUB, 'VIS'))
      NWTADD = DATADD (STRM3(NAME, NSUB, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
      CALL DATGETAR (STRM3(TRP, SUB, 'TRP'), NAX, NAXIS, ATYPE, TRPADD)
      NTRP = NAXIS(1)
      WTTADD = DATADD (STRM3(TRP, SUB, 'WT'))
      TTADD = DATADD (STRM2(TRP, 'TIME'))
      TTTADD = DATADD (STRM2(TRP, 'TRIPLE'))
C
      CALL TRPTVISP (MEMX(TRPADD), MEMR(TTTADD), MEMR(TTADD),
     $   MEMR(WTTADD), MEMX(VSADD), MEMR(BADD), MEMR(TADD), MEMR(WTADD), 
     $   MEMX(MVSADD), MEMR(MWTADD), MEMX(NVSADD), MEMR(NWTADD), NVIS,
     $   NTRP, NANT, ORESID, NRESID, NFLAG, MODE)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
