C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visantpo.f	1.2    11/7/90
C
       SUBROUTINE VISANTPO (NAME, SUB)
C
CD Find antenna positions. This creates arrays to hold the antenna
C positions. The arrays have names like 'VIS/OBS/I/UPOS' (NANT*NUMINT) 
C real, where NUMINT is the number of integration times.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of subclass
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB
       REAL		FLUX
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISANTPO')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, GADD,
     1			BADD, TADD, NDUMMY, UADD, VADD, WTADD, TGADD,
     2			NVIS, NANT, NUMINT, NFLAG, UPADD, VPADD
      REAL		RESID, TINT, TINTD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      LOGICAL		DATEXIST
C=======================================================================
      IF (ERROR) GO TO 999
C
      IF (ERROR) GO TO 999
      CALL DATGETR (NAME, 'TINT', TINT, 1, NDUMMY)
      IF (ERROR) THEN
         CALL ERRCANCE
         TINT = 1.0
      END IF
      IF (TINT.EQ.0.0) TINT = 1.0
      TINTD = TINT/86400.0
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM2(NAME, 'UU'), NAX, NAXIS, ATYPE, UADD)
      NVIS = NAXIS(1)
      VADD = DATADD (STRM2(NAME, 'VV'))
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
C Now make antenna position array
C
      IF (DATEXIST(STRM3(NAME, SUB, 'GAINTIME'))) THEN
         CALL DATGETAR (STRM3(NAME, SUB, 'ANTGAIN'), NAX, NAXIS, 
     1      ATYPE, GADD)
         NANT = NAXIS(1)
         NUMINT = NAXIS(2)
         TGADD = DATADD (STRM3(NAME, SUB, 'GAINTIME'))
         NAX = 2
         NAXIS(1) = NANT
         NAXIS(2) = NUMINT
         ATYPE = 'R'
         CALL DATMAKAR (STRM3(NAME, SUB, 'UPOS'), NAX, NAXIS, 
     1      ATYPE, UPADD)
         CALL DATMAKAR (STRM3(NAME, SUB, 'VPOS'), NAX, NAXIS, 
     1      ATYPE, VPADD)
      ELSE
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     1      'No integration intervals found')
         GO TO 999
      END IF
C
      CALL VISANTPP (MEMR(BADD), MEMR(TADD), MEMR(TGADD),
     1   MEMR(WTADD), MEMR(UADD), MEMR(VADD), NVIS, NANT, TINTD, 
     2   NUMINT, MEMR(UPADD), MEMR(VPADD))
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
