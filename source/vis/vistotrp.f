C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistotrp.f	1.2    11/7/90
C
       SUBROUTINE VISTOTRP (NAME, SUB, ASUB, TRP, TSUB, NPH, NFRAMES)
C
CD Insert a visibility data set into a triple product data set. The
C triple product data base must already exist since it does not make
C sense otherwise. If NPH and NFRAMES are both non-zero then photon
C noise is added.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	ASUB	CH*(*)	input	Name of sub-class for amplitudes e.g. OBS/I
C      TRP     CH*(*)  input   Name of triple product entry
C	TSUB	CH*(*)	input	Name of sub-class for triples e.g. OBS/I
C	NPH	REAL	input	Average total number of photons/frame
C	NFRAMES	INT	input	Number of frames/integration
C
C Audit trail:
C      New routine
C				T.J.Cornwell	March 15 1989
C	Added photon noise capability
C				T.J. Cornwell	October 23 1989
C	Added separate subclass for triple
C				T.J. Cornwell	October 24 1989
C	Fixed extra definition of NPH, NFRAMES
C				T.J. Cornwell	November 2 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, TRP, ASUB, TSUB
      REAL		NPH
      INTEGER		NFRAMES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTOTRP')
C
      INTEGER		IAX, NAX, NAXIS(SYSMXDIM), DATADD, NTRP,
     1			BADD, WTADD, TADD, VSADD, MVSADD, VSNADD, 
     2			MWTADD, WTNADD, NVIS, NANT, 
     3			RADD, TRPADD, WTTADD, TTADD, NVSADD, NWTADD,
     $                  TTTADD, NDUMMY
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
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
      CALL DATGETI (TRP, 'NANT', NANT, 1, NDUMMY)
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      NVSADD = DATADD (STRM3(NAME, ASUB, 'VIS'))
      NWTADD = DATADD (STRM3(NAME, ASUB, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
      CALL DATGETAR (STRM3(TRP, TSUB, 'TRP'), NAX, NAXIS, ATYPE, TRPADD)
      NTRP = NAXIS(1)
      WTTADD = DATADD (STRM3(TRP, TSUB, 'WT'))
      TTADD = DATADD (STRM2(TRP, 'TIME'))
      TTTADD = DATADD (STRM2(TRP, 'TRIPLE'))
C
      CALL VISTTRPP (MEMX(VSADD), MEMR(BADD), MEMR(TADD), MEMR(WTADD),
     $   NVIS, MEMX(TRPADD), MEMR(TTTADD), MEMR(TTADD), MEMR(WTTADD),
     $   MEMX(NVSADD), MEMR(NWTADD), NTRP, NANT, NPH, NFRAMES)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
