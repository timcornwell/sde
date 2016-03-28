C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viscorru.f	1.6	 10/15/93
C
       SUBROUTINE VISCORRU (NAME, PHASE, GRMS, GDRIFT, NRMS, 
     $   SEED, SUB, CAL)
C
CD Corrupt Visibility data. 
C
C This creates temporary arrays to hold the antenna
C gains. The arrays have names like 'VIS/CAL/I/ANTGAIN' (NANT*NUMINT) 
C complex, and 'VIS/CAL/I/GAINTIME' (NUMINT) real, where NUMINT
C is the number of integration times.
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	PHASE	REAL	input	Rms phase error
C	GRMS	REAL	input	Rms complex antenna gain
C	NRMS	REAL	input	Rms noise per correlator
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	CAL	CH*(*)	input	Name of calibrated data
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added ability to read telescope type for NANT
C				R.Braun 	Jul 9 1989
C	References to DATDELET commented out: when it is called,
C	part of the deleted NODE is left, causing errors later on.
C				M.A.Holdaway	Dec 12 1989
C	Added GRMS, GDRIFT: complex antenna gain fluctuations
C				M.A.Holdaway	May 22 1990
C	Put back references to DATDELET
C				T.J. Cornwell	Nov 2 1990
C       Added call to PIXNANT to determine number of antennas
C                               R.G. Marson    Dec 12 1990
C	SEED is now controlled by calling program
C				M.A.Holdaway	Apr 25 1991
C	Nasty bug when some telescopes not present in the data.
C	Changed call to PIXNANT to VISNANT, (one returns number of
C	antennas present, the other returns maximum antenna number.)
C				D.S.Briggs	Oct 15 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, CAL
      REAL		PHASE, NRMS, GRMS, GDRIFT
      INTEGER		SEED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCORRU')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, GADD,
     1			BADD, WTADD, TADD, VSADD, VSNADD, 
     2			WTNADD, NVIS, NANT, NUMINT, NFLAG, 
     3			TGADD, NDUMMY
      REAL		TINT, TINTD, RMSERR
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3, TELESCOP
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
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      VSNADD = DATADD (STRM3(NAME, CAL, 'VIS'))
      WTNADD = DATADD (STRM3(NAME, CAL, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
      CALL DATGETC (NAME, 'TELESCOP', TELESCOP, 1, NDUMMY)
C
C Now find out how many integrations are required
C
      CALL PIXNBOX(MEMR(TADD), MEMR(WTADD), NVIS, TINTD, NUMINT)
      IF (NUMINT.LE.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     1      'No integration intervals found')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1000) NUMINT, TINT
 1000    FORMAT ('Found ',I6,' integration intervals, each less than ',
     1     F9.2, ' seconds')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Now make antenna gain array, and solution time array
C
      IF (.NOT.DATEXIST(STRM3(NAME, CAL, 'ANTGAIN'))) THEN
         CALL VISNANT (NAME, NANT)
         WRITE (MESSAGE, 1050) NANT
 1050    FORMAT ('Number of antennas =', I4) 
         CALL MSGPUT (MESSAGE, 'I')
         NAXIS(1) = NANT
         NAXIS(2) = NUMINT
         CALL DATMAKAR (STRM3(NAME, CAL, 'ANTGAIN'), 2, NAXIS, 'X',
     1      GADD)
         NAXIS(1) = NUMINT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM3(NAME, CAL, 'GAINTIME'), 1, NAXIS, 'R',
     1      TGADD)
      ELSE
         CALL DATGETAR (STRM3(NAME, CAL, 'ANTGAIN'), NAX, NAXIS, 
     1      ATYPE, GADD)
         NANT = NAXIS(1)
         NUMINT = NAXIS(2)
         TGADD = DATADD (STRM3(NAME, CAL, 'GAINTIME'))
      END IF
C
      CALL VISCORRP (MEMX(VSADD), PHASE, GRMS, GDRIFT, NRMS, SEED,
     $   MEMR(BADD), 
     1   MEMR(TADD), MEMR(WTADD), NVIS, NANT, TINTD, MEMX(VSNADD), 
     2   MEMR(WTNADD), NUMINT, MEMX(GADD), MEMR(TGADD), NFLAG, RMSERR)
      IF (ERROR) GO TO 990
C
      CALL DATDELET (STRM3(NAME, CAL, 'ANTGAIN'))
      CALL DATDELET (STRM3(NAME, CAL, 'GAINTIME'))
C
      WRITE (MESSAGE, 1100) RMSERR
 1100 FORMAT ('RMS error introduced = ',1PE12.3,' Jy')
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
