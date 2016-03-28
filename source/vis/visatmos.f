C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visatmos.f	1.1    5/28/91
C
      SUBROUTINE VISATMOS (NAME, SUBIN, SUBOUT, NRMS, ATMOS)
C
CD Applies the ATMOSphere to the VIS
C
C 
C
C	NAME	CH*(*)	input	Name of VIS directory entry
C	SUBIN	CH*(*)	input	Name of in sub-class (OBS/I)
C	SUBOUT	CH*(*)	input	Name of out sub-class (OBS/I)
C	NRMS	real	input	RMS noise added      
C	ATMOS	CH*(*)	input	Name of ATMOSPHERE
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 9 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME, SUBIN, SUBOUT, ATMOS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISATMOS')
C
      INTEGER	NDUMMY, I, SEED,
     $   	NAX, NAXIS(SYSMXDIM), NVIS, NUMINT, NANT, NFLAG
      INTEGER	WTADD, WTNADD, VSNADD, VSADD, TADD,
     $   	BADD, GADD, TGADD, WATERADD
C
      REAL	TINT, TINTD, REFINDEX, ABSCOEFF, TSKY,
     $   	RMSERR, LAMBDA, NRMS
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      INTEGER			DATADD, PIXNANT
      LOGICAL			DATEXIST
      INTEGER			STRSEARC
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETR  (ATMOS, 'REFINDEX', REFINDEX, 1, NDUMMY)
      CALL DATGETR  (ATMOS, 'ABSCOEFF', ABSCOEFF, 1, NDUMMY)
      CALL DATGETR  (ATMOS, 'TSKY', TSKY, 1, NDUMMY)
C     
      SEED = 12345
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
      CALL DATGETAR (STRM3(NAME, SUBIN, 'VIS'), NAX, NAXIS, ATYPE, 
     $   VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUBIN, 'WT'))
      VSNADD = DATADD (STRM3(NAME, SUBOUT, 'VIS'))
      WTNADD = DATADD (STRM3(NAME, SUBOUT, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL DATGETD (STRM2(NAME,SUBIN), 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (STRM2(NAME,SUBIN), 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      I = STRSEARC ('FREQ', CTYPE, NDUMMY)
      IF (I.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No frequency axis')
         GO TO 999
      ELSE
         LAMBDA = 3.0E+8/CRVAL(I) 
      END IF
      
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
      IF (.NOT.DATEXIST(STRM3(NAME, SUBOUT, 'ANTGAIN'))) THEN
         NANT = PIXNANT(MEMR(BADD), NVIS)
         WRITE (MESSAGE, 1050) NANT
 1050    FORMAT ('Number of antennas =', I4) 
         CALL MSGPUT (MESSAGE, 'I')
         NAXIS(1) = NANT
         NAXIS(2) = NUMINT
         CALL DATMAKAR (STRM3(NAME, SUBOUT, 'ANTGAIN'), 2, NAXIS, 'X',
     1      GADD)
         NAXIS(1) = NUMINT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM3(NAME, SUBOUT, 'GAINTIME'), 1, NAXIS, 'R',
     1      TGADD)
      ELSE
         CALL DATGETAR (STRM3(NAME, SUBOUT, 'ANTGAIN'), NAX, NAXIS, 
     1      ATYPE, GADD)
         NANT = NAXIS(1)
         NUMINT = NAXIS(2)
         TGADD = DATADD (STRM3(NAME, SUBOUT, 'GAINTIME'))
      END IF
C
      WATERADD = DATADD (STRM2 (ATMOS, 'AtmoW'))
      IF (ERROR) GOTO 990
      CALL VISATMOP (MEMX(VSADD), MEMR(BADD), 
     1   MEMR(TADD), MEMR(WTADD), NVIS, NANT, TINTD,
     $   MEMR(WATERADD), REFINDEX, ABSCOEFF, TSKY, LAMBDA, NRMS, 
     $   SEED, MEMX(VSNADD), MEMR(WTNADD), NUMINT, MEMX(GADD), 
     $   MEMR(TGADD), NFLAG, RMSERR)
      IF (ERROR) GO TO 990
C
      CALL DATDELET (STRM3(NAME, SUBOUT, 'ANTGAIN'))
      CALL DATDELET (STRM3(NAME, SUBOUT, 'GAINTIME'))
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
