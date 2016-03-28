C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdcorr.f	1.1	 5/4/93
C
      SUBROUTINE VSDCORR (VIS, PDT, OLD, NEW)
C
CD Simulate Polarization D terms and apply them to the visibilities
C  ALL stokes (I, Q, U, V) must be present
C
C	VIS	CH*(*)	inp	Visibility Database
C	PDT	CH*(*)	inp	Polarization "D" Terms database
C	OLD	CH*(*)	inp	Old vis (ie, "OBS", but NO "/I")
C	NEW	CH*(*)	inp	New corrupted vis (ie, "OBS", but NO "/I")
C
C Audit trail:
C	New
C				M.A.Holdaway	Sept 5 1992
C	Now we actually calculate the paralactic angle rather
C	than pass it down from SIMUV's database
C				M.A.Holdaway	Sept 22 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, PDT, OLD, NEW
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDCORR')
C
      REAL		DRMS, DRIFTRMS, NRMS, TINTD, TINT, SLAT, SLON
      REAL		CFRAC, ACDRIFT
      INTEGER		NANT, NDUMMY, SEED, NUMDINT, NUMVINT, NVIS
      INTEGER		IVSADD, IWTADD, IVSNADD, IWTNADD
      INTEGER		QVSADD, QWTADD, QVSNADD, QWTNADD
      INTEGER		UVSADD, UWTADD, UVSNADD, UWTNADD
      INTEGER		VVSADD, VWTADD, VVSNADD, VWTNADD
      INTEGER		TADD, BADD, DLADD, DRADD, DTADD, STADD
      INTEGER		DRWADD, DLWADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RMSI, RMSQ, RMSU, RMSV, EPOCH
      CHARACTER*(SYSMXNAM)	DATEOBS
      DOUBLE PRECISION	OBSRA, OBSDEC
C
      LOGICAL			DATEXIST
      INTEGER			DATADD, PIXNANT
      CHARACTER*(SYSMXNAM)	STRM3, STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETI (PDT, 'SEED', SEED, 1, NDUMMY)
      CALL DATGETR (PDT, 'DRMS', DRMS, 1, NDUMMY)
      CALL DATGETR (PDT, 'NRMS', NRMS, 1, NDUMMY)
      CALL DATGETR (PDT, 'DRIFTRMS', DRIFTRMS, 1, NDUMMY)
      CALL DATGETR (PDT, 'ACDRIFT', ACDRIFT, 1, NDUMMY)
      CALL DATGETR (PDT, 'TINT', TINT, 1, NDUMMY)
      CALL DATGETR (PDT, 'CorrFrac', CFRAC, 1, NDUMMY)
C
      TINTD = TINT / 86400.0
      IF (TINTD .LE. 0.0) TINTD = 1.0
C
      CALL DATGETAR (STRM3(VIS, OLD, 'I/VIS'), NAX, NAXIS, ATYPE, 
     $   IVSADD)
      NVIS = NAXIS(1)
      IWTADD  = DATADD (STRM3(VIS, OLD, 'I/WT'))
      IVSNADD = DATADD (STRM3(VIS, NEW, 'I/VIS'))
      IWTNADD = DATADD (STRM3(VIS, NEW, 'I/WT'))
C
      QVSADD  = DATADD (STRM3(VIS, OLD, 'Q/VIS'))
      QWTADD  = DATADD (STRM3(VIS, OLD, 'Q/WT'))
      QVSNADD = DATADD (STRM3(VIS, NEW, 'Q/VIS'))
      QWTNADD = DATADD (STRM3(VIS, NEW, 'Q/WT'))
C
      UVSADD  = DATADD (STRM3(VIS, OLD, 'U/VIS'))
      UWTADD  = DATADD (STRM3(VIS, OLD, 'U/WT'))
      UVSNADD = DATADD (STRM3(VIS, NEW, 'U/VIS'))
      UWTNADD = DATADD (STRM3(VIS, NEW, 'U/WT'))
C
      VVSADD  = DATADD (STRM3(VIS, OLD, 'V/VIS'))
      VWTADD  = DATADD (STRM3(VIS, OLD, 'V/WT'))
      VVSNADD = DATADD (STRM3(VIS, NEW, 'V/VIS'))
      VWTNADD = DATADD (STRM3(VIS, NEW, 'V/WT'))
C
      TADD = DATADD (STRM2(VIS, 'TIME'))
      BADD = DATADD (STRM2(VIS, 'BASELINE'))
      STADD = DATADD (STRM2(VIS, 'STIME'))
      NUMVINT = NAXIS(2)
C
      CALL DATGETR (VIS, 'SLON', SLON, 1, NDUMMY)
      CALL DATGETR (VIS, 'SLAT', SLAT, 1, NDUMMY)
      CALL DATGETC (VIS, 'DATE-OBS', DATEOBS, 1, NDUMMY)
      CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATGETR (VIS, 'EPOCH', EPOCH, 1, NDUMMY)      
C
C Now find out how many D term time intervals are required
C
      CALL PIXNBOX(MEMR(TADD), MEMR(IWTADD), NVIS, TINTD, NUMDINT)
      IF (NUMDINT.LE.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE, 
     1      'No integration intervals found')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1000) NUMDINT, TINT
 1000    FORMAT ('Found ',I6,' D term intervals, each less than ',
     1     F9.2, ' seconds')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      IF (.NOT.DATEXIST(STRM2(PDT, 'DR'))) THEN
         NANT = PIXNANT(MEMR(BADD), NVIS)
         WRITE (MESSAGE, 1050) NANT
 1050    FORMAT ('Number of antennas =', I4) 
         CALL MSGPUT (MESSAGE, 'I')
         NAXIS(1) = NANT
         NAXIS(2) = NUMDINT
         CALL DATMAKAR (STRM2(PDT, 'DR'), 2, NAXIS, 'X',
     1      DRADD)
         CALL DATMAKAR (STRM2(PDT, 'DL'), 2, NAXIS, 'X',
     1      DLADD)
         CALL DATMAKAR (STRM2(PDT, 'DRW'), 2, NAXIS, 'R',
     1      DRWADD)
         CALL DATMAKAR (STRM2(PDT, 'DLW'), 2, NAXIS, 'R',
     1      DLWADD)
         CALL ARRSETCO (STRM2(PDT, 'DRW'), 0.0, 1.0)
         CALL ARRSETCO (STRM2(PDT, 'DLW'), 0.0, 1.0)
         NAXIS(1) = NUMDINT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM2(PDT, 'DTIME'), 1, NAXIS, 'R',
     1      DTADD)
      ELSE
         CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, 
     1      ATYPE, DRADD)
         NANT = NAXIS(1)
         NUMDINT = NAXIS(2)
         DLADD = DATADD (STRM2(PDT, 'DL'))
         DTADD = DATADD (STRM2(PDT, 'DTIME'))
      END IF
C
      CALL VSDCORRP (MEMX(IVSADD), MEMR(IWTADD), 
     $   MEMX(QVSADD), MEMR(QWTADD), MEMX(UVSADD), MEMR(UWTADD), 
     $   MEMX(VVSADD), MEMR(VWTADD), 
     $   MEMR(BADD), MEMR(TADD), MEMR(STADD),
     $   NVIS, NANT, NUMDINT, NUMVINT, TINTD,
     $   MEMX(DRADD), MEMX(DLADD), MEMR(DTADD), 
     $   SEED, DRMS, DRIFTRMS, CFRAC, ACDRIFT, NRMS,
     $   MEMX(IVSNADD), MEMR(IWTNADD), MEMX(QVSNADD), MEMR(QWTNADD),
     $   MEMX(UVSNADD), MEMR(UWTNADD), MEMX(VVSNADD), MEMR(VWTNADD),
     $   RMSI, RMSQ, RMSU, RMSV,
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON)
C
      WRITE (MESSAGE, 1111) RMSI, RMSQ, RMSU, RMSV
 1111 FORMAT ('RMS Error introduced, IQUV: ',4(F8.4,1X))
      CALL MSGPUT (MESSAGE, 'I')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



