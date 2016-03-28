C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdintr.f	1.1	 5/4/93
C
      SUBROUTINE VSDINTR (VIS, PDT, ROTATED, INTERP)
C
CD Interpolate D terms onto this data set
C
C	VIS	CH*(*)	inp	Visibility Database
C	PDT	CH*(*)	inp	Polarization "D" Terms database
C	ROTATED	L	inp	Have X-hand vis been corrected for
C				Paralactic angles?
C	INTERP	CH*(*)	inp	Inperpolation method [NEAREST | 2PT | ALL]
C
C Audit trail:
C	New
C				M.A.Holdaway	April 21 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, PDT, INTERP
      LOGICAL		ROTATED
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDINTR')
C
      INTEGER		NANT, NDUMMY, NUMDINT, NVIS
      INTEGER		IVSADD, IWTADD
      INTEGER		QVSADD, QWTADD
      INTEGER		UVSADD, UWTADD
      INTEGER		VVSADD, VWTADD
      INTEGER		TADD, BADD, DLADD, DRADD, DTADD
      INTEGER		DRWADD,	DLWADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      REAL			SLAT, SLON, EPOCH
      CHARACTER*(8)             CTYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	DATEOBS
      DOUBLE PRECISION		OBSRA, OBSDEC, CRVAL(SYSMXDIM)
C
      INTEGER			DATADD, STRSEARC
      CHARACTER*(SYSMXNAM)	STRM3, STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(VIS, 'OBS', 'I/VIS'), NAX, NAXIS, ATYPE, 
     $   IVSADD)
      NVIS = NAXIS(1)
      IWTADD  = DATADD (STRM3(VIS, 'OBS', 'I/WT'))
      QVSADD  = DATADD (STRM3(VIS, 'OBS', 'Q/VIS'))
      QWTADD  = DATADD (STRM3(VIS, 'OBS', 'Q/WT'))
      UVSADD  = DATADD (STRM3(VIS, 'OBS', 'U/VIS'))
      UWTADD  = DATADD (STRM3(VIS, 'OBS', 'U/WT'))
      VVSADD  = DATADD (STRM3(VIS, 'OBS', 'V/VIS'))
      VWTADD  = DATADD (STRM3(VIS, 'OBS', 'V/WT'))
C
      TADD = DATADD (STRM2(VIS, 'TIME'))
      BADD = DATADD (STRM2(VIS, 'BASELINE'))
C
      CALL DATGETR (VIS, 'SLON', SLON, 1, NDUMMY)
      CALL DATGETR (VIS, 'SLAT', SLAT, 1, NDUMMY)
      CALL DATGETC (VIS, 'DATE-OBS', DATEOBS, 1, NDUMMY)
      CALL DATGETR (VIS, 'EPOCH', EPOCH, 1, NDUMMY)      
C      CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
C      CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATGETD (STRM2(VIS,'OBS/I'), 'CRVAL', CRVAL, SYSMXDIM, 
     $   NDUMMY)
      CALL DATGETC (STRM2(VIS,'OBS/I'), 'CTYPE', CTYPE, SYSMXDIM,
     $   NDUMMY)
      I = STRSEARC ('RA', CTYPE, NDUMMY)
      IF (I .EQ. 0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No RA found')
         GOTO 999
      ELSE
         OBSRA = CRVAL(I)
      ENDIF
      I = STRSEARC ('DEC', CTYPE, NDUMMY)
      IF (I .EQ. 0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No DEC found')
         GOTO 999
      ELSE
         OBSDEC = CRVAL(I)
      ENDIF
C
      CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, 
     1   ATYPE, DRADD)
      NANT = NAXIS(1)
      NUMDINT = NAXIS(2)
      DLADD = DATADD (STRM2(PDT, 'DL'))
      DLWADD = DATADD (STRM2(PDT, 'DLW'))
      DRWADD = DATADD (STRM2(PDT, 'DRW'))
      DTADD = DATADD (STRM2(PDT, 'DTIME'))
C
      CALL VSDINTRP (MEMX(IVSADD), MEMR(IWTADD), 
     $   MEMX(QVSADD), MEMR(QWTADD), 
     $   MEMX(UVSADD), MEMR(UWTADD), 
     $   MEMR(BADD), MEMR(TADD), ROTATED,
     $   NVIS, NANT, NUMDINT, MEMX(DRADD), 
     $   MEMX(DLADD), MEMR(DTADD), MEMR(DRWADD), MEMR(DLWADD),
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON, INTERP)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



