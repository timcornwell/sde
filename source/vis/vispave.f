C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vispave.f	1.3	 6/21/93
C
      SUBROUTINE VISPAVE (VIS, ROTATE, TXTFILE, DIVI)
C
CD Find average value of (RL + LR*)/2
C
C	VIS	CH*(*)	inp	Visibility Database
C	ROTATE	I	inp	Par Ang rotation: -1=undo, 0=nothing, 1=do
C	TXTFILE	CH*(*)	inp	Name of txt file for AVE vs TIME
C	DIVI	L	inp	Divide by mean I?
C
C Audit trail:
C	New
C				M.A.Holdaway	April 22 1993
C
C	Changed to get CRVAL and CRTYPE instead of OBSRA, OBSDEC
C				M.A. Holdaway	May 10 1993
C	Changed ROTATE to DO, UNDO, and NOTHING
C				M.A. Holdaway	May 12 1993
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, TXTFILE
      LOGICAL		DIVI
      INTEGER		ROTATE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISPAVE')
C
      INTEGER		NANT, NDUMMY, NVIS
      INTEGER		IVSADD, IWTADD
      INTEGER		VVSADD, VWTADD
      INTEGER		QVSADD, QWTADD
      INTEGER		UVSADD, UWTADD
      INTEGER		TADD, BADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      REAL			SLAT, SLON, EPOCH
      CHARACTER*(SYSMXNAM)	DATEOBS
      CHARACTER*(8)		CTYPE(SYSMXDIM)
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
      CALL DATGETAR (STRM3(VIS, 'OBS', 'Q/VIS'), NAX, NAXIS, ATYPE, 
     $   QVSADD)
      NVIS = NAXIS(1)
      QWTADD  = DATADD (STRM3(VIS, 'OBS', 'Q/WT'))
      UVSADD  = DATADD (STRM3(VIS, 'OBS', 'U/VIS'))
      UWTADD  = DATADD (STRM3(VIS, 'OBS', 'U/WT'))
      IVSADD  = DATADD (STRM3(VIS, 'OBS', 'I/VIS'))
      IWTADD  = DATADD (STRM3(VIS, 'OBS', 'I/WT'))
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
      CALL VISPAVEP (MEMX(IVSADD), MEMR(IWTADD), 
     $   MEMX(QVSADD), MEMR(QWTADD), 
     $   MEMX(UVSADD), MEMR(UWTADD), 
     $   MEMX(VVSADD), MEMR(VWTADD), 
     $   MEMR(BADD), MEMR(TADD), ROTATE, NVIS, NANT, 
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON, DIVI, TXTFILE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



