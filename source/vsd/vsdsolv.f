C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W%	 %G%
C
      SUBROUTINE VSDSOLV (VIS, PDT, ROTATED, REFANT)
C
CD Solve for Polarization D Terms
C
C	VIS	CH*(*)	inp	Visibility Database
C	PDT	CH*(*)	inp	Polarization "D" Terms database
C	ROTATED	L	inp	Have X-hand vis been corrected for
C				Paralactic angles?
C	REFANT	INT	inp	Reference Antenna
C
C Audit trail:
C	New
C				M.A.Holdaway	Sept 5 1992
C	Now calculates P.A. from DATEOBS, UT, OBSRA, OBSDEC, EPOCH,
C	SLAT, SLON
C				M.A.Holdaway	Sept 24 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, PDT
      LOGICAL		ROTATED
      INTEGER		REFANT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDSOLV')
C
      REAL		TINTD, TINT, RMSAVE, TINTV
      INTEGER		NANT, NDUMMY, NUMDINT, NUMVINT, NVIS, IINT, IANT
      INTEGER		IVSADD, IWTADD, IVSMADD
      INTEGER		QVSADD, QWTADD, QVSMADD
      INTEGER		UVSADD, UWTADD, UVSMADD
      INTEGER		VVSADD, VWTADD, VVSMADD
      INTEGER		TADD, BADD, DLADD, DRADD, DTADD
      INTEGER		DRLADD, RMSADD, SI1ADD, SX1ADD, SR2ADD, DRWADD,
     $   		DLWADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM), I
      REAL			SLAT, SLON, EPOCH
      CHARACTER*(8)             CTYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	DATEOBS
      DOUBLE PRECISION		OBSRA, OBSDEC, CRVAL(SYSMXDIM)

C
      LOGICAL			DATEXIST
      INTEGER			DATADD, PIXNANT, STRSEARC
      CHARACTER*(SYSMXNAM)	STRM3, STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETR (PDT, 'TINT', TINT, 1, NDUMMY)
C
      TINTD = TINT / 86400.0
      IF (TINTD .LE. 0.0) TINTD = 1.0
C
      CALL DATGETAR (STRM3(VIS, 'OBS', 'I/VIS'), NAX, NAXIS, ATYPE, 
     $   IVSADD)
      NVIS = NAXIS(1)
      IWTADD  = DATADD (STRM3(VIS, 'OBS', 'I/WT'))
      IVSMADD = DATADD (STRM3(VIS, 'MOD', 'I/VIS'))
C
      QVSADD  = DATADD (STRM3(VIS, 'OBS', 'Q/VIS'))
      QWTADD  = DATADD (STRM3(VIS, 'OBS', 'Q/WT'))
      QVSMADD = DATADD (STRM3(VIS, 'MOD', 'Q/VIS'))
C
      UVSADD  = DATADD (STRM3(VIS, 'OBS', 'U/VIS'))
      UWTADD  = DATADD (STRM3(VIS, 'OBS', 'U/WT'))
      UVSMADD = DATADD (STRM3(VIS, 'MOD', 'U/VIS'))
C
      VVSADD  = DATADD (STRM3(VIS, 'OBS', 'V/VIS'))
      VWTADD  = DATADD (STRM3(VIS, 'OBS', 'V/WT'))
      VVSMADD = DATADD (STRM3(VIS, 'MOD', 'V/VIS'))
C
      TADD = DATADD (STRM2(VIS, 'TIME'))
      BADD = DATADD (STRM2(VIS, 'BASELINE'))
      CALL PIXMDIFF (MEMR(TADD), NVIS, TINTV)
      IF (TINTV .GT. 1.0) THEN
         TINTV = 10.0/3600./24.0
         NUMVINT = 1
      ELSE
         CALL PIXNBOX (MEMR(TADD), MEMR(IWTADD), NVIS, TINTV, NUMVINT)
      ENDIF
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
         NANT = NANT + 4
 1050    FORMAT ('Number of antennas =', I4) 
         CALL MSGPUT (MESSAGE, 'I')
         NAXIS(1) = NANT
         NAXIS(2) = NUMDINT
         CALL DATMAKAR (STRM2(PDT, 'DR'), 2, NAXIS, 'X',
     1      DRADD)
         CALL DATMAKAR (STRM2(PDT, 'DRW'), 2, NAXIS, 'R',
     1      DRWADD)
         CALL DATMAKAR (STRM2(PDT, 'DL'), 2, NAXIS, 'X',
     1      DLADD)
         CALL DATMAKAR (STRM2(PDT, 'DLW'), 2, NAXIS, 'R',
     1      DLWADD)
         CALL DATMAKAR (STRM2(PDT, 'DTIME'), 1, NUMDINT, 'R',
     1      DTADD)
         CALL ARRSETCO (STRM2(PDT, 'DRW'), 0.0, 0.0)
         CALL ARRSETCO (STRM2(PDT, 'DLW'), 0.0, 0.0)
C
         NAXIS(1) = NANT*2
         NAXIS(2) = NUMDINT
         CALL DATMAKAR (STRM2(PDT, 'DRL'), 2, NAXIS, 'X',
     1      DRLADD)
         CALL DATMAKAR (STRM2(PDT, 'DRLRMS'), 2, NAXIS, 'X',
     1      RMSADD)
C
         CALL DATMAKAR (STRM2(PDT, 'ScratchI1'), 1, NANT, 'I',
     1      SI1ADD)
         CALL DATMAKAR (STRM2(PDT, 'ScratchX1'), 1, 2*NANT, 'X',
     1      SX1ADD)
         CALL DATMAKAR (STRM2(PDT, 'ScratchR2'), 1, 4*NANT*NANT, 'R',
     1      SR2ADD)
         
      ELSE
         CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, 
     1      ATYPE, DRADD)
         NANT = NAXIS(1)
         NUMDINT = NAXIS(2)
         DLADD = DATADD (STRM2(PDT, 'DL'))
         DTADD = DATADD (STRM2(PDT, 'DTIME'))
         DRLADD = DATADD (STRM2(PDT, 'DRL'))
         RMSADD = DATADD (STRM2(PDT, 'DRLRMS'))
         SI1ADD = DATADD (STRM2(PDT, 'ScratchI1'))
         SX1ADD = DATADD (STRM2(PDT, 'ScratchX1'))
         SR2ADD = DATADD (STRM2(PDT, 'ScratchR2'))
      END IF
C
      CALL VSDSOLVP (MEMX(IVSADD), MEMX(IVSMADD), MEMR(IWTADD), 
     $   MEMX(QVSADD), MEMX(QVSMADD), MEMR(QWTADD), 
     $   MEMX(UVSADD), MEMX(UVSMADD), MEMR(UWTADD), 
     $   MEMR(BADD), MEMR(TADD), ROTATED,
     $   NVIS, NANT, NUMDINT, NUMVINT, TINTD,
     $   MEMX(DRLADD), MEMR(DTADD), MEMR(DRWADD), MEMR(DLWADD),
     $   MEMI(SI1ADD), MEMX(SX1ADD), MEMR(SR2ADD),
     $   DATEOBS, OBSRA, OBSDEC, EPOCH, SLAT, SLON, REFANT)
C      CALL VSDRMSP (MEMX(IVSADD), MEMX(IVSMADD), MEMR(IWTADD), 
C     $   MEMX(QVSADD), MEMX(QVSMADD), MEMR(QWTADD), 
C     $   MEMX(UVSADD), MEMX(UVSMADD), MEMR(UWTADD), 
C     $   MEMR(BADD), MEMR(TADD),
C     $   NVIS, NANT, NUMDINT, NUMVINT, TINTD,
C     $   MEMX(DRLADD), MEMR(DTADD), MEMR(RMSADD), RMSAVE,
C     $   MEMI(SI1ADD) )
C
      CALL ARRSETCO (STRM2(PDT, 'DRLRMS'), 0.0, 0.0)
      CALL MSGPUT ('No Estimate of Errors Written Yet!', 'W')
      DO 820 IINT = 1, NUMDINT
         DO 810 IANT = 1, NANT
            MEMX(DRADD + NANT*(IINT-1) +IANT-1) =
     $         MEMX(DRLADD +2*NANT*(IINT-1) +IANT-1)
            MEMX(DLADD + NANT*(IINT-1) +IANT-1) =
     $         CONJG (MEMX(DRLADD +2*NANT*(IINT-1) +NANT+IANT-1))
 810        CONTINUE
 820     CONTINUE
C
C      WRITE (MESSAGE, 1111) RMSAVE
C 1111 FORMAT ('Average RMS in D term solution: ',F10.4)
C      CALL MSGPUT (MESSAGE, 'I')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



