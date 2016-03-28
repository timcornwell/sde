C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosscal.f	1.4    4/9/93
C
      SUBROUTINE SDEMAIN
C
CD Program to selfcal mosaics
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to VISMOSGE
C				T.J. Cornwell	Feb 3 1989
C	Changed the DELETE directory strategy;  output mosaic database
C	was previously unreadable
C				M.A.Holdaway	April 8 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSSCAL')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, MODFILE, NMOSFILE,
     1			MODE, MOSS, STRM2
      REAL		TINT, UVLIMITS(2), TIME(2), THRES
      INTEGER		NDUMMY, TIMR(8)
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		NPC, IPC, NSEL
      LOGICAL           DATEXIST
      CHARACTER*(SYSMXNAM)	TELESCOP
C==================================================================
      CALL MSGWELCO ('I self-calibrate mosaics')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      CALL USRGETC('SolType', MODE, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewMos', NMOSFILE, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
C
C Get mosaic file
C
      CALL VISMOSGE ('Mos', MOSFILE)
      CALL DATPUTR ('Mos', 'TINT', TINT, 1)
C
C Get model image
C
      CALL FILIMGGE ('IModel', MODFILE, ' ')
      CALL IMGDOUBL ('IModel', 'Model')
      CALL DATDELET ('IModel')
      CALL IMGGRIDC ('Model', 'Model', 'CORRECT')
      CALL IMGCLONE ('Model', 'PBModel')
C
C Loop over pointings: flag data by uvlimits and timerange in the
C model visibility only. This will mean that solutions are calculated
C for all the data but using only the un-flagged data.
C
      IF (MODE(1:6).EQ.'AMPPHI') THEN
         CALL MSGPUT ('Correcting both antenna amplitudes and phases',
     $      'I')
      ELSE IF (MODE(1:6).EQ.'GLOBAL') THEN
         CALL MSGPUT ('Correcting global calibration errors', 'I')
      ELSE
         CALL MSGPUT ('Correcting antenna phases only', 'I')
      END IF
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 10 IPC = 1, NPC
         WRITE (MESSAGE, 1000) IPC
 1000    FORMAT ('Pointing ',I3)
         CALL MSGPUT (MESSAGE, 'I')
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
         IF (DATEXIST(STRM2(MOSS, 'OBS/I/ANTGAIN'))) THEN
            CALL DATDELAR (STRM2(MOSS, 'OBS/I/ANTGAIN'))
            CALL DATDELAR (STRM2(MOSS, 'OBS/I/GAINTIME'))
            CALL DATDELAR (STRM2(MOSS, 'OBS/I/ORES'))
            CALL DATDELAR (STRM2(MOSS, 'OBS/I/NRES'))
         END IF
         CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
         WRITE (MESSAGE, 1100) OBSRA, OBSDEC
 1100    FORMAT ('Observed RA, DEC = ',F10.4,1X,F10.4)
         CALL MSGPUT (MESSAGE, 'I')
         CALL VISCLONE (MOSS, 'OBS', 'I', 'MOD')
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL (MOSS, 'MOD/I', TIME, UVLIMITS, NSEL)
         CALL DATPUTD ('Model', 'OBSRA', OBSRA, 1)
         CALL DATPUTD ('Model', 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC ('Model', 'TELESCOP', TELESCOP, 1)
         CALL IMGPB ('Model', 'PBModel', 'APPLY')
         CALL IMGFFT ('PBModel', 'ModMos')
         CALL VISDEGRI (MOSS, 'MOD/I', 'ModMos')
         CALL VISSCAL (MOSS, 'OBS/I', 'MOD/I', 'OBS/I', MODE)
         IF (THRES.GT.0.0) THEN
            WRITE (MESSAGE, 1200) THRES
 1200       FORMAT ('Edit all points with rms > ',F7.1,' sigma')
            CALL MSGPUT (MESSAGE, 'I')
            CALL VISEDIT (MOSS, 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
         END IF
         CALL DATDELET (STRM2(MOSS, 'MOD'))
         CALL DATDELET (STRM2(MOSS, 'OBS/I/ANTGAIN'))
         CALL DATDELET (STRM2(MOSS, 'OBS/I/GAINTIME'))
         CALL DATDELET (STRM2(MOSS, 'OBS/I/ORES'))
         CALL DATDELET (STRM2(MOSS, 'OBS/I/NRES'))
         IF (ERROR) GO TO 999
  10  CONTINUE
C
      IF (NMOSFILE.NE.' ') THEN
         CALL VISMOSPU ('Mos', NMOSFILE)
      END IF
C
 999  CONTINUE
      END
