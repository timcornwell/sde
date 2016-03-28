C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sunmosscal.f	1.3	 7/20/92
C
      SUBROUTINE SDEMAIN
C
CD Program to selfcal mosaics of the Sun
C
CS Arguments: CALL SDEMAIN
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SUNMOSSCAL')
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
      CHARACTER*12	STRTIMC
      REAL              PSUN, TREF, SOLRAD, TSTEP, TRANGE(2)
C==================================================================
      CALL MSGWELCO ('I self-calibrate mosaics of the Sun')
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
      CALL USRGETR ('Solrad', SOLRAD, 1, NDUMMY)
      CALL USRGETR ('Psun', PSUN, 1, NDUMMY)
      CALL USRGETR ('Tref', TREF, 1, NDUMMY)
      CALL USRGETR ('Timestep', TSTEP, 1, NDUMMY)
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
      CALL IMGCLONE ('Model', 'TmpModel')
      CALL DATDELET ('IModel')
C
C Loop over pointings: flag data by uvlimits and timerange in the
C model visibility only. This will mean that solutions are calculated
C for all the data but using only the un-flagged data.
C
      IF (MODE(1:6).EQ.'AMPPHI') THEN
         CALL MSGPUT ('Correcting both amplitude and phase', 'I')
      ELSE
         CALL MSGPUT ('Correcting phase only', 'I')
      END IF
      CALL DATPUTR ('Model', 'SOLRAD', SOLRAD, 1)
      CALL DATPUTR ('Model', 'PSUN', PSUN, 1)
      CALL DATPUTR ('Model', 'TREF', TREF, 1)
      CALL DATPUTR ('Model', 'TSTEP', TSTEP/24, 1)
      CALL DATPUTR ('TmpModel', 'SOLRAD', SOLRAD, 1)
      CALL DATPUTR ('TmpModel', 'PSUN', PSUN, 1)
      CALL DATPUTR ('TmpModel', 'TREF', TREF, 1)
      CALL DATPUTR ('TmpModel', 'TSTEP', TSTEP/24, 1)
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
         CALL VISCLONE (MOSS, 'OBS', 'I', 'TMP')
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL (MOSS, 'MOD/I', TIME, UVLIMITS, NSEL)
         CALL ARRCOPY (STRM2(MOSS,'OBS/I/WT'), STRM2(MOSS,
     1      'OBS/I/OLDWT'))
         CALL ARRCOPY (STRM2(MOSS, 'TIME'), STRM2(MOSS, 'OTIME'))
         CALL VISATF (MOSS)
         CALL ARRSTAT (STRM2(MOSS,'TIME'), ' ')
         CALL DATGETR (STRM2(MOSS,'TIME'), 'ARRMIN', TRANGE(1), 1,
     $      NDUMMY)
         CALL DATGETR (STRM2(MOSS,'TIME'), 'ARRMAX', TRANGE(2), 1,
     $      NDUMMY)
         IF (TIME(1).GT.0.0) TRANGE(1) = MAX(TRANGE(1), TIME(1))
         IF (TIME(2).GT.0.0) TRANGE(2) = MIN(TRANGE(2), TIME(2))
         MESSAGE = 'Start of data = '//STRTIMC(TRANGE(1))
         CALL MSGPUT (MESSAGE, 'I')
         MESSAGE = 'End of data   = '//STRTIMC(TRANGE(2))
         CALL MSGPUT (MESSAGE, 'I')
         CALL DATPUTR (MOSS, 'TRANGE', TRANGE, 2)
         CALL MOSCLMOD ('Model', MOSS)
         CALL ARRCOPY (STRM2(MOSS, 'OTIME'), STRM2(MOSS, 'TIME'))
         CALL VISSCAL (MOSS, 'OBS/I', 'MOD/I', 'OBS/I', MODE)
         IF(ERROR) GO TO 999
         IF (THRES.GT.0.0) THEN
            WRITE (MESSAGE, 1200) THRES
 1200       FORMAT ('Edit all points with rms > ',F7.1,' sigma')
            CALL MSGPUT (MESSAGE, 'I')
            CALL VISEDIT (MOSS, 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
         END IF
         CALL DATDELET (STRM2(MOSS, 'MOD/I'))
         CALL DATDELAR (STRM2(MOSS, 'OBS/I/ANTGAIN'))
         CALL DATDELAR (STRM2(MOSS, 'OBS/I/GAINTIME'))
         CALL DATDELAR (STRM2(MOSS, 'OBS/I/ORES'))
         CALL DATDELAR (STRM2(MOSS, 'OBS/I/NRES'))
         IF (ERROR) GO TO 999
  10  CONTINUE
C
      IF (NMOSFILE.NE.' ') THEN
         CALL VISMOSPU ('Mos', NMOSFILE)
      END IF
C
 999  CONTINUE
      END
      SUBROUTINE MOSCLMOD (MODEL, VIS)
C
CD Calculate Model visibility
C
CS Arguments: CALL MOSCLMOD (MODEL, VIS)
CS
CS	MODEL	CH*(*)	input	Name of model image
CA Audit trail:
CA	Original version: Audit trail comments go on this line
CA	and successive lines
CA				T.J.Cornwell	Jan 5 1989
CE
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, VIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCLMOD')
C
      INTEGER		NDUMMY, NSEL
      DOUBLE PRECISION	OBSRA, OBSDEC
      CHARACTER*(SYSMXNAM) 	STRM2, TELESCOP
      REAL		PSUN, TREF, TSTEP, PHI, TIME(2), UVLIMITS(2)
      REAL              TRANGE(2)
      REAL	PI
      PARAMETER	(PI=3.14159274101257)
      DATA              UVLIMITS/0.0,1E10/
C==================================================================
C
      IF (ERROR) GO TO 999
C
C
      CALL DATGETR (MODEL, 'TSTEP', TSTEP, 1, NDUMMY)
      CALL DATGETR (MODEL, 'PSUN', PSUN, 1, NDUMMY)
      CALL DATGETR (MODEL, 'TREF', TREF, 1, NDUMMY)
C
C Loop over all times
C
      CALL DATGETD (VIS, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATPUTD (MODEL, 'OBSRA', OBSRA, 1)
      CALL DATPUTD ('TmpModel', 'OBSRA', OBSRA, 1)
      CALL DATGETD (VIS, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATPUTD (MODEL, 'OBSDEC', OBSDEC, 1)
      CALL DATPUTD ('TmpModel', 'OBSDEC', OBSDEC, 1)
      CALL DATGETC (VIS, 'TELESCOP', TELESCOP, 1, NDUMMY)
      CALL DATPUTC (MODEL, 'TELESCOP', TELESCOP, 1)
      CALL DATPUTC ('TmpModel', 'TELESCOP', TELESCOP, 1)
      CALL DATGETR (VIS, 'TRANGE', TRANGE, 2, NDUMMY)
      TIME(1) = TRANGE(1)
      TIME(2) = TIME(1) + TSTEP
 2    CONTINUE
      CALL ARRCOPY (STRM2(VIS,'OBS/I/OLDWT'), STRM2(VIS,
     1   'OBS/I/WT'))
      CALL VISSEL(VIS, 'OBS/I', TIME, UVLIMITS, NSEL)
      IF (PSUN.NE.0.0) THEN
         PHI =  2 * PI * (0.5*(TIME(1)+TIME(2)) - TREF)/PSUN
         CALL IMGSOLRO (MODEL, 'TmpModel', PHI)
         CALL IMGPB ('TmpModel', 'TmpModel', 'APPLY')
      ELSE
         CALL IMGPB (MODEL, 'TmpModel', 'APPLY')
      END IF
      CALL IMGGRIDC ('TmpModel', 'TmpModel', 'CORRECT')
      CALL IMGFFT ('TmpModel', 'ModelVis')
      CALL VISDEGRI (VIS, 'TMP/I', 'ModelVis')
      CALL ARRADD (STRM2(VIS, 'TMP/I/VIS'), 
     1   STRM2(VIS, 'MOD/I/VIS'), STRM2(VIS, 'MOD/I/VIS'))
      CALL ARRCOPY (STRM2(VIS,'OBS/I/OLDWT'), STRM2(VIS,
     1   'OBS/I/WT'))
      TIME(1) = TIME(2)
      TIME(2) = TIME(2) + TSTEP
      IF(TIME(2).LE.TRANGE(2)) GO TO 2
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
