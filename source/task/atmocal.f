C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)atmocal.f	1.1	 1/24/92
C
      SUBROUTINE SDEMAIN
C
CD From a model atmosphere, determine the RMS in T, interferometer phase
C
C Audit trail:
C	Original Version
C	Stollen from ATMOPHAS
C				M.A.Holdaway	Sep 19 1991
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ATMOCAL')
C
      CHARACTER*(SYSMXNAM)	ATMOS, ANTFILE, TPAFILE, VISFILE, 
     $   			TPVISFIL, VIS, TPVIS, TPTELE,
     $   			TPAVFIL, CVISFILE, MODE
      REAL		ATAVE, REFINDEX, ABSCOEFF, TSKY, HEIGHT
      REAL		ATVEL(2), DEC, DECC, HAC, TCYC, TCAL, FREQ
      REAL		TGAIN
      REAL		AUTOW
      LOGICAL		AUTOC, DOCAL, ELCORR
C
      INTEGER		NDUMMY, NUMINT, I, NCYCLE, NCAL, IS, IC
      INTEGER		NAXIS(SYSMXDIM), NAX, NANT, IANT(40)
      REAL		ROTA(SYSMXDIM), DELT(SYSMXDIM), RPIX(SYSMXDIM)
      REAL		AVE, ELMIN, TIME, HMIN, HMAX, TINT, TINTD, 
     $   		TPDIAM, RPIX2(2), SCALE, RFACT, TIMER(2),
     $   		GAINAVE
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
      INTEGER		ENAX, ENAXIS, EADD
      CHARACTER*1	ETYPE
      REAL		SOURCEL, CALEL, ELFACT, D2R, TINTBOXC
C==================================================================
      CHARACTER*8	STRINT
      CHARACTER*(SYSMXNAM)	STRM2
      REAL		DATFGETR
C
      DATA		NAXIS /SYSMXDIM * 1/
      DATA		ROTA /SYSMXDIM * 0.0/
      DATA		DELT /SYSMXDIM * 0.0/
      DATA		RPIX /SYSMXDIM * 1.0/
      DATA		RVAL /SYSMXDIM * 0.0/
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I determine errors in calibration from mod ATMOS')
C
C Call user interface routine, and set debug status
C      
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Atmosphere', ATMOS, 1, NDUMMY)
      CALL USRGETC ('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC ('TPAfile', TPAFILE, 1, NDUMMY)
      CALL USRGETC ('TPTelescope', TPTELE, 1, NDUMMY)
      CALL USRGETR ('TPTelDiam', TPDIAM, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('CVis', CVISFILE, 1, NDUMMY)
      CALL USRGETC ('TPVis', TPVISFIL, 1, NDUMMY)
      CALL USRGETC ('TPAllanVar', TPAVFIL, 1, NDUMMY)      
C
      CALL USRGETR ('RefractIndex', REFINDEX, 1, NDUMMY)
      CALL USRGETR ('TSKY', TSKY, 1, NDUMMY)
      CALL USRGETR ('AtVelocity', ATVEL, 2, NDUMMY)
      CALL USRGETR ('AtHeight', HEIGHT, 1, NDUMMY)
      CALL USRGETR ('AveMM', ATAVE, 1, NDUMMY)
      CALL USRGETR ('Rpix', RPIX2, 2, NDUMMY)
C
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('Dec',  DEC,  1, NDUMMY)
      CALL USRGETR ('DecC',  DECC,  1, NDUMMY)
      CALL USRGETL ('ElCorr',  ELCORR,  1, NDUMMY)
      CALL USRGETR ('HAC',  HAC,  1, NDUMMY)
      CALL USRGETR ('Tcycle',  TCYC,  1, NDUMMY)
      CALL USRGETR ('Tcal',  TCAL,  1, NDUMMY)
      CALL USRGETC ('GainMode', MODE, 1, NDUMMY)
      CALL USRGETR ('BoxcarAvet', TINTBOXC, 1, NDUMMY)
      CALL USRGETR ('Tint', TINT, 1, NDUMMY)
      CALL USRGETI ('Nint', NUMINT, 1, NDUMMY)
      CALL USRGETL ('AutoC', AUTOC, 1, NDUMMY)
      CALL USRGETR ('AutoW', AUTOW, 1, NDUMMY)
C
C Fiddle with the atmosphere
C
      CALL FILIMGGE ('Atmos', ATMOS, ' ')
      IF (ATAVE .NE. 0.0) THEN
         CALL ARRSTAT  ('Atmos', ' ')
         CALL DATGETR  ('Atmos', 'ARRAVE', AVE, 1, NDUMMY)
         IF (AVE .NE. 0.0) THEN
            CALL ARRSCALE ('Atmos', ATAVE/AVE, 0.0, 'Atmos')
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $         'Atmosphere has zero average')
            GOTO 999
         ENDIF
      ENDIF
C 1mm H2O ==> opacity of .06
      ABSCOEFF = .06
      CALL DATPUTR  ('Atmos', 'REFINDEX', REFINDEX, 1)
      CALL DATPUTR  ('Atmos', 'ABSCOEFF', ABSCOEFF, 1)
      CALL DATPUTR  ('Atmos', 'TSKY',     TSKY, 1)
      CALL DATPUTR  ('Atmos', 'VELOCITY', ATVEL, 2)
      CALL DATPUTR  ('Atmos', 'HEIGHT',   HEIGHT, 1)
      CALL DATPUTC  ('Atmos', 'TELESCOP', TPTELE, 1)
      CALL DATPUTR  ('Atmos', 'TELDIAM',  TPDIAM, 1)
      CALL CRDGET ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RPIX(1) = RPIX2(1)
      RPIX(2) = RPIX2(2)
      RVAL(4) = FREQ
      CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GOTO 999
C      
C Antfile is for the interferometer;  TPfile is for the total power device
C
      CALL FILGETAN ('Antfile', ANTFILE)
      CALL FILGETAN ('TPAfile', TPAFILE)
      IF (ERROR) GOTO 999
C
C Look at the Zenith as atmosphere blows over us
C      
      ELMIN = 10.
      TIME = 0.0
      HMIN = 0.0
      HMAX = TINT /3600.
      TINTD = TINT/(3600.D0 * 24.D0)
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, AUTOW, 
     $   'IVis')
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DECC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, AUTOW, 
     $   'CVis')
      CALL SIMUV (DBLE(FREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), .TRUE., 1.0, 
     $   'TPVis')
C
      CALL DATGETAR ('IVis/EL', ENAX, ENAXIS, ETYPE, EADD)
      SOURCEL = MEMR(EADD)
      CALL DATGETAR ('CVis/EL', ENAX, ENAXIS, ETYPE, EADD)
      CALEL = MEMR(EADD)
      WRITE (MESSAGE, 1754) SOURCEL, CALEL
 1754 FORMAT ('Source elevation = ',F10.2, 
     $        '  Calibrator elevation = ', F10.2)
      CALL MSGPUT (MESSAGE, 'I')
      D2R = ATAN2 (1.0, 1.0) /45.D0
      ELFACT = SIN (CALEL * D2R) / SIN (SOURCEL * D2R)
C
C Shift the reference values of 'Atmos' to account for AZ, EL, HEIGHT
C
      CALL CRDATMSH ('IVis', 'Atmos')
      CALL MSGPUT  ('Shifted Atmosphere''s Coordinates', 'D')
      CALL CRDLIST ('Atmos')
C
C Get the scaling factor for the total power beam
C
      CALL DATPUTR  ('TPVis', 'TELDIAM',  TPDIAM, 1)
      CALL IMGCLONE ('Atmos', 'PBAtmos')
      CALL ARRSETCO ('PBAtmos', 0.0, 1.0)
      CALL CRDATMPO ('PBAtmos', 'TPVis')
      CALL IMGPB    ('PBAtmos', 'PBAtmos', 'APPLY')
      CALL ARRSTAT  ('PBAtmos', ' ')
      SCALE = 1./DATFGETR ('PBAtmos', 'ARRSUM')
      CALL ARRSETCO ('PBAtmos', 0.0, 0.0)
      IF (ERROR) GOTO 990
C
C Make the image (1 - EXP^{-ABSCOEFF * MMWATER}) * TSKY
C
      CALL IMGCLONE ('Atmos', 'Emission')
      RFACT = -ABSCOEFF
      CALL ARREXP   ('Atmos', RFACT, 0.0, 'Emission')
      CALL ARRSCALE ('Emission', -1.0, 1.0, 'Emission')
      CALL ARRSCALE ('Emission', TSKY, 0.0, 'Emission')
      IF (ERROR) GOTO 990
C
C This is ASS backwards!  But it is required to always be looking
C at the SAME POINT ON THE SKY
C
      NCYCLE = NINT(TCYC/TINT)
      NCAL   = NINT(TCAL/TINT)
      IF (NCAL .GE. NCYCLE) THEN
         CALL MSGPUT ('Calibration time exceeds cycle time', 'W')
         NCYCLE = NCAL + 1
      ENDIF
      IS = 0
      IC = 0
      DO 1000 I = 1, NUMINT
C
C Determine: Do we do source or calibrator?
C
         IF (MOD(I, NCYCLE) .LT. NCAL .OR. I .EQ. 1 .OR.
     $      I .EQ. NUMINT) THEN
            IC = IC + 1
            DOCAL = .TRUE.
            CALL MSGPUT ('Calibrator: Integration '//STRINT(IC), 'I')
            VIS = 'CVis/PC1I'//STRINT(IC)
         ELSE
            IS = IS + 1
            DOCAL = .FALSE.
            CALL MSGPUT ('Source: Integration '//STRINT(IS), 'I')
            VIS = 'IVis/PC1I'//STRINT(IS)
         ENDIF
C
         TPVIS = 'TPVis/PC1I'//STRINT(I)
         IF (DOCAL) THEN
            HMIN = HAC
            HMAX = HMIN + TINT /3600.
            CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $         DBLE(DECC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, 
     $         AUTOW, VIS)
         ELSE
            HMIN = 0.0
            HMAX = 0.0 + TINT/3600.
            CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $         DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, 
     $         AUTOW, VIS)
         ENDIF
         CALL SIMUV (DBLE(FREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
     $      DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), .TRUE.,
     $      1.0, TPVIS)
C
C "Point Source Model", Apply the atmosphere to the Interferometer phases
C
         CALL ARRSETCO (STRM2 (VIS, 'OBS/I/VIS'), 0.0, 1.0)
         CALL CRDATMPO ('Atmos', VIS)
         CALL VISATMOS (VIS, 'OBS/I', 'OBS/I', 0.0, 'Atmos')
C
         CALL ARRSETCO ('PBAtmos', 0.0, 0.0)
         CALL DATPUTR  (TPVIS, 'TELDIAM',  TPDIAM, 1)
         CALL CRDATMPO ('Emission', TPVIS)
         CALL IMGDFTPB (TPVIS, 'OBS/I', 'Emission', 'PBAtmos')
         CALL ARRSCALE (STRM2 (TPVIS, 'OBS/I/VIS'), SCALE, 0.0,
     $      STRM2 (TPVIS, 'OBS/I/VIS'))
C
         TIME = TIME + TINTD
         IF (ERROR) GOTO 990
 1000 CONTINUE
C
C*************** INT DATA *******************************************
C
C
C Reorganize Interferometer and Calibrator data
C
      CALL VISREORG ('IVis', 'IVis', 1, IS)
      DO 2000 I = 1, IS
         VIS = 'IVis/PC1I'//STRINT(I)
         CALL DATDELET (VIS)
 2000 CONTINUE
      CALL VISREORG ('CVis', 'CVis', 1, IC)
      DO 2020 I = 1, IC
         VIS = 'CVis/PC1I'//STRINT(I)
         CALL DATDELET (VIS)
 2020 CONTINUE
C
C Create 1JY point source model;   Calibrate the calibrator data
C
      IF (SYSDEBUG) THEN
         CALL VISPUT ('IVis/PC1', 'VIS.SDE', 'OBS', 'I', '*', ' ')
         CALL VISPUT ('CVis/PC1', 'CAL.SDE', 'OBS', 'I', '*', ' ')      
      ENDIF
      CALL VISCLONE ('CVis/PC1', 'OBS', 'I', 'MOD')
      CALL ARRSETCO (STRM2 ('CVis/PC1', 'MOD/I/VIS'), 0.0, 1.0)
      TGAIN = 1.1 * TCAL
      CALL DATPUTR ('CVis/PC1', 'TINT', TGAIN, 1)
      CALL VISSCAL  ('CVis/PC1', 'OBS/I', 'MOD/I', 'OBS/I', ' ')
C
C Scale Gain Phases by ELevation FACTor
C
      IF (ELCORR) THEN
         WRITE (MESSAGE, 1083) ELFACT
 1083    FORMAT ('Correcting gains by elevation factor: ',F10.6)
         CALL MSGPUT (MESSAGE, 'I')
         CALL ARRX2AP ('CVis/PC1/OBS/I/ANTGAIN', 'GAINAMP', 'GAINPH')
         CALL ARRSCALE ('GAINPH', ELFACT, 0.0, 'GAINPH')
         CALL ARRAP2X ('GAINAMP', 'GAINPH', 'CVis/PC1/OBS/I/ANTGAIN')
      ENDIF
C
C List Gains for Debugging
C
      IF (SYSDEBUG) THEN
         TIMER(1) = 0.0
         TIMER(2) = 0.0
         CALL DATGETI ('Antfile', 'NANT', NANT, 1, NDUMMY)
         DO 2030 I = 1, NANT
            IANT(I) = I
 2030    CONTINUE
         CALL TXTOPEN ('Gains', 'GAINS.LIST', 'WRITE')
         CALL GAILIST ('Gains', 'CVis/PC1', 'OBS/I', TIMER, IANT, NANT)
         CALL TXTCLOSE ('Gains')
      ENDIF
C
C Apply Gains to Source visibilities; write out interferometric vis files
C
      IF (MODE .NE. '2PT') MODE = 'BOXCAR'
      CALL DATPUTC  ('IVis/PC1/OBS/I', 'GAITYPE', MODE, 1)
      CALL DATPUTC  ('IVis/PC1/OBS/I', 'GAIAVG', 'AMPPHI', 1)
      GAINAVE =  TINTBOXC/3600./24.0
      CALL DATPUTR  ('IVis/PC1/OBS/I', 'GAITINT', GAINAVE, 1)
      CALL GAIAPPLY ('IVis/PC1', 'OBS/I', 'CVis/PC1', 'OBS/I', 
     $   'IVis/PC1', 'OBS/I')
C
C
      IF (VISFILE .NE. ' ') THEN
         CALL CRDGET ('IVis/PC1/OBS/I', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL CRDPUT ('IVis/PC1', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL DATPUTR ('IVis/PC1', 'REFDATE', 0.0, 1)
         CALL VISPUT ('IVis/PC1', VISFILE, 'OBS', 'I', '*', ' ')
      ENDIF
      IF (CVISFILE .NE. ' ') THEN
         CALL CRDGET ('CVis/PC1/OBS/I', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL CRDPUT ('CVis/PC1', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL DATPUTR ('CVis/PC1', 'REFDATE', 0.0, 1)
         CALL VISPUT ('CVis/PC1', CVISFILE, 'OBS', 'I', '*', ' ')
      ENDIF
C
C*************** TP DATA *******************************************
C
C Reorganize TP stability data
C
      CALL VISREORG ('TPVis', 'TPVis', 1, NUMINT)
      DO 2100 I = 1, NUMINT
         TPVIS = 'TPVis/PC1I'//STRINT(I)
         CALL DATDELET (VIS)
 2100 CONTINUE
      IF (TPVISFIL .NE. ' ') THEN
         CALL VISPUT ('TPVis/PC1', TPVISFIL, 'OBS', 'I', '*', ' ')
      ENDIF
      IF (TPAVFIL .NE. ' ') THEN
         CALL ARRREAL ('TPVis/PC1/OBS/I/VIS', 'TPReal')
         CALL ARRALVAR ('TPReal', TINT,  'AllanVar')
         CALL FILALVPU ('AllanVar', TPAVFIL)
      ENDIF
C
 990  IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      ENDIF
 999  CONTINUE
      END





