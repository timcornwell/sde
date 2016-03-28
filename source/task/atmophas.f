C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)atmophas.f	1.8	 5/8/95
C
      SUBROUTINE SDEMAIN
C
CD From a model atmosphere, determine the RMS in T, interferometer phase
C
C Audit trail:
C	Original Version
C				M.A.Holdaway	May 8 1991
C	Added ALLAN VARIANCE calculations
C				M.A.Hldaway	June 26 1991
C	Discovered an error relating to ABSCOEFF
C				M.A.Holdaway	Sept 12 1991
C	FREQ (interferometer) and TFREQ (Tipper, Total power)
C	are now different quantities
C				M.A.Holdaway	Nov 15 1991
C	Added REFDATE, put coordinate values in top vis directory
C	for outfile
C				M.A.Holdaway	Dec 10 1991
C	Time is now the 4th axis, Freq is the 5th axis
C	Added absorption coefficient as input
C				M.A.Holdaway	May 8 1995
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ATMOPHAS')
C
      CHARACTER*(SYSMXNAM)	ATMOS, ANTFILE, TPAFILE, VISFILE, 
     $   			TPVISFIL, VIS, TPVIS, TPTELE,
     $   			AVFIL, TPAVFIL
      REAL		ATAVE, REFINDEX, ABSCOEFF, TSKY, HEIGHT
      REAL		ATVEL(2), DEC, FREQ, TFREQ
      REAL		AUTOW
      LOGICAL		AUTOC
C
      INTEGER		NDUMMY, NUMINT, I
      INTEGER		NAXIS(SYSMXDIM), NAX
      REAL		ROTA(SYSMXDIM), DELT(SYSMXDIM), RPIX(SYSMXDIM)
      REAL		AVE, ELMIN, TIME, HMIN, HMAX, TINT, TINTD, 
     $   		TPDIAM, RPIX2(2), SCALE, RFACT
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      CHARACTER*8	TYPE(SYSMXDIM)
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
      CALL MSGWELCO ('I apply a model atmosphere to point source')
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
      CALL USRGETC ('TPVis', TPVISFIL, 1, NDUMMY)
      CALL USRGETC ('AllanVar', AVFIL, 1, NDUMMY)
      CALL USRGETC ('TPAllanVar', TPAVFIL, 1, NDUMMY)      
C
      CALL USRGETR ('RefractIndex', REFINDEX, 1, NDUMMY)
      CALL USRGETR ('Absorption', ABSCOEFF, 1, NDUMMY)
      CALL USRGETR ('TSKY', TSKY, 1, NDUMMY)
      CALL USRGETR ('AtVelocity', ATVEL, 2, NDUMMY)
      CALL USRGETR ('AtHeight', HEIGHT, 1, NDUMMY)
      CALL USRGETR ('AveMM', ATAVE, 1, NDUMMY)
      CALL USRGETR ('Rpix', RPIX2, 2, NDUMMY)
C
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('TFreq', TFREQ, 1, NDUMMY)
      CALL USRGETR ('Dec',  DEC,  1, NDUMMY)
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
C
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
      RVAL(5) = FREQ
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
      ELMIN = 25.
      TIME = 0.0
      HMIN = 0.0
      HMAX = TINT /3600.
      TINTD = TINT/(3600.D0 * 24.D0)
      CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, AUTOW, 
     $   'IVis')
      CALL SIMUV (DBLE(TFREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), .TRUE., 1.0, 
     $   'TPVis')
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
      CALL CRDGET ('PBAtmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RVAL(5) = TFREQ
      CALL CRDPUT ('PBAtmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
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
      CALL CRDGET ('Emission', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RVAL(5) = TFREQ
      CALL CRDPUT ('Emission', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RFACT = -ABSCOEFF
      CALL ARREXP   ('Atmos', RFACT, 0.0, 'Emission')
      CALL ARRSCALE ('Emission', -1.0, 1.0, 'Emission')
      CALL ARRSCALE ('Emission', TSKY, 0.0, 'Emission')
      IF (ERROR) GOTO 990
C
C This is ASS backwards!  But it is required to always be looking
C at the SAME POINT ON THE SKY
C
      DO 1000 I = 1, NUMINT
         CALL MSGPUT ('Integration '//STRINT(I), 'I')
         VIS = 'IVis/PC1I'//STRINT(I)
         TPVIS = 'TPVis/PC1I'//STRINT(I)
         HMIN = 0.0
         HMAX = TINT /3600.
         CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HMIN), DBLE(HMAX), 
     $      DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), AUTOC, 
     $      AUTOW, VIS)
         CALL SIMUV (DBLE(TFREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
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
C Reorganize Interferometer and TP data; Write Out
C
      CALL VISREORG ('IVis', 'IVis', 1, NUMINT)
      DO 2000 I = 1, NUMINT
         VIS = 'IVis/PC1I'//STRINT(I)
         CALL DATDELET (VIS)
 2000 CONTINUE
      IF (VISFILE .NE. ' ') THEN
         CALL CRDGET ('IVis/PC1/OBS/I', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL CRDPUT ('IVis/PC1', NAX, TYPE, NAXIS, RVAL, 
     $      RPIX, DELT, ROTA)
         CALL DATPUTR ('IVis/PC1', 'REFDATE', 0.0, 1)
         CALL VISPUT ('IVis/PC1', VISFILE, 'OBS', 'I', '*', ' ')
      ENDIF
C
C Need to write something like Allan Variance for interferometer
C data!
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





