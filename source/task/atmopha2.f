C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)atmopha2.f	1.1	12/10/91
C
      SUBROUTINE SDEMAIN
C
CD From a model atmosphere, determine the RMS in T, interferometer phase
C
C Audit trail:
C	Original Version
C	Added a second atmospheric layer to the simulations!
C				M.A.Holdaway	Oct 23 1991
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ATMOPHA2')
C
      CHARACTER*(SYSMXNAM)	ATMOS1, ANTFILE, TPAFILE, VISFILE, 
     $   			TPVISFIL, VIS, TPVIS, TPTELE,
     $   			AVFIL, TPAVFIL, ATMOS2
      REAL		ATAVE1, ATAVE2, REFINDEX, ABSCOEFF, 
     $   		TSKY, HEIGHT1, HEIGHT2
      REAL		ATVEL1(2), ATVEL2(2), DEC, FREQ
      REAL		AUTOW
      LOGICAL		AUTOC
C
      INTEGER		NDUMMY, NUMINT, I
      INTEGER		NAXIS(SYSMXDIM), NAX
      REAL		ROTA(SYSMXDIM), DELT(SYSMXDIM), RPIX(SYSMXDIM)
      REAL		AVE, ELMIN, TIME, HMIN, HMAX, TINT, TINTD, 
     $   		TPDIAM, RPIX2(2), SCALE1, SCALE2, RFACT
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
      CALL USRGETC ('Atmos1', ATMOS1, 1, NDUMMY)
      CALL USRGETC ('Atmos2', ATMOS2, 1, NDUMMY)
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
      CALL USRGETR ('TSKY', TSKY, 1, NDUMMY)
      CALL USRGETR ('AtVel1', ATVEL1, 2, NDUMMY)
      CALL USRGETR ('AtVel2', ATVEL2, 2, NDUMMY)
      CALL USRGETR ('AtHt1', HEIGHT1, 1, NDUMMY)
      CALL USRGETR ('AtHt2', HEIGHT2, 1, NDUMMY)
      CALL USRGETR ('AveMM1', ATAVE1, 1, NDUMMY)
      CALL USRGETR ('AveMM2', ATAVE2, 1, NDUMMY)
      CALL USRGETR ('Rpix', RPIX2, 2, NDUMMY)
C
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('Dec',  DEC,  1, NDUMMY)
      CALL USRGETR ('Tint', TINT, 1, NDUMMY)
      CALL USRGETI ('Nint', NUMINT, 1, NDUMMY)
      CALL USRGETL ('AutoC', AUTOC, 1, NDUMMY)
      CALL USRGETR ('AutoW', AUTOW, 1, NDUMMY)
C
C Fiddle with the atmosphere
C      
      CALL FILIMGGE ('Atmos1', ATMOS1, ' ')
      CALL FILIMGGE ('Atmos2', ATMOS2, ' ')
      IF (ATAVE1 .NE. 0.0) THEN
         CALL ARRSTAT  ('Atmos1', ' ')
         CALL DATGETR  ('Atmos1', 'ARRAVE', AVE, 1, NDUMMY)
         IF (AVE .NE. 0.0) THEN
            CALL ARRSCALE ('Atmos1', ATAVE1/AVE, 0.0, 'Atmos1')
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $         'Atmosphere has zero average')
            GOTO 999
         ENDIF
      ENDIF
      IF (ATAVE2 .NE. 0.0) THEN
         CALL ARRSTAT  ('Atmos2', ' ')
         CALL DATGETR  ('Atmos2', 'ARRAVE', AVE, 1, NDUMMY)
         IF (AVE .NE. 0.0) THEN
            CALL ARRSCALE ('Atmos2', ATAVE2/AVE, 0.0, 'Atmos2')
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $         'Atmosphere has zero average')
            GOTO 999
         ENDIF
      ENDIF
C 1mm H2O ==> opacity of .06
      ABSCOEFF = .06
      CALL DATPUTR  ('Atmos1', 'REFINDEX', REFINDEX, 1)
      CALL DATPUTR  ('Atmos1', 'ABSCOEFF', ABSCOEFF, 1)
      CALL DATPUTR  ('Atmos1', 'TSKY',     TSKY, 1)
      CALL DATPUTR  ('Atmos1', 'VELOCITY', ATVEL1, 2)
      CALL DATPUTR  ('Atmos1', 'HEIGHT',   HEIGHT1, 1)
      CALL DATPUTC  ('Atmos1', 'TELESCOP', TPTELE, 1)
      CALL DATPUTR  ('Atmos1', 'TELDIAM',  TPDIAM, 1)
      CALL CRDGET ('Atmos1', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RPIX(1) = RPIX2(1)
      RPIX(2) = RPIX2(2)
      RVAL(4) = FREQ
      CALL CRDPUT ('Atmos1', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GOTO 999
      ABSCOEFF = .06
      CALL DATPUTR  ('Atmos2', 'REFINDEX', REFINDEX, 1)
      CALL DATPUTR  ('Atmos2', 'ABSCOEFF', ABSCOEFF, 1)
      CALL DATPUTR  ('Atmos2', 'TSKY',     TSKY, 1)
      CALL DATPUTR  ('Atmos2', 'VELOCITY', ATVEL2, 2)
      CALL DATPUTR  ('Atmos2', 'HEIGHT',   HEIGHT2, 1)
      CALL DATPUTC  ('Atmos2', 'TELESCOP', TPTELE, 1)
      CALL DATPUTR  ('Atmos2', 'TELDIAM',  TPDIAM, 1)
      CALL CRDGET ('Atmos2', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      RPIX(1) = RPIX2(1)
      RPIX(2) = RPIX2(2)
      RVAL(4) = FREQ
      CALL CRDPUT ('Atmos2', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
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
      CALL SIMUV (DBLE(FREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
     $   DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), .TRUE., 1.0, 
     $   'TPVis')
C
C Shift the reference values of 'Atmos' to account for AZ, EL, HEIGHT
C
      CALL CRDATMSH ('IVis', 'Atmos1')
      CALL MSGPUT  ('Shifted Atmosphere 1''s Coordinates', 'D')
      CALL CRDLIST ('Atmos1')
      CALL CRDATMSH ('IVis', 'Atmos2')
      CALL MSGPUT  ('Shifted Atmosphere 2''s Coordinates', 'D')
      CALL CRDLIST ('Atmos2')
C
C Get the scaling factor for the total power beam
C
      CALL DATPUTR  ('TPVis', 'TELDIAM',  TPDIAM, 1)
      CALL IMGCLONE ('Atmos1', 'PBAtmos1')
      CALL ARRSETCO ('PBAtmos1', 0.0, 1.0)
      CALL CRDATMPO ('PBAtmos1', 'TPVis')
      CALL IMGPB    ('PBAtmos1', 'PBAtmos1', 'APPLY')
      CALL ARRSTAT  ('PBAtmos1', ' ')
      SCALE1 = 1./DATFGETR ('PBAtmos1', 'ARRSUM')
      CALL ARRSETCO ('PBAtmos1', 0.0, 0.0)
      IF (ERROR) GOTO 990
      CALL DATPUTR  ('TPVis', 'TELDIAM',  TPDIAM, 1)
      CALL IMGCLONE ('Atmos2', 'PBAtmos2')
      CALL ARRSETCO ('PBAtmos2', 0.0, 1.0)
      CALL CRDATMPO ('PBAtmos2', 'TPVis')
      CALL IMGPB    ('PBAtmos2', 'PBAtmos2', 'APPLY')
      CALL ARRSTAT  ('PBAtmos2', ' ')
      SCALE2 = 1./DATFGETR ('PBAtmos2', 'ARRSUM')
      CALL ARRSETCO ('PBAtmos2', 0.0, 0.0)
      IF (ERROR) GOTO 990
C
C Make the image (1 - EXP^{-ABSCOEFF * MMWATER}) * TSKY
C
      CALL IMGCLONE ('Atmos1', 'Emission1')
      RFACT = -ABSCOEFF
      CALL ARREXP   ('Atmos1', RFACT, 0.0, 'Emission1')
      CALL ARRSCALE ('Emission1', -1.0, 1.0, 'Emission1')
      CALL ARRSCALE ('Emission1', TSKY, 0.0, 'Emission1')
      IF (ERROR) GOTO 990
      CALL IMGCLONE ('Atmos2', 'Emission2')
      RFACT = -ABSCOEFF
      CALL ARREXP   ('Atmos2', RFACT, 0.0, 'Emission2')
      CALL ARRSCALE ('Emission2', -1.0, 1.0, 'Emission2')
      CALL ARRSCALE ('Emission2', TSKY, 0.0, 'Emission2')
      IF (ERROR) GOTO 990
C
      WRITE (MESSAGE, 742) SCALE1, SCALE2
 742  FORMAT ('Scale 1, 2 = ',2F10.5)
      CALL MSGPUT (MESSAGE, 'D')
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
         CALL SIMUV (DBLE(FREQ), 'TPAfile', DBLE(HMIN), DBLE(HMAX), 
     $      DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(TINT), .TRUE.,
     $      1.0, TPVIS)
C
C "Point Source Model", Apply the atmosphere to the Interferometer phases
C
         CALL ARRSETCO (STRM2 (VIS, 'OBS/I/VIS'), 0.0, 1.0)
         CALL CRDATMPO ('Atmos1', VIS)
         CALL CRDATMPO ('Atmos2', VIS)
         CALL VISATMOS (VIS, 'OBS/I', 'OBS/I', 0.0, 'Atmos1')
         CALL VISATMOS (VIS, 'OBS/I', 'OBS/I', 0.0, 'Atmos2')
C
C Emission from atmosphere: assume optically thin
C
         CALL ARRSETCO ('PBAtmos1', 0.0, 0.0)
         CALL ARRSETCO ('PBAtmos2', 0.0, 0.0)
         CALL DATPUTR  (TPVIS, 'TELDIAM',  TPDIAM, 1)
         CALL CRDATMPO ('Emission1', TPVIS)
         CALL CRDATMPO ('Emission2', TPVIS)
         CALL IMGDFTPB (TPVIS, 'OBS/I', 'Emission1', 'PBAtmos1')
         CALL ARRCOPY  (STRM2 (TPVIS, 'OBS/I/VIS'), 'TP1')
         CALL IMGDFTPB (TPVIS, 'OBS/I', 'Emission2', 'PBAtmos2')
         CALL ARRCOPY  (STRM2 (TPVIS, 'OBS/I/VIS'), 'TP2')
         CALL ARRLC    ('TP1', SCALE1, 'TP2', SCALE2, 
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





