C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)atmomake.f	1.5	9/15/94
C
      SUBROUTINE SDEMAIN
C
CD Generate a model atmosphere
C
C Recomendation: This program should be run on a hefty machine:
C Should do 1K x 1K X-->X FFT's, or worse
C
C Arguments: CALL SDEMAIN
C Audit trail:
C				M.A.Holdaway	May 7 1991
C	New Features:	Velocity depended X,Y OUTER scale sizes
C			Allows different DELT(1), DELT(2)
C			Kinked Power Law with 2 sections
C				M.A.Holdaway	May 15 1991
C	Revamped pre-averaging to properly deal with the velocity
C				M.A.Holdaway	May 29 1991
C	SEED = 4 * SEED + 1
C				M.A.Holdaway	July 1 1991
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ATMOMAKE')
C
      CHARACTER*(SYSMXNAM)	ATMOS
      INTEGER		SEED
      REAL		POWER1, POWER2, SMALL, MIDDLE, BIG,
     $   		CELLSIZE(3),AVE, DISP, VELOCITY(2), TINT
      INTEGER		IMSIZE(3)
C
      INTEGER		NDUMMY
      INTEGER		NAXIS(SYSMXDIM), NAX, ATADD
      REAL		ROTA(SYSMXDIM), DELT(SYSMXDIM), RPIX(SYSMXDIM)
      REAL		D2R, INNK, MIDK, OUTK, AVX, AVY, DISP1, 
     $   		AVE1, SCALE
      CHARACTER*(8)	TYPE(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM)
C
      DATA		NAXIS /SYSMXDIM * 1/
      DATA		BLC  /SYSMXDIM * 1/
      DATA		TRC  /SYSMXDIM * 1/
      DATA		ROTA /SYSMXDIM * 0.0/
      DATA		DELT /SYSMXDIM * 0.0/
      DATA		RPIX /SYSMXDIM * 1.0/
      DATA		RVAL /SYSMXDIM * 0.0/
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I create a model atmosphere')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETR ('Pow1', POWER1, 1, NDUMMY)
      CALL USRGETR ('Pow2', POWER2, 1, NDUMMY)
      CALL USRGETR ('Small', SMALL, 1, NDUMMY)
      CALL USRGETR ('Middle', MIDDLE, 1, NDUMMY)
      CALL USRGETR ('Big', BIG, 1, NDUMMY)
      CALL USRGETR ('Velocity', VELOCITY, 2, NDUMMY)
      CALL USRGETR ('Inttime', TINT, 1, NDUMMY)
      CALL USRGETR ('DISP', DISP, 1, NDUMMY)
      CALL USRGETR ('AVE', AVE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETC ('Atmosphere', ATMOS, 1, NDUMMY)
      SEED = 4*SEED + 1
C
C Make the atmosphere image in the U,V plane
C
      IF (IMSIZE(3) .NE. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Cannot yet deal with 3-D Atmosphere')
         GOTO 990
      ENDIF
C
      NAX = 3
      NAXIS(1) = IMSIZE(1)
      NAXIS(2) = IMSIZE(2)
      NAXIS(3) = 1
      TYPE(1)  = 'RX------'
      TYPE(2)  = 'RY------'
      TYPE(3)  = 'FREQ'
      RVAL(1)  = 0.0
      RVAL(2)  = 0.0
      RVAL(3)  = 3.E+8
      RPIX(1)  = NAXIS(1)/2
      RPIX(2)  = NAXIS(2)/2
      RPIX(3)  = 1.0
      ROTA(1)  = 0.0
      ROTA(2)  = 0.0
      ROTA(3)  = 0.0
      DELT(1)  = CELLSIZE(1)
      DELT(2)  = CELLSIZE(2)
      DELT(3)  = RVAL(3)/10.
C
      CALL DATMAKAR ('Atmos', NAX, NAXIS, 'R', ATADD)      
      CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL IMGCLONE ('Atmos', 'Atmos2')
C
      CALL ARRSETCO ('Atmos2', 0.0, AVE)
      CALL ARRGAUSS ('Atmos2', 'Atmos2', 1.0, SEED)
      D2R = ATAN2(1.0, 1.0) * 4.0/ 180.
      INNK   = 1./(BIG)
      OUTK   = 1./(SMALL )
      MIDK   = 1./(MIDDLE)
      AVX = VELOCITY(1) * TINT
      AVY = VELOCITY(2) * TINT
C
      CALL IMGPOWFA  ('Atmos2', INNK, MIDK, OUTK, POWER1, POWER2, 
     $   AVX, AVY, 'Atmos', 'Swork')
      CALL DATDELET ('Swork')
C
C Make sure the AVE, RMS af the atmosphere are what we wanted
C
      CALL ARRSTAT  ('Atmos', ' ')
      CALL DATGETR  ('Atmos', 'ARRDISP', DISP1, 1, NDUMMY)
      IF (DISP1 .NE. 0.0) THEN
         SCALE = DISP/DISP1
         WRITE (MESSAGE, 1010) SCALE
 1010    FORMAT ('Scaling by = ', E15.5)
         CALL MSGPUT (MESSAGE, 'D')
         CALL ARRSCALE ('Atmos', SCALE, 0., 'Atmos')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero RMS!')
         GOTO 990
      ENDIF
      CALL ARRSTAT  ('Atmos', ' ')
      CALL DATGETR  ('Atmos', 'ARRAVE', AVE1, 1, NDUMMY)
      SCALE = AVE - AVE1
      CALL ARRSCALE ('Atmos', 1.0, SCALE, 'Atmos')
C
C Coordinate Fiddle
C
      TYPE(3) = 'RZ------'
      RVAL(3) = 0.D0
      DELT(3) = DELT(2)
      TYPE(4) = 'TIME'
      RVAL(4) = 0.0
      DELT(4) = 0.0001157407
      TYPE(5) = 'FREQ'
      RVAL(5) = 2.3E+11
      DELT(5) = RVAL(5)/10.
      NAX     =  5
      CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL FILIMGPU ('Atmos', ATMOS, ' ')
C
 990  IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      ENDIF
 999  CONTINUE
      END

