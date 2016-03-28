C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)viscorru.f	1.4	 4/13/95
C
      SUBROUTINE SDEMAIN
C
CD Corrupts Visibility DATA
C
C	New Program:  main reason for creation is to run ATMOSPHERIC
C	simulations quickly.
C	Ultimately, we will want the calibrator observation
C	in here as well (atmocal)
C				M.A.Holdaway	Dec 9 1991
C	Take SEED as input parameter.
C				D.S.Briggs	Oct 8 1993
C	Input AbsCoeff rather than assume a value.
C	Other cosmetic changes
C				M.A. Holdaway	Sep 15 1994
C	TYPE was not dimensioned correctly
C				M.A. Holdaway	April 13 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCORRU')
C
      CHARACTER*(SYSMXNAM)	VISIN, VISOUT, STOKES, CDUMMY
      CHARACTER*(SYSMXNAM)	ATMOS, SUBCLASS
      REAL			PHRMS, NRMS, GRMS, GDRIFT
      REAL			REFINDEX, ATVEL(2), ATHT(2), ATAVE
      REAL			ATRPIX(2), TSKY, AVE, ABSCOEFF
      INTEGER			NDUMMY, SEED
      INTEGER			NAX, NAXIS(SYSMXDIM)
      REAL			RPIX(SYSMXDIM), DELT(SYSMXDIM),
     $   			ROTA(SYSMXDIM)
      DOUBLE PRECISION		RVAL(SYSMXDIM), FREQ
      CHARACTER*8		TYPE(SYSMXDIM)
      LOGICAL			ADDN
      CHARACTER*6		STRINT
      CHARACTER*(SYSMXNAM)	VIS, STRRMBL, STRM2
C==================================================================
      CALL MSGWELCO ('I corrupt existing visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('VISin', VISIN, 1, NDUMMY)
      CALL USRGETC ('VISout', VISOUT, 1, NDUMMY)
      CALL USRGETC ('STOKES', STOKES, 1, NDUMMY)
      CALL USRGETR ('PHRMS', PHRMS, 1, NDUMMY)
      CALL USRGETR ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('GRMS', GRMS, 1, NDUMMY)
      CALL USRGETL ('AddNoise', ADDN, 1, NDUMMY)
      CALL USRGETC ('Atmosphere', ATMOS, 1, NDUMMY)
      CALL USRGETR ('RefractIndex', REFINDEX, 1, NDUMMY)
      CALL USRGETR ('AbsCoeff', ABSCOEFF, 1, NDUMMY)
      CALL USRGETR ('AtVelocity', ATVEL, 2, NDUMMY)
      CALL USRGETR ('AtHeight', ATHT, 1, NDUMMY)
      CALL USRGETR ('AveMM', ATAVE, 1, NDUMMY)
      CALL USRGETR ('Rpix', ATRPIX, 2, NDUMMY)
      CALL USRGETR ('Tsky', TSKY, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Question: Do we need the frequency for anything?
C
      SEED = (SEED - 1) / 4
      SEED = SEED*4 + 1
      FREQ = 230E+9
C
      CALL MSGPUT ('Working on Stokes type '//STOKES(1:1), 'I')
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Now get relevant files
C
      IF (VISIN .NE.' ') THEN
         CALL MSGPUT ('Reading visibility file', 'I')
         CALL VISGET ('Vis', VISIN, STOKES(1:1), '*', ' ')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Need VIS file')
         GOTO 999
      END IF
      CALL CRDLIST ('Vis/OBS/I')
C
C Do Atmnosphere Bookkeeping
C
      IF (ATMOS .NE. ' ') THEN
         CALL FILIMGGE ('Atmos', ATMOS, ' ')
         IF (ATAVE .NE. 0.0) THEN
            CALL ARRSTAT  ('Atmos', ' ')
            CALL DATGETR  ('Atmos', 'ARRAVE', AVE, 1, NDUMMY)
            IF (AVE .NE. 0.0) THEN
               CALL ARRSCALE ('Atmos', ATAVE/AVE, 0.0, 'Atmos')
            ELSE
               CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $            'Atmosphere has zero average')
               GOTO 999
            ENDIF
         ENDIF
C
         CALL DATPUTR  ('Atmos', 'REFINDEX', REFINDEX, 1)
         CALL DATPUTR  ('Atmos', 'ABSCOEFF', ABSCOEFF, 1)
         CALL DATPUTR  ('Atmos', 'TSKY',     TSKY, 1)
         CALL DATPUTR  ('Atmos', 'VELOCITY', ATVEL, 2)
         CALL DATPUTR  ('Atmos', 'HEIGHT',   ATHT, 1)
         CALL CRDGET ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         RPIX(1) = ATRPIX(1)
         RPIX(2) = ATRPIX(2)
         CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         IF (ERROR) GOTO 999
C
C Shift the reference values of 'Atmos' to account for AZ, EL, HEIGHT
C
         CALL CRDATMSH ('Vis', 'Atmos')
         CALL MSGPUT  ('Shifted Atmosphere''s Coordinates', 'D')
         CALL CRDLIST ('Atmos')
      ENDIF
C
C corrupt NOW
C
      GDRIFT = 0.0
      IF (ADDN)THEN
         SUBCLASS = 'OBS/'//STOKES(1:1)
         CALL VISCORRU ('Vis', PHRMS, GRMS, GDRIFT, NRMS, 
     $      SEED, SUBCLASS, SUBCLASS)
      ENDIF
      IF (ATMOS .NE. ' ') THEN
         CALL CRDATMPO ('Atmos', 'Vis')
         CALL VISATMOS ('Vis', 'OBS/I', 'OBS/I', 0.0, 'Atmos')
      ENDIF
C
C write out NOW
C
      IF (VISOUT.NE.' ') THEN
         CALL HISINPUT ('Vis')
         CALL MSGPUT ('Writing corrupted visibility file', 'I')
         CALL VISPUT ('Vis', VISOUT, 'OBS', STOKES(1:1), '*', ' ')
      ENDIF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

