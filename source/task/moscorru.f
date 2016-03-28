C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moscorru.f	1.4	 12/10/91
C
C Various defines
C
#define ntel 20
C
      SUBROUTINE SDEMAIN
C
CD Corrupts MOSAIC DATA
C
C Pass seed down from top
C				Mark Holdaway	May 1 1991
C Apply a model atmosphere to the visibilities
C				Mark Holdaway	June 19 1991
C Added ADDN option
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCORRU')
C
      LOGICAL			ADDN
      CHARACTER*(SYSMXNAM)	MOSIN, MOSOUT, STOKES, SUBCLASS
      CHARACTER*(SYSMXDIM)	ATMOS
      REAL			PHRMS, NRMS, GRMS, GDRIFT, AUTOW
      REAL			REFINDEX, ATVEL(2), ATHT(2), ATAVE
      REAL			ATRPIX(2), TSKY, AVE, ABSCOEFF
      INTEGER			NPC, IPC, NDUMMY, SEED
      INTEGER			NAX, NAXIS(SYSMXDIM)
      REAL			RPIX(SYSMXDIM), DELT(SYSMXDIM),
     $   			ROTA(SYSMXDIM)
      DOUBLE PRECISION		RVAL(SYSMXDIM), FREQ
      CHARACTER*8		TYPE
      CHARACTER*6		STRINT
      CHARACTER*(SYSMXNAM)	VIS, STRRMBL, STRM2
C==================================================================
      CALL MSGWELCO ('I corrupt mosaic existing mosaic data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Mosin', MOSIN, 1, NDUMMY)
      CALL USRGETC ('Mosout', MOSOUT, 1, NDUMMY)
      CALL USRGETC ('STOKES', STOKES, 1, NDUMMY)
      CALL USRGETR ('PHRMS', PHRMS, 1, NDUMMY)
      CALL USRGETR ('NRMS', NRMS, 1, NDUMMY)
      CALL USRGETR ('GRMS', GRMS, 1, NDUMMY)
      CALL USRGETR ('GDRIFT', GDRIFT, 1, NDUMMY)
      CALL USRGETL ('AddNoise', ADDN, 1, NDUMMY)
      CALL USRGETR ('AUTOW', AUTOW, 1, NDUMMY)
      CALL USRGETC ('Atmosphere', ATMOS, 1, NDUMMY)
      CALL USRGETR ('RefractIndex', REFINDEX, 1, NDUMMY)
      CALL USRGETR ('AtVelocity', ATVEL, 2, NDUMMY)
      CALL USRGETR ('AtHeight', ATHT, 1, NDUMMY)
      CALL USRGETR ('AveMM', ATAVE, 1, NDUMMY)
      CALL USRGETR ('Rpix', ATRPIX, 2, NDUMMY)
      CALL USRGETR ('Tsky', TSKY, 1, NDUMMY)

      IF (ERROR) GO TO 999
C
C Question: Do we need the ferquency for anything?
C
      FREQ = 230E+9
      SEED = 88045
      CALL MSGPUT ('Working on Stokes type '//STOKES(1:1), 'I')
      SUBCLASS = 'OBS/'//STOKES(1:1)
C
C Now get relevant files
C
C
      NPC = 0
      CALL DATCREAT('IM')
      IF (MOSIN .NE.' ') THEN
         CALL MSGPUT ('Reading Mosaic visibility file', 'I')
         CALL VISMOSGE ('IM', MOSIN)
C
         CALL DATGETI ('IM', 'NPC', NPC, 1, NDUMMY)
         IF (NPC.EQ.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'No pointings')
            GO TO 999
         END IF
      END IF
      CALL DATPUTI ('IM', 'NPC', NPC, 1)
      IF(NPC.EQ.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'You must specify an input file via Mosaic')
         GO TO 999
      ELSE
         WRITE (MESSAGE, 1050) NPC
 1050    FORMAT ('There are ',I4,' pointings in all')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
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
C 1mm H2O ==> opacity of .06
         ABSCOEFF = .06
         CALL DATPUTR  ('Atmos', 'REFINDEX', REFINDEX, 1)
         CALL DATPUTR  ('Atmos', 'ABSCOEFF', ABSCOEFF, 1)
         CALL DATPUTR  ('Atmos', 'TSKY',     TSKY, 1)
         CALL DATPUTR  ('Atmos', 'VELOCITY', ATVEL, 2)
         CALL DATPUTR  ('Atmos', 'HEIGHT',   ATHT, 1)
C      CALL DATPUTC  ('Atmos', 'TELESCOP', TELE, 1)
C      CALL DATPUTR  ('Atmos', 'TELDIAM',  DIAM, 1)
         CALL CRDGET ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         RPIX(1) = ATRPIX(1)
         RPIX(2) = ATRPIX(2)
         RVAL(4) = FREQ
         CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         IF (ERROR) GOTO 999
C
C Shift the reference values of 'Atmos' to account for AZ, EL, HEIGHT
C
         CALL CRDATMSH ('IM/PC1', 'Atmos')
         CALL MSGPUT  ('Shifted Atmosphere''s Coordinates', 'D')
         CALL CRDLIST ('Atmos')
      ENDIF
C
C corrupt NOW
C
      GDRIFT = 0.
      DO 100 IPC = 1, NPC
         VIS = STRRMBL ('IM/PC'//STRINT(IPC))
         IF (AUTOW .GE. 0.0) THEN
            CALL VISSETAW (VIS, SUBCLASS, AUTOW)
         ENDIF
         IF (ADDN) THEN
            CALL VISCORRU (VIS, PHRMS, GRMS, GDRIFT, NRMS, 
     $         SEED, SUBCLASS, SUBCLASS)
         ENDIF
         IF (ATMOS .NE. ' ') THEN
            CALL CRDATMPO ('Atmos', VIS)
            CALL VISATMOS (VIS, 'OBS/I', 'OBS/I', 0.0, 'Atmos')
         ENDIF
 100  CONTINUE
C
C write out NOW
C
      IF (MOSOUT.NE.' ') THEN
         CALL MSGPUT ('Writing corrupted mosaic file as SDE file', 'I')
         CALL VISMOSPU ('IM', MOSOUT)
      ENDIF
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

