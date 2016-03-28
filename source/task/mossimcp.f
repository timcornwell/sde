C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mossimcp.f	1.1	 3/31/93
C
      SUBROUTINE SDEMAIN
C
#define ntel 10
CD Program to simulate an existing mosaic database
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 30 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSSIMCP')
C
      CHARACTER*(SYSMXNAM) 	MOSIN, MOSOUT, IMAGE, TEL(ntel)
      REAL			NOISE(ntel), PHASE(ntel)
      INTEGER		NDUMMY, NPC, IPC, SEED, ITEL
      LOGICAL		DFT(ntel)
      CHARACTER*(SYSMXNAM)	VIS, MYTEL
C
      INTEGER		STRSEARC
      CHARACTER*6	STRINT
C==================================================================
C
      CALL MSGWELCO ('I am a copy mosaic simulator')
      CALL USRCTL
C
      CALL USRGETC ('InMos', MOSIN, 1, NDUMMY)
      CALL USRGETC ('OutMos', MOSOUT, 1, NDUMMY)
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      CALL USRGETR ('Noise', NOISE, 10, NDUMMY)
      CALL USRGETR ('Phase', PHASE, 10, NDUMMY)
      CALL USRGETL ('DFT', DFT, 10, NDUMMY)
      CALL USRGETC ('Tele', TEL, 10, NDUMMY)
C
      CALL FILIMGGE ('Model', IMAGE, ' ')
      CALL IMGCLONE ('Model', 'PBModel')
      CALL VISMOSGE ('M', MOSIN)
      IF (ERROR) GOTO 999
C
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      DO 100 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL HEDSETOB (VIS, 'Model')
         CALL DATGETC (VIS, 'TELESCOP', MYTEL, 1, NDUMMY)
         ITEL = STRSEARC(MYTEL, TEL, ntel)
         IF (DFT(ITEL)) THEN
            CALL IMGDFTPB (VIS, 'OBS/I', 'Model', 'PBModel')
         ELSE
            CALL IMGPB ('Model', 'PBModel', 'APPLY')
            CALL IMGGRIDC ('PBModel', 'PBModel', 'CORRECT')
            CALL IMGFFT ('PBModel', 'Modelvis')
            CALL VISDEGRI (VIS, 'OBS/I', 'Modelvis')
         ENDIF
C
C Add Noise?
C
         IF (NOISE(ITEL) .GT. 0.0 .OR. PHASE(ITEL) .GT. 0.0) THEN
            CALL VISCORRU (VIS, PHASE(ITEL), 0.0, 0.0, NOISE(ITEL), 
     $         SEED, 'OBS/I', 'OBS/I')
         ENDIF
 100  CONTINUE
C
      CALL DATSETTP ('M', 'VISMOSAIC')
      CALL VISMOSPU ('M', MOSOUT)
C
 999  CONTINUE
      END
