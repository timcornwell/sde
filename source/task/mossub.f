C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mossub.f	1.1	 4/6/93
C
      SUBROUTINE SDEMAIN
C
#define ntel 10
CD Subtract a model from all pointings in a mosaic database
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 1 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSSUB')
C
      CHARACTER*(SYSMXNAM) 	MOSIN, MOSOUT, IMAGE, STOKES
      INTEGER		NDUMMY, NPC, IPC
      CHARACTER*(SYSMXNAM)	DFT(ntel)
      CHARACTER*(SYSMXNAM)	VIS, MYTEL
      CHARACTER*(SYSMXNAM)	VISMOD, VISDAT
      CHARACTER*1		STOKES1
      LOGICAL		DODFT
C
      INTEGER		STRSEARC
      CHARACTER*(SYSMXNAM)	STRM3, STRM2
      CHARACTER*6	STRINT
C==================================================================
C
      CALL MSGWELCO ('I subtract a model from a mosaic database')
      CALL USRCTL
C
      CALL USRGETC ('InMos', MOSIN, 1, NDUMMY)
      CALL USRGETC ('OutMos', MOSOUT, 1, NDUMMY)
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETC ('DFT', DFT, 10, NDUMMY)
C
      CALL FILIMGGE ('Model', IMAGE, ' ')
      CALL IMGCLONE ('Model', 'PBModel')
      CALL VISMOSGE ('M', MOSIN)
      IF (ERROR) GOTO 999
C
      STOKES1 = STOKES(1:1)
      CALL MSGPUT ('Working on STOKES '//STOKES1, 'I')
      CALL DATGETI ('M', 'NPC', NPC, 1, NDUMMY)
      DO 100 IPC = 1, NPC
         VIS = 'M/PC'//STRINT(IPC)
         CALL MSGPUT ('Subtracting MODEL from '//VIS, 'I')
         CALL HEDSETOB (VIS, 'Model')
         CALL DATGETC (VIS, 'TELESCOP', MYTEL, 1, NDUMMY)
         CALL VISCLONE (VIS, 'OBS', STOKES1, 'MOD')
         DODFT = STRSEARC(MYTEL, DFT, ntel) .GT. 0
         IF (DODFT) THEN
            CALL IMGDFTPB (VIS, 'MOD/'//STOKES1, 'Model', 'PBModel')
         ELSE
            CALL IMGPB ('Model', 'PBModel', 'APPLY')
            CALL IMGGRIDC ('PBModel', 'PBModel', 'CORRECT')
            CALL IMGFFT ('PBModel', 'Modelvis')
            CALL VISDEGRI (VIS, 'MOD/'//STOKES1, 'Modelvis')
         ENDIF
         VISDAT = STRM3(VIS, 'OBS/'//STOKES1, 'VIS')
         VISMOD = STRM3(VIS, 'MOD/'//STOKES1, 'VIS')
         CALL ARRLC (VISDAT, 1.0, VISMOD, -1.0, VISDAT)
         CALL DATDELET (STRM2( VIS, 'MOD'))
         IF (ERROR) GOTO 999
 100  CONTINUE
C
      CALL VISMOSPU ('M', MOSOUT)
C
 999  CONTINUE
      END
