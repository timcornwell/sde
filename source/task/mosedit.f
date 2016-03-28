C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosedit.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to edit mosaics
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSEDIT')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, MODFILE, NMOSFILE,
     1			MODE, MOSS, STRM2
      REAL		TINT, UVLIMITS(2), TIME(2), THRES
      INTEGER		NDUMMY, TIMR(8)
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		NPC, IPC, NSEL
      CHARACTER*(SYSMXNAM)	TELESCOP
C==================================================================
      CALL MSGWELCO ('I edit mosaics')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Threshold', THRES, 1, NDUMMY)
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      IF (TINT.LE.0.0) TINT = 10.0
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Model', MODFILE, 1, NDUMMY)
      CALL USRGETC('NewMos', NMOSFILE, 1, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
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
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 10 IPC = 1, NPC
         WRITE (MESSAGE, 1000) IPC
 1000    FORMAT ('Pointing ',I3)
         CALL MSGPUT (MESSAGE, 'I')
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
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
         CALL VISEDIT (MOSS, 'OBS/I', 'MOD/I', 'OBS/I', THRES, ' ')
         CALL DATDELET (STRM2(MOSS, 'MOD/I'))
         IF (ERROR) GO TO 999
  10  CONTINUE
C
      IF (NMOSFILE.NE.' ') THEN
         CALL VISMOSPU ('Mos', NMOSFILE)
      END IF
C
 999  CONTINUE
      END
