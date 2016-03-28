
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosinit.f	1.1	 9/21/94
C
      SUBROUTINE SDEMAIN
C
CD Replaces each pointing in a mosaic DB with a single VIS DB
C
C Audit trail:
C	New task
C				M.A. Holdaway   Aug 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSINIT')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, OUTFILE, MOSS, POIFILE,
     $   			STRM2, TELE, VISFILE
      DOUBLE PRECISION		OBSRA, OBSDEC
      REAL			DIAM
C
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM), CDUMMY
C
      CHARACTER*6	STRINT
      INTEGER		NDUMMY, NPC, IPC, I
      INTEGER		RAADD, DECADD, NAX, NAXIS(SYSMXDIM)
      CHARACTER*1	ATYPE
C==================================================================
      CALL MSGWELCO ('I concatenate mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC('Pointings', POIFILE, 1, NDUMMY)
      CALL USRGETC('Telescope', TELE, 1, NDUMMY)
      CALL USRGETR('Teldiam', DIAM, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
C
      IF (POIFILE .NE. ' ') THEN
         CALL FILGETN ('Points', POIFILE)
         CALL DATGETAR ('Points/RA', NAX, NAXIS, ATYPE, RAADD)
         CALL DATGETAR ('Points/DEC', NAX, NAXIS, ATYPE, DECADD)
         NPC = NAXIS(1)
         CALL DATCREAT ('Mos')
         CALL DATPUTI ('Mos', 'NPC', NPC, 1)
      ELSE
         CALL VISMOSGE ('Mos', MOSFILE)
         CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      ENDIF
C
C check to see if there is a pointing with identical parameters
C
      DO 100 IPC = 1, NPC
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
         IF (POIFILE .NE. ' ') THEN
            OBSRA = MEMR(RAADD + IPC - 1)
            OBSDEC = MEMR(DECADD + IPC - 1)
         ELSE
            CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
            CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
            CALL DATGETC (MOSS, 'TELESCOP', TELE, 1, NDUMMY)
            IF (TELE(1:4) .EQ. 'AIRY') THEN
               CALL DATGETR (MOSS, 'TELDIAM', DIAM, 1, NDUMMY)
            ENDIF
            CALL DATDELET (MOSS)
         ENDIF
C
         CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
         CALL CRDGET ('Vis/OBS/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, 
     $        ROTA)
         DO 90 I = 1, NAX
            CDUMMY = TYPE(I)
            IF (CDUMMY(1:2) .EQ. 'RA') RVAL(I) = OBSRA
            IF (CDUMMY(1:3) .EQ. 'DEC') RVAL(I) = OBSDEC
 90      CONTINUE
         CALL CRDPUT ('Vis/OBS/I', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, 
     $        ROTA)
C
         CALL DATRENAM ('Vis', MOSS)         
         CALL DATPUTD (MOSS, 'OBSRA', OBSRA, 1)
         CALL DATPUTD (MOSS, 'OBSDEC', OBSDEC, 1)
         CALL DATPUTC (MOSS, 'TELESCOP', TELE, 1)
         IF (TELE(1:4) .EQ. 'AIRY') THEN
            CALL DATPUTR (MOSS, 'TELDIAM', DIAM, 1)
         ENDIF
C
         WRITE (MESSAGE, 1006) IPC
 1006    FORMAT ('Replaced Pointing ',I3)
C
 100  CONTINUE
C
      CALL VISMOSPU ('Mos', OUTFILE)
C
 999  CONTINUE
      END
