C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moscenter.f	1.1	 6/5/95
C
      SUBROUTINE SDEMAIN
C
CD Program to "center" a mosaic database
C
C Audit trail:
C				M.A. Holdaway	June 5 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCENTER')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, OUTFILE, MOSS, STRM2
      CHARACTER*(SYSMXNAM)	TELESCOP, CTELESCO, TELEMIN
C
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC, RAMIN, RAMAX, DECMIN, DECMAX
      DOUBLE PRECISION	DIST, DISTMIN, RA0, DEC0
      INTEGER		NDUMMY, NPC, IPC, CPOINT
C==================================================================
      CALL MSGWELCO ('I center mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETC('Telescope', CTELESCO, 1, NDUMMY)
      CALL USRGETI('Pointing', CPOINT, 1, NDUMMY)
C
      CALL VISMOSGE ('Mos', MOSFILE)
C
      IF (CPOINT .LE. 0) THEN
         CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
C
C Find the extreme RA, DEC pointings in the database
C
         RAMAX = -999999.
         RAMIN =  999999.
         DECMAX = -999999.
         DECMIN =  999999.
C
         DO 10 IPC = 1, NPC
            MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
            CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
            CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
            CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
C
C  If inputs specify that the center must be a particular TELESCOP,
C  and this one is NOT the particular TELSCOP, skip on
C
            IF (CTELESCO .NE. ' ' .AND. TELESCOP .NE. CTELESCO) GOTO 10
C
            RAMAX = MAX (RAMAX, OBSRA)
            RAMIN = MIN (RAMIN, OBSRA)
            DECMAX = MAX (DECMAX, OBSDEC)
            DECMIN = MIN (DECMIN, OBSDEC)
 10      CONTINUE
         RA0 = (RAMAX + RAMIN) / 2.0
         DEC0 = ( DECMAX + DECMIN) / 2.0
C
C  Find pointing which is closest to center
C
         DISTMIN = 999999.0
         DO 20 IPC = 1, NPC
            MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
            CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
            CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
            CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
C
C  If inputs specify that the center must be a particular TELESCOP,
C  and this one is NOT the particular TELSCOP, skip on
C
            IF (CTELESCO .NE. ' ' .AND. TELESCOP .NE. CTELESCO) 
     $           GOTO 20
            DIST = SQRT( (OBSRA-RA0)**2 + (OBSDEC-DEC0)**2 )
            IF (DIST .LT. DISTMIN) THEN
               DISTMIN = DIST
               CPOINT = IPC
               TELEMIN = TELESCOP
            ENDIF
 20      CONTINUE
      ENDIF
C
C Now reshuffle the mosaic database to put pointing CPOINT in the
C first position
C
      IF (CPOINT .GT. 1) THEN
         WRITE (MESSAGE, 1010) CPOINT, TELEMIN(1:6)
 1010    FORMAT ('Moving pointing PC',I4,' TELE ',A6,' to PC1')
         CALL MSGPUT (MESSAGE, 'I')
         MOSS = STRM2 ('Mos', 'PC'//STRINT(CPOINT))
         CALL DATRENAM (MOSS, 'Mos/PC0')
         CALL DATRENAM ('Mos/PC1', MOSS)
         CALL DATRENAM ('Mos/PC0', 'Mos/PC1')
      ELSE
         CALL MSGPUT ('Center Pointing is already PC1: '//
     $        'new MDB same as old', 'I')
      ENDIF
C
      CALL VISMOSPU ('Mos', OUTFILE)
C
 999  CONTINUE
      END
