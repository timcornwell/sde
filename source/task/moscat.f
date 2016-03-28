
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)moscat.f	1.5	 3/21/94
C
      SUBROUTINE SDEMAIN
C
CD Program to concatenate mosaic data
C
C Audit trail:
C	New task
C				T.J.Cornwell	April 18 1990
C	If TELESCOP and OBSRA, OBSDEC are equal, instead of making a
C	new directory, we put the visibilities into the existing dir
C	IFF APPEND is set to TRUE.
C				M.A.Holdaway	March 20 1991
C	FIXED ABS(tolerance), added a few MSGPUTS
C				M.A.HOLDAWAY	May 11, 1992
C	Changed APPEND to MODE: added REPLACE mode, in which
C	the second VIS will overwrite the first
C				M.A. Holdaway   March 21 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSCAT')
C
      CHARACTER*(SYSMXNAM)	MOSFILE1, MOSFILE2, OUTFILE, MOSS1,
     $   			MOSS2, STRM2, TELE1, TELE2
      DOUBLE PRECISION		OBSRA1, OBSRA2, OBSDEC1, OBSDEC2,
     $     			DIAM1, DIAM2
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	MODE
      REAL		TOLAS, TOLRA, TOLDEC, DELTRA, DELTDEC
      INTEGER		NDUMMY, NPC1, NPC2, IPC, JPC, KPC
C==================================================================
      CALL MSGWELCO ('I concatenate mosaic visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Mos1', MOSFILE1, 1, NDUMMY)
      CALL USRGETC('Mos2', MOSFILE2, 1, NDUMMY)
      CALL USRGETC('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETC('Mode', MODE, 1, NDUMMY)
      CALL USRGETR ('Tolerance', TOLAS, 1, NDUMMY)
C
      CALL VISMOSGE ('Mos1', MOSFILE1)
      CALL VISMOSGE ('Mos2', MOSFILE2)
C
      CALL DATGETI ('Mos1', 'NPC', NPC1, 1, NDUMMY)
      CALL DATGETI ('Mos2', 'NPC', NPC2, 1, NDUMMY)
C
      CALL DATGETD ('Mos1/PC1', 'OBSDEC', OBSDEC1, 1, NDUMMY)
      TOLDEC = ABS (TOLAS /3600.)
      TOLRA  = ABS (TOLAS /3600. /COS(OBSDEC1))
      KPC = 0
C
C Take care of "APPEND" and "REPLACE" options
C
      IF (MODE(1:1) .EQ. 'A' .OR. MODE(1:1) .EQ. 'R') THEN
C
C check to see if there is a pointing with identical parameters
C
         DO 100 IPC = 1, NPC2
            MOSS2 = STRM2 ('Mos2', 'PC'//STRINT(IPC))
            CALL DATGETD (MOSS2, 'OBSRA', OBSRA2, 1, NDUMMY)
            CALL DATGETD (MOSS2, 'OBSDEC', OBSDEC2, 1, NDUMMY)
            DO 50 JPC = 1, NPC1
               MOSS1 = STRM2 ('Mos1', 'PC'//STRINT(JPC))
               CALL DATGETD (MOSS1, 'OBSRA', OBSRA1, 1, NDUMMY)
               CALL DATGETD (MOSS1, 'OBSDEC', OBSDEC1, 1, NDUMMY)
               DELTDEC = ABS(OBSDEC2 - OBSDEC1)
               DELTRA = ABS(OBSRA2 - OBSRA1)
               IF (DELTDEC .LE. TOLDEC .AND. 
     $             DELTRA  .LE. TOLRA) THEN
                  DIAM1 = 0.0
                  DIAM2 = 0.0
                  CALL DATGETC (MOSS1, 'TELESCOP', TELE1, 1, NDUMMY)
                  IF (TELE1(1:4) .EQ. 'AIRY') THEN
                     CALL DATGETD (MOSS1, 'TELDIAM', DIAM1, 1, NDUMMY)
                  ENDIF
                  CALL DATGETC (MOSS2, 'TELESCOP', TELE2, 1, NDUMMY)
                  IF (TELE2(1:4) .EQ. 'AIRY') THEN
                     CALL DATGETD (MOSS2, 'TELDIAM', DIAM2, 1, NDUMMY)
                  ENDIF
                  IF (TELE1 .EQ. TELE2 .AND. DIAM1 .EQ. DIAM2) THEN
                     IF (MODE(1:1) .EQ. 'A') THEN
                        CALL VISCAT (MOSS1, MOSS2, 'Temp', 'OBS', 'I')
                        CALL DATDELET (MOSS1)
                        CALL DATRENAM ('Temp', MOSS1)
                        CALL DATDELET ('Temp')
                        WRITE (MESSAGE, 1005) IPC, JPC
 1005                   FORMAT ('Mos2:Pointing ',I3,
     $                     ' appended to Mos1:ointing ',I3)
                        CALL MSGPUT (MESSAGE, 'I')
                     ELSE IF (MODE(1:1) .EQ. 'R') THEN
                        CALL DATDELET (MOSS1)
                        CALL DATRENAM (MOSS2, MOSS1)
                        WRITE (MESSAGE, 1006) IPC, JPC
 1006                   FORMAT ('Mos2:Pointing ',I3,
     $                     ' replaces Mos1:Pointing ',I3)
                        CALL MSGPUT (MESSAGE, 'I')
                     ENDIF
                     GOTO 80
                  ENDIF
               ENDIF
 50         CONTINUE
C
C Else if parameters of pointings are different
C
            KPC = KPC + 1
            WRITE (MESSAGE, 1000) IPC, NPC1+KPC
 1000       FORMAT ('Pointing ',I3,' becomes pointing ',I3)
            CALL MSGPUT (MESSAGE, 'I')
            MOSS1 = STRM2 ('Mos1', 'PC'//STRINT(NPC1+KPC))
            MOSS2 = STRM2 ('Mos2', 'PC'//STRINT(IPC))
            CALL DATRENAM (MOSS2, MOSS1)
 80         CONTINUE
 100     CONTINUE
         CALL DATPUTI ('Mos1', 'NPC', (NPC1+KPC), 1)
      ELSE
C
C put each pointing from each database into a seperate directory,
C even if they have the same pointing
C
         DO 210 IPC = 1, NPC2
            WRITE (MESSAGE, 1020) IPC, NPC1+IPC
 1020       FORMAT ('Pointing ',I3,' becomes pointing ',I3)
            MOSS1 = STRM2 ('Mos1', 'PC'//STRINT(NPC1+IPC))
            MOSS2 = STRM2 ('Mos2', 'PC'//STRINT(IPC))
            CALL DATRENAM (MOSS2, MOSS1)
 210     CONTINUE
         CALL DATPUTI ('Mos1', 'NPC', (NPC1+NPC2), 1)
      ENDIF
C
      CALL VISMOSPU ('Mos1', OUTFILE)
C
 999  CONTINUE
      END
