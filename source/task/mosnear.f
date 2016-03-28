C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C     @(#)mosnear.f	1.1	 5/1/95
C
      SUBROUTINE SDEMAIN
C
CD Program to find pointings near a MASK sky region
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 30 1995
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSNEAR')
C
      CHARACTER*(SYSMXNAM)	MOSFILE, MASKFILE,
     1			MODE, MOSS, STRM2
      REAL		RADIUS(2), PCENT(SYSMXDIM),
     $     		WCENT(SYSMXDIM), DIFF, DIFF0
      INTEGER		NDUMMY
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		NPC, IPC, IPC0
      DATA		PCENT /SYSMXDIM * 0.0/
      DATA		WCENT /SYSMXDIM * 0.0/
C==================================================================
      CALL MSGWELCO ('I edit mosaics')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Mos', MOSFILE, 1, NDUMMY)
      CALL USRGETC('Mask', MASKFILE, 1, NDUMMY)
      CALL USRGETR ('Radius', RADIUS, 2, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      IF (MODE .NE. 'OUT' .AND. MODE .NE. 'out') THEN
         MODE = 'IN'
      ELSE
         MODE = 'OUT'
      ENDIF
C
C Get mosaic file
C
      CALL VISMOSGE ('Mos', MOSFILE)
C
C Get model image
C
      CALL FILIMGGE ('Mask', MASKFILE, ' ')
      CALL IMGCLONE ('Mask', 'Test')
      CALL ARRCENTR ('Mask', PCENT(1), PCENT(2))
      CALL CRDPTOW ('Mask', PCENT, WCENT)
      DIFF0 = 1.0E+10
C
      CALL DATGETI ('Mos', 'NPC', NPC, 1, NDUMMY)
      DO 10 IPC = 1, NPC
         MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
         CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
         CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
         DIFF =  (WCENT(1) - OBSRA)**2 + (WCENT(2) - OBSDEC)**2 
         IF (DIFF .LT. DIFF0) THEN
            DIFF0 = DIFF
            IPC0 = IPC
         ENDIF
         IF (ERROR) GO TO 999
  10  CONTINUE
      WRITE (MESSAGE, 1005) IPC0
 1005 FORMAT( 'Central pointing for mask is ',I5)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DOIT (IPC0, RADIUS, MODE)
      DO 20 IPC = 1, NPC
         IF (IPC .EQ. IPC0) GOTO 20
         CALL DOIT(IPC, RADIUS, MODE)
         IF (ERROR) GO TO 999
  20  CONTINUE
C
 999  CONTINUE
      END
C
C Actually figure out if a pointing is IN or OUT, print
C
      SUBROUTINE DOIT (IPC, RADIUS, MODE)
#include	"stdinc.h"
      INTEGER	IPC
      CHARACTER*(SYSMXNAM)  MODE, MOSS, STRM2
      REAL		RADIUS(2), AMAX
      INTEGER		NDUMMY
      CHARACTER*6	STRINT
      DOUBLE PRECISION	OBSRA, OBSDEC
      INTEGER		IPC
      CHARACTER*(SYSMXNAM)	TELESCOP
C---------------------------------------------------------------
      MOSS = STRM2 ('Mos', 'PC'//STRINT(IPC))
      CALL DATGETD (MOSS, 'OBSRA', OBSRA, 1, NDUMMY)
      CALL DATGETD (MOSS, 'OBSDEC', OBSDEC, 1, NDUMMY)
      CALL DATGETC (MOSS, 'TELESCOP', TELESCOP, 1, NDUMMY)
      CALL DATPUTD ('Test', 'OBSRA', OBSRA, 1)
      CALL DATPUTD ('Test', 'OBSDEC', OBSDEC, 1)
      CALL IMGPCANN ('Test', RADIUS(1), RADIUS(2))
      CALL ARRMULT ('Test', 'Mask', 'Test')
      CALL ARRSTAT ('Test', 'NoWindow')
      CALL DATGETR ('Test', 'ARRMAX', AMAX, 1, NDUMMY)
      IF (AMAX .GT. 0.0 .AND. MODE .EQ. 'IN') THEN
         WRITE (MESSAGE, 1010) IPC, TELESCOP(1:10), OBSRA, OBSDEC
 1010    FORMAT('IN Pointing ',I4,2X, A, 2F11.5)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE IF (AMAX .EQ. 0.0 .AND. MODE .EQ. 'OUT') THEN
         WRITE (MESSAGE, 1020) IPC, TELESCOP(1:10), OBSRA, OBSDEC
 1020    FORMAT('OUT Pointing ',I4,2X, A, 2F11.5)
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
      END
