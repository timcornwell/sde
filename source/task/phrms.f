C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)phrms.f	1.4	 11/15/91
C
      SUBROUTINE SDEMAIN
C
#define nt 10
C
CD I read in a visibility set and print out the RMS phase over
C  various time scales
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 8 1991
C	Currently disabled all but AVE options
C	Added ploting routine
C				M.A.Holdaway	Aug 7 1991
C	Added AveBL, a baseline averaging interval
C				M.A.Holdaway	Nov 15 1991
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PHRMS')
C
C Names of input variables
C
      CHARACTER*(SYSMXNAM)	VISFILE, RMSFILE, PLOTFILE
C
      INTEGER		NDUMMY, NAX, NAXIS(SYSMXDIM), NT
      INTEGER		BADD, UADD, VADD, TADD, VISADD, I
      CHARACTER*1	TYPE
      CHARACTER*(SYSMXNAM)	COMMENT
      REAL		TIMES(nt), FREQ, SCALE, AVEBL
C==================================================================
      INTEGER		DATADD
      REAL		CRDGFREQ
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I calculate phase RMS')
C
      CALL USRCTL
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('RMS', RMSFILE, 1, NDUMMY)
      CALL USRGETC ('Plot', PLOTFILE, 1, NDUMMY)
      CALL USRGETR ('Times', TIMES, nt, NT)
      CALL USRGETC ('Comment', COMMENT, 1, NDUMMY)
      CALL USRGETR ('AveBL', AVEBL, 1, NDUMMY)
C
      DO 100 I = 1, nt
         IF (TIMES(I) .LE. 0.0) THEN
            NT = I - 1
            GOTO 200
         ENDIF
 100  CONTINUE
      NT = nt
 200  CONTINUE
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
      UADD = DATADD ('Vis/UU')
      VADD = DATADD ('Vis/VV')
      TADD = DATADD ('Vis/TIME')
      BADD = DATADD ('Vis/BASELINE')
      CALL DATGETAR ('Vis/OBS/I/VIS', NAX, NAXIS, TYPE, VISADD)
C
      CALL VISPHRMS (MEMR(TADD), MEMR(BADD), MEMR(UADD), MEMR(VADD), 
     $   MEMX(VISADD), NAXIS(1), TIMES, NT, 'RMS')
C
C Convert from \lambda to meters for UV distance
C
      FREQ = CRDGFREQ ('Vis/OBS/I')
C
      IF (FREQ .NE. 0.0) THEN
         SCALE = 3.0E+8 / FREQ
         CALL ARRSCALE ('RMS/UV', SCALE, 0.0, 'RMS/UV')
      ELSE
         CALL MSGPUT ('No FREQ: (u,v) distance in lambda', 'W')
      ENDIF
      IF (RMSFILE .NE. ' ') THEN
         CALL FILRMSPU ('RMS', RMSFILE, COMMENT, AVEBL)
      ENDIF
      IF (PLOTFILE .NE. ' ') THEN
         CALL PLTPHRMS ('RMS', COMMENT, PLOTFILE)
      ENDIF
C
      END
