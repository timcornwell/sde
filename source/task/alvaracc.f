C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)alvaracc.f	1.1    7/19/91
C
      SUBROUTINE SDEMAIN
C
#define nfiles 20
C
CD Accumulate single Allan Variance files into one large horizontal file
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Allan Variance is my middle name	
C				M.A.Holdaway	July 10 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ALVARACC')
C
      CHARACTER*(SYSMXNAM) 	INFILE(nfiles), OUTFILE
      INTEGER		NDUMMY, I, NF
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I accumulate allan variance files')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETC ('Infiles', INFILE, nfiles, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      DO 100 I = 1, nfiles
         IF (INFILE(I) .EQ. ' ') THEN
            NF = I - 1
            GOTO 200
         ENDIF
 100  CONTINUE
      NF = nfiles
 200  CONTINUE
C
      DO 300 I = 1, NF
         CALL FILALVGE ('Allan', INFILE(I))
         CALL FILAV2PU ('Allan', OUTFILE)
         CALL DATDELET ('Allan')
 300  CONTINUE
C
      END
