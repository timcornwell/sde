C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)alvarave.f	1.2	 7/20/92
C
      SUBROUTINE SDEMAIN
C
#define nfiles 20
C
CD A program to average several Allan Variance files
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 28 1991
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TEMPLATE')
C
      CHARACTER*(SYSMXNAM)	INFILES(nfiles), OUTFILE
C
      CHARACTER*(SYSMXNAM)	NAME
      INTEGER		NDUMMY, NIN, NIN2, I
      REAL		SCALE
      CHARACTER*6	STRINT
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I average Allan variance files')
C
      CALL USRCTL
      CALL USRGETC ('Infiles', INFILES, nfiles, NIN)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL FILALVGE ('AVE', INFILES(1))
      CALL FILALVGE ('ALV1', INFILES(1))
C
C Average the RMS values, take SQRT, put into ALVARIANCE
C
      DO 100 I = 2, NIN
         IF (INFILES(I) .EQ. ' ') THEN
            NIN2 = I -1
            GOTO 200
         ENDIF
         NAME = 'ALV'//STRINT(I)
         CALL FILALVGE (NAME, INFILES(I))
         CALL ARRLC  ('AVE/ALVARIANCE', 1.0, 
     $      STRM2(NAME, 'ALVARIANCE'), 1.0, 'AVE/ALVARIANCE')
         IF (ERROR) GOTO 999
 100  CONTINUE
      NIN2 = NIN
 200  CONTINUE
C
      IF (NIN2 .NE. 0) THEN
         SCALE = 1./FLOAT(NIN2)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No Data!')
         GOTO 999
      ENDIF
      CALL ARRSCALE ('AVE/ALVARIANCE', SCALE, 0.0, 'AVE/ALVARIANCE')
C
C Get the scatter, place in RMS: \sqrt{\frac{\sum{(ALV_i/ALV_ave)^2} - N}{N-1}}
C
      CALL ARRPOWER ('AVE/ALVARIANCE', 2.0, 0.0, 'AVE/RMS')
      CALL ARRDIV ('ALV1/RMS', 'AVE/RMS', 'WORKSUM')
      DO 300 I = 2, NIN2
         NAME = 'ALV'//STRINT(I)
         CALL ARRDIV (STRM2(NAME, 'RMS'), 'AVE/RMS', 'WORK')
         CALL ARRLC    ('WORKSUM', 1.0, 'WORK', 1.0, 'WORKSUM')
         CALL ARRLIST  ('WORK', 'Work '//STRINT(I), 1)
         CALL ARRLIST  ('WORKSUM', 'WorkSum '//STRINT(I), 1)
 300  CONTINUE
      SCALE = -NIN2
      CALL ARRSCALE ('WORKSUM', 1.0, SCALE, 'AVE/RMS')
      CALL ARRLIST  ('AVE/RMS', 'Subtracted', 1)
      IF (NIN2 .GT. 1) THEN
         SCALE = 1./FLOAT(NIN2-1)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Need at least 2')
         GOTO 999
      ENDIF
      CALL ARRSCALE ('AVE/RMS', SCALE, 0.0, 'AVE/RMS')
      CALL ARRLIST ('AVE/RMS', 'Scaled', 1)
      CALL ARRPOWER ('AVE/RMS', .5, 0.0, 'AVE/RMS')
      CALL ARRLIST  ('AVE/RMS', 'SQRT', 1)
C
C Write out AVE/RMS and AVE/ALVARIANCE
C
      CALL FILALVPU ('AVE', OUTFILE)
C
 999  CONTINUE
      END
