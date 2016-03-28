C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)chisq.f	1.2    6/5/93
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate Chi^2 of visibility file wrt model
C
C Audit trail:
C	Original version:
C				D.S.Briggs	July 6 1992
C	Changed to track VISCHISQ.  (grumble)  Added NumParams.
C				D.S.Briggs	May 15 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CHISQ')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE
      CHARACTER		STOKES*1
      REAL		SIGUW, CHISQ, CHISIG, TIME(2), UVLIM(2),
     $   		SUMWT, SUMWTS
      INTEGER		NDUMMY, NU, NP, TIMR(8), NSEL, NVIS
C
      INTEGER		ARRNPIX
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I calculate Chi Squared')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF(1:1), STOKES)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('Uvlimits', UVLIM, 2, NDUMMY)
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGUW, 1, NDUMMY)
      CALL USRGETI ('NumParams', NP, 1, NDUMMY)
      IF (SIGUW.LE.0.0) SIGUW = 1.0
      IF (ERROR) GO TO 999
C
      CALL VISGET ('Vis', VISFILE, STOKES, '*', ' ')
      IF(DATEXIST('Vis/TIME')) THEN
         NVIS = ARRNPIX ('Vis/TIME')
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis', 'OBS/'//STOKES, TIME, UVLIM, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL, NVIS
 1000    FORMAT ('Selected',I8,' of',I8,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
      CALL VISCLONE ('Vis', 'OBS', STOKES, 'MOD')
      IF (ERROR) GO TO 999
C
      CALL FILIMGGE ('Model', MODFILE, ' ')
      CALL MSGPUT ('Transforming model to visibilities','I')
      CALL IMGTOVIS ('Vis', 'MOD/'//STOKES, 'Model', 'Modvis',
     $   'DModel')
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Calculating Chi^2','I')
      CALL VISCHISQ ('Vis', 'OBS/'//STOKES, 'MOD/'//STOKES, CHISQ,
     $   SUMWT, SUMWTS, NU)
      NU = NU - NP
      CHISQ = CHISQ / (NU * SIGUW**2)
      CHISIG = SQRT(2.0/NU)
C
      CALL MSGPUT (' ','I')
      WRITE (MESSAGE, 1010) CHISQ
 1010 FORMAT ('Reduced Chi Squared = ',1PE11.4)
      CALL MSGPUT (MESSAGE,'I')
      WRITE (MESSAGE, 1015) NU
 1015 FORMAT (I7,' degrees of freedom')
      CALL MSGPUT (MESSAGE,'I')
      WRITE (MESSAGE, 1020) CHISIG
 1020 FORMAT ('Expect 1 +/- ',1PE11.4)
      CALL MSGPUT (MESSAGE,'I')
C
 999  CONTINUE
      END
