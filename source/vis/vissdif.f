C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissdif.f	1.2    3/3/95
C
       SUBROUTINE VISSDIF (INNAME, CLASS, STOKES, SDTHRESH, SIGMAUW)

C
CD Estimate thermal noise in a visibility database by taking successive
CD differences
C
C	INNAME	CH*(*)	input	Name of input directory entry
C	CLASS	CH*(*)	input	Class, eg 'OBS'
C	STOKES	CH*(*)	input	Stokes parameters to be considered
C	SDTHRESH REAL	input	Time threshold in seconds
C	SIGMAUW	REAL	input	Sigma for unit weight
C
C If V1 & V2 are the real or imaginary parts of two visibility samples
C close enough in time that the signal in them can be considered constant,
C then (V1 - V2) / SQRT(1/W1 + 1/W2) is a Gaussian random variable with
C standard deviation SIGMAUW and zero mean.
C
C Only successive samples on a given baselines are considered, and then
C only if their time difference is less than SDTHRESH
C
C Audit trail:
C	Original version
C				D.S.Briggs	Feb 23 1995
C	Added a lower limit error estimate
C				D.S.Briggs	March 3 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	INNAME, CLASS, STOKES
      REAL		SDTHRESH, SIGMAUW
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSDIF')
C
      REAL		SDTD
      DOUBLE PRECISION	SX2, TOTSX2
      CHARACTER*(SYSMXNAM)	NSUBSIN
      INTEGER		IS, NIN, NANT, TADDI, BADDI,
     $   		VISADDI, WTADDI, NDIF, TOTNDIF
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      INTEGER		ARRNPIX, DATADD, STRLEN
C=======================================================================
      IF (ERROR) GO TO 999
C
      TADDI = DATADD(STRM2(INNAME,'TIME'))
      BADDI = DATADD(STRM2(INNAME,'BASELINE'))
      NIN = ARRNPIX(STRM2(INNAME,'TIME'))
      SDTD = SDTHRESH / (24.0 * 3600.0)
C
      TOTSX2 = 0.D0
      TOTNDIF = 0
C
      CALL VISNANT (INNAME, NANT)
C
C Loop over all stokes parameters
C
      DO 20 IS = 1, STRLEN(STOKES)
C
         IF (STRLEN(STOKES).GT.1) THEN
            MESSAGE = 'Processing Stokes ' // STOKES(IS:IS)
            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
         NSUBSIN = STRM3 (INNAME, CLASS, STOKES(IS:IS))
         VISADDI = DATADD(STRM2(NSUBSIN, 'VIS'))
         WTADDI = DATADD(STRM2(NSUBSIN, 'WT'))
C
         CALL VISSDIFP (MEMX(VISADDI), MEMR(WTADDI), MEMR(TADDI),
     $      MEMR(BADDI), NIN, NANT, SDTD, NDIF, SX2)
         
C
         TOTSX2 = TOTSX2 + SX2
         TOTNDIF = TOTNDIF + NDIF
C
         WRITE (MESSAGE, 1000) NDIF
 1000    FORMAT (I8,' complex differences used')
         CALL MSGPUT (MESSAGE, 'I')
C
 20   CONTINUE
C
      SIGMAUW = SQRT(SX2/(TOTNDIF*2))
C
      IF (STRLEN(STOKES).GT.1) THEN
         WRITE (MESSAGE, 1005) TOTNDIF
 1005    FORMAT (I8,' total complex differences used')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
      CALL MSGPUT (' ','I')
      WRITE (MESSAGE, 1010) SIGMAUW
 1010 FORMAT ('Measured sigma for unit weight is',1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1020) SIGMAUW / SQRT(2. * 2*TOTNDIF)
 1020 FORMAT ('Standard error of this is at least ',1PE12.4)
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
