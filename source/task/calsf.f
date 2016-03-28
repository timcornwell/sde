C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)calsf.f	1.3    5/15/92
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate the structure function of antenna gains
C
C
C Audit trail:
C	Added variable number of lags
C				T.J.Cornwell	Jan 11 1989
C	Added SFTYPE
C				T.J.Cornwell	Jan 12 1989
C	Added print option
C				T.J.Cornwell	Jan 16 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CALSF')
C
      REAL		TIME(2), UVLIMITS(2), TAVG,
     2			TBEG, TEND
      INTEGER 		NDUMMY, I, DIR, NSEL, TIMR(8), NLAG
      CHARACTER*(SYSMXNAM)	VISFILE, SFTYPE
      REAL		ZFLUX
      CHARACTER*(SYSMXNAM)	NUMBFILE, GLFILE, PLFILE
      CHARACTER*6	STRINT
      CHARACTER*12	STRTIMC
C
C==================================================================
C
      CALL MSGWELCO (
     1   'I calculate the antenna gain structure function')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('Taverage', TAVG, 1, NDUMMY)
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('ZFlux', ZFLUX, 1, NDUMMY)
      CALL USRGETI ('Nlags', NLAG, 1, NDUMMY)
      CALL USRGETC ('SfType', SFTYPE, 1, NDUMMY)
      CALL USRGETC ('Outfile', GLFILE, 1, NDUMMY)
      CALL USRGETC ('Plotfile', PLFILE, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Now get relevant files
C
      CALL VISGET ('Vis', VISFILE, 'I', '*', ' ')
C
C Do initial selection
C
      CALL UTLTTOD (TIMR(1), TBEG)
      IF (TBEG.EQ.0.0) THEN
         CALL ARRSTAT ('Vis/TIME', ' ')
         CALL DATGETR ('Vis/TIME', 'ARRMIN', TBEG, 1, NDUMMY)
      END IF
      CALL UTLTTOD (TIMR(5), TEND)
      IF (TEND.EQ.0.0) THEN
         CALL ARRSTAT ('Vis/TIME', ' ')
         CALL DATGETR ('Vis/TIME', 'ARRMAX', TEND, 1, NDUMMY)
      END IF
      TIME(1) = TBEG
      TIME(2) = TEND
      CALL VISSEL ('Vis','OBS/I', TIME, UVLIMITS, NSEL)
      IF (NSEL.NE.0) THEN
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities for time: ')
         CALL STRAPPEN (MESSAGE, STRTIMC(TIME(1))//' to '//
     1      STRTIMC(TIME(2)))
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No valid data', 'W')
         GO TO 999
      END IF
C
C Fill in model data
C
      CALL VISPOINT ('Vis', 'OBS/I', 'MOD/I', ZFLUX)
C
C Now self-calibrate data
C
      CALL DATPUTR ('Vis', 'TINT', TAVG, 1)
      CALL VISSCAL ('Vis', 'OBS/I', 'MOD/I', 'OBS/I', 'AMPPHI')
C
C Find the structure function
C
      IF (SFTYPE.EQ.'AMPPHASE') THEN
         CALL MSGPUT ('Finding structure function of complex gains',
     1      'I')
      ELSE IF (SFTYPE.EQ.'PHASE') THEN
         CALL MSGPUT ('Finding structure function of phasors',
     1      'I')
      ELSE IF (SFTYPE.EQ.'AMP') THEN
         CALL MSGPUT ('Finding structure function of amplitudes',
     1      'I')
      END IF
C
C Calculate structure function: note that this destroys the antenna
C gains
C
      CALL CALSF('Vis', 'OBS/I', NLAG, SFTYPE)
C
C Write out the gains if required
C
      IF (GLFILE.NE.' ') THEN
         CALL TXTOPEN ('SFprint', GLFILE, 'WRITE')
         WRITE (MESSAGE, 1100) VISFILE
 1100    FORMAT ('Structure function for file ',A)
         CALL TXTWRITE ('SFprint', MESSAGE)
         CALL CALSFLIS ('SFprint', 'Vis', 'OBS/I')
         CALL TXTCLOSE ('SFprint')
      END IF
C
C Plot the structure function if required
C
      IF (PLFILE.NE.' ') THEN
         CALL VISANTPO ('Vis', 'OBS/I')
         CALL CALSFPLO (VISFILE, PLFILE, 'Vis', 'OBS/I')
      END IF
C
 999  CONTINUE
      END
