C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visavg.f	1.4    2/28/95
C
      SUBROUTINE SDEMAIN
C
CD Program to average visibility data and do sensible things with the weights
C
C Since we must maintain the correspondence between the header arrays
C and the visibilities, all selection is done on the basis of the
C weights in the first stokes parameter.  No flagged visibilities in the
C other stokes parameters will be used, but not all good data will
C necessarily be used.
C
C Audit trail:
C	New task
C				D.S.Briggs	May 20 1993
C	Get DEBUG status
C				D.S.Briggs	June 12 1994
C	Added successive difference noise estimation
C				D.S.Briggs	Feb 23 1995
C	Disable output is sd option is used
C				D.S.Briggs	Feb 28 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISAVG')
C
      CHARACTER*(SYSMXNAM)	VISFILE(SYSMXIMG), OUTFILE, STOKES
      REAL		UVLIMITS(2), SIGMA, SDTHRESH
      INTEGER		TIMR(8)
      LOGICAL		COPYHIS, USEWT, VBHACK, DOMEDIAN
C
      INTEGER		NDUMMY, NVIS, NSEL
      CHARACTER*(SYSMXNAM)	SUBCLASS, AVGTYPE
      REAL		TINT, TIME(2)
C
      INTEGER		ARRNPIX
C==================================================================
      CALL MSGWELCO ('I average visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, SYSMXIMG, NDUMMY)
      CALL USRGETC ('Stokes', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, STOKES)
      CALL USRGETR ('Uvlimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('Tavg', TINT, 1, NDUMMY)
      CALL USRGETL ('Median', DOMEDIAN, 1, NDUMMY)
      CALL USRGETR ('SDThresh', SDTHRESH, 1, NDUMMY)
      CALL USRGETR ('Sigma', SIGMA, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL USRGETL ('CopyHis', COPYHIS, 1, NDUMMY)
      CALL USRGETL ('UseWeights', USEWT, 1, NDUMMY)
      CALL USRGETL ('VBplt', VBHACK, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
      IF (TINT.LE.0.0) TINT = 10.0
      IF (SIGMA.LE.0.0) SIGMA = 1.0
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
      SUBCLASS = 'OBS/' // STOKES(1:1)
      IF (SDTHRESH.GT.0.0) THEN
         CALL MSGPUT ('Successive difference noise estimation: '
     $      // 'No averaging performed!','I')
 1000    FORMAT ('Will not consider pairs separated by more than ',
     $      F8.2, ' seconds')
         WRITE (MESSAGE, 1000) SDTHRESH
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         IF (DOMEDIAN) THEN
            IF (.NOT.USEWT)
     $         CALL MSGPUT ('UseWeight forced to TRUE','I')
            USEWT = .TRUE.
         END IF
         IF (USEWT) THEN
            IF (VBHACK) THEN
               AVGTYPE = 'VB-WT'
            ELSE
               AVGTYPE = 'WT'
            END IF
         ELSE
            IF (VBHACK) THEN
               AVGTYPE = 'VB-RMS'
            ELSE
               AVGTYPE = 'RMS'
            END IF
         END IF
      END IF
C
C Get the original visibility files
C
      CALL VISGET ('Vis', VISFILE(1), STOKES, '*', ' ')
C
      CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
      IF (ERROR) GO TO 999
      NVIS = ARRNPIX ('Vis/TIME')
      WRITE (MESSAGE, 1010) NSEL, NVIS
 1010 FORMAT ('Selected',I8,' of',I8,' visibilities')
      CALL MSGPUT (MESSAGE, 'I')
C
C Do the actual work
C
      IF (SDTHRESH.GT.0.0) THEN
         CALL VISSDIF ('Vis', 'OBS', STOKES, SDTHRESH, SIGMA)
         GO TO 999
      ELSE
         CALL VISAV ('Vis', 'VisAvg', 'OBS', STOKES, TINT, SIGMA,
     $      AVGTYPE, DOMEDIAN)
      END IF
C
C Put the visibility file
C
      IF (COPYHIS) CALL HISCOPY ('Vis', 'VisAvg')
      CALL HISINPUT ('VisAvg')

      IF (OUTFILE.NE.' ')
     $   CALL VISPUT ('VisAvg', OUTFILE, 'OBS', STOKES, '*', ' ')
C
 999  CONTINUE
      END
