C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)polcal.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program solve for polarization D terms
C
C Audit trail:
C	New Program
C				M.A. Holdaway	Sept 9 1992
C	Now we calculate Paralactic Angle from the TIME,
C	OBSRA, OBSDEC, EPOCH, DATEOBS, SLAT, SLON.
C	SLON and SLAT are supplied for the VLA in this
C	program, rather than read from the vis file.
C				M.A.Holdaway	Sept 24 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'POLCAL')
C
      CHARACTER*(SYSMXNAM)	VISFILE, IFILE, QFILE, UFILE,
     $   			DTFILE, ADTFILE, OUTFILE
      LOGICAL		ROTATED, BEAMCOR
      REAL		DTINT, IFLUX
      INTEGER		NDUMMY, REFANT
C
      REAL		AVERR, AVELL
      INTEGER		TIMR(8), NSEL
      REAL		SLON, SLAT, UVLIMITS(2), TIME(2), RLSCALE
C==================================================================
      CALL MSGWELCO ('I solve for D terms')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETR ('UVLimits', UVLIMITS, 2, NDUMMY)
      CALL USRGETL ('Rotated', ROTATED, 1, NDUMMY)
      CALL USRGETI ('Refant', REFANT, 1, NDUMMY)
      CALL USRGETR ('TINT', DTINT, 1, NDUMMY)
      CALL USRGETR ('IFlux', IFLUX, 1, NDUMMY)
      CALL USRGETC ('Imap', IFILE, 1, NDUMMY)
      CALL USRGETC ('Qmap', QFILE, 1, NDUMMY)
      CALL USRGETC ('Umap', UFILE, 1, NDUMMY)
      CALL USRGETL ('BeamCor', BEAMCOR, 1, NDUMMY)
      CALL USRGETC ('DTerms', DTFILE, 1, NDUMMY)
      CALL USRGETC ('ADTerms', ADTFILE, 1, NDUMMY)
C
      CALL VISGET ('Vis', VISFILE, '*', '*', ' ')
      CALL VISCLONE ('Vis', 'OBS', 'I', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'Q', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'U', 'MOD')
      CALL VISCLONE ('Vis', 'OBS', 'V', 'MOD')
      CALL UTLTTOD (TIMR(1), TIME(1))
      CALL UTLTTOD (TIMR(5), TIME(2))
C
C Just flag the total intensity
C
      CALL VISSEL ('Vis', 'OBS/I', TIME, UVLIMITS, NSEL)
C
C VLA site hardwired in
C
      SLON = 107.6177275
      SLAT = 34.078749167
      CALL DATPUTR ('Vis', 'SLON', SLON, 1)
      CALL DATPUTR ('Vis', 'SLAT', SLAT, 1)
C
      IF(IFILE .NE. ' ') THEN
         CALL FILIMGGE ('Imap', IFILE, ' ')
         CALL IMGTOVIS ('Vis', 'MOD/I', 'Imap', 'Modvis', 'DModel')
      ELSE
         CALL ARRSETCO ('Vis/MOD/I/VIS', 0.0, IFLUX)
      ENDIF
C
      IF (BEAMCOR) THEN
         IF (IFLUX .LE. 0.0) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Must use On-Axis Flux in IFLUX for BeamCor')
            GOTO 999
         ENDIF
         CALL VISARRLL ('Vis', 'OBS', AVERR, AVELL)
         IF (ERROR) GOTO 999
         RLSCALE = SQRT(AVERR/IFLUX) * SQRT(AVELL/IFLUX)
         WRITE (MESSAGE, 1926) AVERR, AVELL, RLSCALE
 1926    FORMAT (' AveRR = ', F8.5, ' AveLL = ', F8.5, ' RLscale = ',
     $      F8.5)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         RLSCALE = 1.0
      ENDIF
C
      IF(QFILE .NE. ' ') THEN
         CALL FILIMGGE ('Qmap', QFILE, ' ')
         CALL ARRSCALE ('Qmap', RLSCALE, 0.0, 'Qmap')
         CALL IMGTOVIS ('Vis', 'MOD/Q', 'Qmap', 'Modvis', 'DModel')
      ELSE
         CALL ARRSETCO ('Vis/MOD/Q/VIS', 0.0, 0.0)
      ENDIF
C
      IF(UFILE .NE. ' ') THEN
         CALL FILIMGGE ('Umap', UFILE, ' ')
         CALL ARRSCALE ('Umap', RLSCALE, 0.0, 'Umap')
         CALL IMGTOVIS ('Vis', 'MOD/U', 'Umap', 'Modvis', 'DModel')
      ELSE
         CALL ARRSETCO ('Vis/MOD/U/VIS', 0.0, 0.0)
      ENDIF
C
      CALL ARRSETCO ('Vis/MOD/V/VIS', 0.0, 0.0)
C
      CALL DATCREAT ('DTerms')
      CALL DATPUTR ('DTerms', 'TINT', DTINT, 1)
C
      CALL VSDSOLV ('Vis', 'DTerms', ROTATED, REFANT)
C
      IF (DTFILE .NE. ' ') THEN
         CALL VSDPUT ('DTerms', DTFILE)
      ENDIF
      IF (ADTFILE .NE. ' ') THEN
         CALL VSDPUT ('DTerms', ADTFILE)
      ENDIF
      IF (OUTFILE.NE.' ') THEN
         CALL VISPUT ('Vis', OUTFILE, 'OBS', 'IQUV', '*', ' ')
      END IF
C
 999  CONTINUE
      END

