C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)bmelong.f	1.4	 24 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to determine mean beam elongation
C
C Audit trail:
C					M.A.Holdaway  March 13 1993
C	Made ARRANPLT call consistent with ANTDEM's
C					M.A.Holdaway	March 26 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BMELONG')
C
C Variables
C
      REAL		DECS(20), HAS(20), INTTIME, ELMIN, DECLIM(2)
      REAL		BEAM(4), ELONG(20), WT(20), D2R
      INTEGER		NDECS

      REAL		SHIFT(3), CELLSIZE(3), MELONG
      INTEGER		IMSIZE(3), FOV, NANT, DIR
      LOGICAL		NICEPSF
      DOUBLE PRECISION	SITELAT, SITELONG
      REAL		SUMWT, SUMWTS, MAXPSF
      CHARACTER*1	T

      INTEGER		        NDUMMY
      LOGICAL			DATEXIST
      REAL			DEC,
     $   			HAMIN, HAMAX, FREQ,
     $				TIME, AELONG(3)
      CHARACTER*(SYSMXNAM)	ANTFILE
      REAL			XLIM(2), YLIM(2)
      DOUBLE PRECISION		EARTH(3), LOCAL(3)
      INTEGER			RXADD, RYADD, RZADD, LXADD, LYADD, LZADD
      INTEGER			I, NAX, NAXIS(SYSMXDIM)
C
      REAL			DATFGETR
C
C==================================================================
C
      CALL MSGWELCO ('I determine the mean beam elongation')
 1    CONTINUE
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETI('NDECS', NDECS, 1, NDUMMY)
      CALL USRGETR('Dlimits', DECLIM, 2, NDUMMY)
      CALL USRGETR('Decs', DECS, 20, NDUMMY)
      CALL USRGETR('HAlim', HAS, 20, NDUMMY)
      CALL USRGETR('TINT', INTTIME, 1, NDUMMY)
      CALL USRGETR('ELmin', ELMIN, 1, NDUMMY)
      CALL USRGETR('FOV', FOV, 1, NDUMMY)
      CALL USRGETL('NICEPSF', NICEPSF, 1, NDUMMY)
      CALL USRGETR('CellSize', CELLSIZE, 3, NDUMMY)
      CALL USRGETI('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR('Elong', AELONG, 3, NDUMMY)

      IF(ERROR) GO TO 990
      D2R = ATAN2(1.0, 1.0)/45.0
C
      IF (ANTFILE.NE.' ') THEN
         CALL MSGPUT ('Getting antenna file from '//ANTFILE, 'I')
         CALL FILGETAN ('Antfile', ANTFILE)
      ELSE
         CALL MSGPUT ('Need to specify Antfile', 'E')
         GO TO 1
      END IF
      IF (AELONG(1) .NE. 1.0 .OR. AELONG(2) .NE. 1.0) THEN
         CALL DATGETD ('Antfile', 'SITELAT', SITELAT, 1, NDUMMY)
         CALL DATGETD ('Antfile', 'SITELONG', SITELONG, 1, NDUMMY)
         CALL DATGETAR ('Antfile/RX', NAX, NAXIS, T, RXADD)
         CALL DATGETAR ('Antfile/RY', NAX, NAXIS, T, RYADD)
         CALL DATGETAR ('Antfile/RZ', NAX, NAXIS, T, RZADD)
         CALL DATGETAR ('Antfile/LX', NAX, NAXIS, T, LXADD)
         CALL DATGETAR ('Antfile/LY', NAX, NAXIS, T, LYADD)
         CALL DATGETAR ('Antfile/LZ', NAX, NAXIS, T, LZADD)
         NANT = NAXIS(1)
         DO 37 I = 0, NANT-1
            LOCAL(1) = MEMR(RXADD + I)
            LOCAL(2) = MEMR(RYADD + I)
            LOCAL(3) = MEMR(RZADD + I)
            LOCAL(1) = LOCAL(1) * AELONG(1)
            LOCAL(2) = LOCAL(2) * AELONG(2)
            LOCAL(3) = LOCAL(3) * AELONG(3)
            CALL UTLL2G(LOCAL, SITELONG, SITELAT, EARTH)
            MEMR(RXADD + I) = LOCAL(1) 
            MEMR(RYADD + I) = LOCAL(2)
            MEMR(RZADD + I) = LOCAL(3)
            MEMD(LXADD + I) = EARTH(1) 
            MEMD(LYADD + I) = EARTH(2)
            MEMD(LZADD + I) = EARTH(3)
 37      CONTINUE
         XLIM(1) = 0.0
         XLIM(2) = 0.0
         YLIM(1) = 0.0
         YLIM(2) = 0.0
C         CALL ARRANPLT ('Antfile', '/xw', .TRUE., XLIM, YLIM,
C     $      .TRUE., .TRUE.)
      ENDIF
C
      IF (NDECS .LT. 2) THEN
         CALL MSGPUT ('Make NDECS > 1', 'W')
         GOTO 1
      ENDIF
C
      SUMWTS = 0.0
      MELONG = 0.0
      TIME = 0.0
      SHIFT(1) = 0.0
      SHIFT(2) = 0.0
      SHIFT(3) = 0.0
      FREQ = 230E+9
      DO 1000 I = 1, NDECS
         IF(DATEXIST('Vis')) CALL DATDELET ('Vis')
         DEC = DECS(I)
         HAMIN = -ABS(HAS(I))
         HAMAX =  ABS(HAS(I))
         CALL SIMUV (DBLE(FREQ), 'Antfile', DBLE(HAMIN), DBLE(HAMAX), 
     1      DBLE(DEC), DBLE(ELMIN), DBLE(TIME), DBLE(INTTIME), 
     $      .FALSE., 1.0, 'Vis')
         CALL IMGMAKE ('Vis/OBS/I', CELLSIZE, IMSIZE, SHIFT, 
     1      'R', 'PSF')
         CALL DATPUTC ('PSF', 'FFTSIZE', 'PWR2', 1)
         IF (ERROR) GO TO 999
         IF (I .EQ. 1) THEN
            CALL MSGPUT ('Coordinates for PSF:', 'I')
            CALL CRDLIST ('PSF')
         ENDIF
         NDUMMY = 0
         CALL FFTCONJA ('PSF', 'XFR', DIR, NDUMMY)
         CALL DATPUTC ('PSF', 'CFTYPE', 'SF', 1)
         CALL DATPUTC ('XFR', 'CFTYPE', 'SF', 1)
C
C Now grid data and weights
C
         IF (NICEPSF) THEN
            CALL MSGPUT ('Doing NICE PSF weighting of data', 'I')
            CALL IMGMSPSF ('Vis', 'OBS/I', 'XFR', 'PSF')
         ELSE IF (FOV.NE.0.0) THEN
            CALL DATPUTR ('XFR', 'WTFOV', FOV, 1)
            CALL GRDUWT ('Vis', 'OBS/I', 'XFR')
            IF (ERROR) GO TO 999
C            WRITE (MESSAGE, 1100) FOV
C 1100       FORMAT ('Weighting for fraction of field of view = ',
C     1         F7.2)
C            CALL MSGPUT (MESSAGE, 'I')
         END IF
C
C Make PSF
C
         CALL VISTOIMG ('Vis', 'OBS/I', 'PSF', 'XFR', .TRUE.)
         CALL DATGETR ('XFR', 'SUMWT', SUMWT, 1, NDUMMY)
         CALL DATPUTR ('PSF', 'SUMWT', SUMWT, 1)
         WRITE (MESSAGE,1200) SUMWT
 1200    FORMAT ('Sum of weights = ',1PE15.6)
C         CALL MSGPUT (MESSAGE, 'I')
         CALL ARRSTAT ('PSF', ' ')
         CALL DATGETR ('PSF', 'ARRMAX', MAXPSF, 1, NDUMMY)
         IF (MAXPSF.EQ.0.0) THEN
            CALL ERRREPOR (ERRNTFND, ROUTINE, 'Peak of PSF zero')
            GO TO 999
         ELSE
            CALL ARRSCALE('PSF',1.0/MAXPSF,0.0,'PSF')
         END IF
C         FILENAME = 'PSF'//STRINT(I)
C         CALL FILIMGPU ('PSF', FILENAME, ' ' )
C         
         CALL IMGBMSHP( 'PSF' )
         IF (ERROR) GO TO 999
         BEAM(1) =  DATFGETR( 'PSF', 'BMAJ')*3600.0
         BEAM(2) =  DATFGETR( 'PSF', 'BMIN')*3600.0
         BEAM(3) =  DATFGETR( 'PSF', 'BPA')
         BEAM(4) =  DATFGETR( 'PSF', 'BZ')*3600.0
C
         ELONG(I) = BEAM(1) / BEAM(2)
         IF (I.EQ. 1) THEN
            WT(I) = SIN(D2R * DECLIM(1)) - 
     $         SIN(D2R * (DECS(I) + DECS(I+1))/2.0)
         ELSE IF (I .EQ. NDECS) THEN
            WT(I) = SIN(D2R * (DECS(I-1) + DECS(I))/2.0) -
     $         SIN(D2R * DECLIM(2))
         ELSE
            WT(I) = SIN(D2R * (DECS(I-1) + DECS(I))/2.0) -
     $         SIN(D2R * (DECS(I) + DECS(I+1))/2.0)
         ENDIF
         MELONG = MELONG + ELONG(I) * WT(I)
         SUMWTS = SUMWTS + WT(I)
         CALL DATDELET( 'PSF' )
         CALL DATDELET( 'XFR' )
 1000 CONTINUE
C
      MELONG = MELONG / SUMWTS
C
      WRITE (MESSAGE, 1987) MELONG
 1987 FORMAT ('Mean beam elongation = ',F10.5)
      CALL MSGPUT (MESSAGE, 'I')
      CALL DATDELET('Antfile')
C
      GOTO 1
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
