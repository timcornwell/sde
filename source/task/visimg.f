C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visimg.f	1.1    12/26/91
C
      SUBROUTINE SDEMAIN
C
CD Program to grid uv data into an image
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C                               D.S.Briggs      Aug 16 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISIMG')
C
      CHARACTER*(SYSMXNAM) 	VISFILE, IMGFILE, STOKES
      REAL              VCELLSIZ(3)
      INTEGER		TIMR(8), IMSIZE(3)
C
      INTEGER           I, NDUMMY, NSEL, IMDIM
      REAL              TIME(2), UVLIMITS(2), ICELLSIZ(3), SHIFT(3),
     $                  WIDTH
      CHARACTER*(SYSMXNAM)      SUBCLASS
C
      REAL		D2R, PI
      PARAMETER		(PI = 3.141592654)
      PARAMETER		(D2R = PI/180.0)
C
      INTEGER           FFTPWR2
      LOGICAL           DATEXIST
C
      DATA              UVLIMITS/0.0, 1.E20/, SHIFT/3*0.0/
C==================================================================
      CALL MSGWELCO ('I grid visibility data sets into images')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Inputs
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL USRGETR ('Cellsize', VCELLSIZ, 3, NDUMMY)
      CALL USRGETI ('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETC ('Stokes', STOKES, 1, NDUMMY)
      CALL USRGETI ('Timerange', TIMR, 8, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
C
C Get visibilities
C
      CALL VISGET ('Vis', VISFILE, STOKES(1:1), '*', ' ')
      IF (ERROR) GO TO 999
C
C Select the appropriate chunk
C
      SUBCLASS = 'OBS/'//STOKES(1:1)
      IF(DATEXIST('Vis/TIME')) THEN
         CALL UTLTTOD (TIMR(1), TIME(1))
         CALL UTLTTOD (TIMR(5), TIME(2))
         CALL VISSEL ('Vis', SUBCLASS, TIME, UVLIMITS, NSEL)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) NSEL
 1000    FORMAT ('Selected ',I7,' visibilities')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No TIME information', 'W')
      END IF
C
C FFTCONJA will force this to a power of two, even if we don't do it
C  now.  A modified version of FFTCONJA could make this unnecessary.
C
      IMDIM = 1
      DO 100 I = 1, 3
         IF (IMSIZE(I).GT.1) THEN
            NDUMMY = FFTPWR2 (IMSIZE(I))
            IF (NDUMMY.GT.IMSIZE(I)) THEN
 1010          FORMAT ('Image size increased to',I4, ' on axis ',I1)
               WRITE (MESSAGE, 1010) NDUMMY, I
               CALL MSGPUT (MESSAGE, 'W')
               IMSIZE(I) = NDUMMY
            END IF
            IMDIM = I
         END IF
 100  CONTINUE
C
C Make an image header
C
      DO 200 I = 1, IMDIM
         WIDTH = 1000.0 * VCELLSIZ(I) * IMSIZE(I)
         ICELLSIZ(I) = 3600.0 / (D2R * WIDTH)
 200  CONTINUE
      CALL IMGMAKE ('Vis/'//SUBCLASS, ICELLSIZ, IMSIZE, SHIFT,
     $   'R', 'TempImg')
      IF (ERROR) GO TO 999
C
C Conjugate it to get what we really want
C
      CALL FFTCONJA ('TempImg', 'VisImg', NDUMMY, IMDIM)
      IF (ERROR) GO TO 999
      CALL CRDLIST ('VisImg')
C
C Grid it
C
      CALL DATPUTC ('VisImg', 'CFTYPE', 'BOX', 1)      
      CALL VISGRID ('Vis', SUBCLASS, 'VisImg', .FALSE.)
      IF (ERROR) GO TO 999
C
C Write result 
C
      CALL FILIMGPU('VisImg',IMGFILE,' ')
C
 999  CONTINUE
      END
