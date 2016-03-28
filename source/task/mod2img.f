C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mod2img.f	1.2    11/7/90
C
      SUBROUTINE sdemain
CD     
CD    Program to generate a image from a model file
C     
CH @(#)mod2img.f	1.1    9/13/90
C
CS    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcssum
C                                         R.G. Marson     Aug 29 1990
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*)	routine
      PARAMETER		(routine = 'MOD2IMG')
C     
      CHARACTER*(sysmxnam) infile, outfile
      CHARACTER*8       ctype(2)
      DOUBLE PRECISION  rval(2)
      REAL              rota(2), delt(2), rpix(2), sampling
      INTEGER           ndummy, i, iadd
      INTEGER           size(2)
C     
C==================================================================
C     
      CALL msgwelco ('I convert models to images')
      CALL usrctl
C     
      CALL usrgetl ('Debug', sysdebug, 1, ndummy)
C     
C     Get model file
C     
      CALL usrgetc ('Infile', infile, 1, ndummy)
      CALL filgetmo('Model', infile)
C
C Create an empty array for the model to fit into
C
      CALL usrgeti('Size', size, 2, ndummy)
      IF (ndummy.LT.2) THEN
         size(2) = 1
         IF (ndummy.LT.1) size(1) = 1
      END IF
      CALL usrgetr('Sampling', sampling, 1, ndummy)
      CALL datmakar('Image', 2, size, 'R', iadd)
      DO i = 1, 2
         rval(i) = 0.0
         rota(i) = 0.0
         rpix(i) = FLOAT(size(i)/2 + 1)
         delt(i) = sampling/3600.0
      END DO
      ctype(1) = 'RA---SIN'
      ctype(2) = 'DEC--SIN'
      CALL crdput('Image', 2, ctype, size, rval, rpix, delt, rota)
C
C Turn the model into an image
C
      CALL arrsetco('Image', 0.0, 0.0)
      CALL modimg('Model', 'Image')
C
C
      CALL usrgetc ('Outfile', outfile, 1, ndummy)
C
C
C And write out the file
C
      CALL filimgpu('Image', outfile, ' ')
C
 999  CONTINUE
      END
