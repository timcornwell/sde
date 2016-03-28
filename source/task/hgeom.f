C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)hgeom.f	1.3	 7/20/92
C
       SUBROUTINE SDEMAIN
C
C Geometric transform of one image to geometry of the header of another.
C
C Arguments: CALL SDEMAIN
C
C Audit trail:
C      Main program to test the code modeled on AIPS routine HGEOM.
C                                     R.T.Duquet   March 15 1990
C
C-----------------------------------------------------------------------------
#include     "stdinc.h"
C
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'HGEOM')
C
      CHARACTER*(SYSMXNAM)	IN, EXAMPLE, OUT, ORDER
      REAL			PADVAL
      INTEGER			NDUMMY, IMSIZE(SYSMXDIM)
C=============================================================================
C
      CALL MSGWELCO('I convert image geometry')
      CALL USRCTL
C
C Get Images and interpolation order
C
      CALL USRGETC('Image', IN, 1, NDUMMY)
      CALL USRGETC('Example', EXAMPLE, 1, NDUMMY)
      CALL USRGETC('Output', OUT, 1, NDUMMY)
      CALL USRGETC('InterpOrder', ORDER, 1, NDUMMY)
      CALL USRGETI('Imsize', IMSIZE, SYSMXDIM, NDUMMY)
C
C Read the input and example files
C
      CALL FILIMGGE('Input', IN, ' ')
      CALL FILIMGGE('Sample', EXAMPLE, ' ')
C
C Call the routines
C
      CALL MSGPUT ('Interpolation will be '//ORDER, 'I')
      CALL MSGPUT ('Header of input image', 'I')
      CALL CRDLIST ('Input')
      IF((IMSIZE(1).GT.1).AND.(IMSIZE(2).GT.1)) THEN
         PADVAL = 0.0
         CALL IMGPAD('Sample', 'Padim', IMSIZE, PADVAL)
         CALL MSGPUT ('Header of sample image', 'I')
         CALL CRDLIST ('Padim')
         CALL IMGHGEOM('Input', 'Padim',  'Output', ORDER)
      ELSE
         CALL MSGPUT ('Header of sample image', 'I')
         CALL CRDLIST ('Sample')
         CALL IMGHGEOM('Input', 'Sample', 'Output', ORDER)
      END IF
C
C Write History Info
C
      CALL HISOPEN('Output')
      CALL HISINPUT('Output')
C
C Write result    
C
      CALL FILIMGPU('Output',OUT,' ')
C
      END







