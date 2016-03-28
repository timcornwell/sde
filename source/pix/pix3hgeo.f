C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pix3hgeo.f	1.3    11/7/90
C
       SUBROUTINE PIX3HGEO (A, B, NAX, NAY, NAZ, 
     1                       NBX, NBY, NBZ, ORDER)
C++
CD Translates an input map into a (larger) output map.
C
C  A         REAL(*)  input    Name of array to copy from
C  B         REAL(*)  output   Name of array to copy to
C  NAX       INT      input    Number of pixels in A in X-dimension
C  NAY       INT      input    Number of pixels in A in Y-dimension
C  NAZ       INT      input    Number of pixels in A in Z-dimension
C  NBX       INT      input    Number of pixels in B in X-dimension
C  NBY       INT      input    Number of pixels in B in Y-dimension
C  NBZ       INT      input    Number of pixels in B in Z-dimension
C  ORDER     INT      input    Interpolation order
C Audit trail:
C      Original code modeled after AIPS routine GEOSUB in HGEOM.
C                                     R.T.Duquet   March 15 1990
C
C-- ---------------------------------------------------------------------------
#include     "stdinc.h"
C

      INTEGER           NAX, NAY, NAZ, NBX, NBY, NBZ, ORDER
      REAL              A(NAX,NAY,*), B(NBX,NBY,*)
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'PIX3HGEO')
C
C
C=============================================================================
      IF (ERROR) GO TO 999
C
       CALL MSGPUT('PIX3HGEOM has not yet been written - Sorry!','W')
       GO TO 999
C
999   CONTINUE
      END




