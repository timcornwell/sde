C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tangentize.f	1.1	 1/18/96
C
      SUBROUTINE SDEMAIN
C
CD Program to make a template image with a central reference pixel
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	Jan 17 1996
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TANGENTIZE')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      INTEGER		NDUMMY
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      REAL		PIXEL(SYSMXDIM), WORLD(SYSMXDIM)
      DATA		PIXEL /7*0.0/
      DATA		WORLD /7*0.0/
C==================================================================
C
      CALL MSGWELCO ('I tangentize images')
      CALL USRCTL
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', INFILE, '*')
      CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      PIXEL(1) = NAXIS(1)/2
      PIXEL(2) = NAXIS(2)/2
      CALL CRDPTOW ('Image', PIXEL, WORLD)
      RVAL(1) = WORLD(1)
      RVAL(2) = WORLD(2)
      RPIX(1) = PIXEL(1)
      RPIX(2) = PIXEL(2)
      CALL CRDPUT ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      CALL FILIMGPU('Image',OUTFILE,' ')
C
 999  CONTINUE
      END
