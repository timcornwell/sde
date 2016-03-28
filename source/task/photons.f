C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)photons.f	1.4    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to add photons noise to an image
C
CS Arguments: CALL SDEMAIN
CD Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C      Added calls to treat the header a bit better
C                              R.G.MARSON      Sep 3 1990
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PHOTONS')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      CHARACTER*8       CTYPE(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL		SCALE, ARRMAX, BACK, RPIX(SYSMXDIM)
      REAL              DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER		NDUMMY, SEED, NAX, NAXIS(SYSMXDIM)
      INTEGER           ADD
C==================================================================
C
      CALL MSGWELCO ('I add photon noise to images')
      CALL USRCTL
C
C Get Image
C
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETR ('Photons', SCALE, 1, NDUMMY)
      CALL USRGETR ('Background', BACK, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL CRDGET('Image', NAX, CTYPE, NAXIS,
     $   RVAL, RPIX, DELT, ROTA)
C
C Create the output array
C
      CALL DATMAKAR('Output', NAX, NAXIS, 'R', ADD)
      CALL CRDPUT('Output', NAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Call main routine
C
      CALL ARRSTAT ('Image', ' ')
      CALL DATGETR ('Image', 'ARRMAX', ARRMAX, 1, NDUMMY)
      CALL ARRSCALE ('Image', SCALE/ARRMAX, BACK, 'Image')
      SEED = 2000001
      CALL ARRPOISS ('Image', 'Image', SEED)
C
C Fix up the header
C
      CALL HISINPUT('Output')
      CALL DATPUTC('Output', 'BUNIT', 'PHOTONS', 1)
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Image', OUTFILE, ' ')
C
  999 CONTINUE
      END
