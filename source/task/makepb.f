C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)makepb.f	1.5	 5/16/91
C
      SUBROUTINE SDEMAIN
C
CD Program to make primary beam images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 3 1990
C	Fudge: Often, OBSRA and OBSDEC are not near 
C	CRVAL(1) and CRVAL(2), especially in simulations.
C	Therefore, we SET OBSRA and OBSDEC = CRVALs if requested
C	in order to get a nice PB.
C				M.A.Holdaway	Dec 7 1990
C	TYPE was not dimensioned correctly
C				M.A.Holdaway	May 16 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAKEPB')
C
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE, MODE
      CHARACTER*(SYSMXNAM) 	TELESCOP
      INTEGER		NDUMMY, IMSIZE(3)
      REAL		CELLSIZE(3), TELDIAM, PBLEVEL
      DOUBLE PRECISION	NEWFREQ
C
      CHARACTER*8	TYPE(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM),
     $   		ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      LOGICAL		FIXPOS
      DATA		IMSIZE		/1, 1, 1/
      DATA		CELLSIZE	/1., 1., 1./
C==================================================================
C
      CALL MSGWELCO ('I make primary beams')
      CALL USRCTL
C
C Get Image
C
      MODE = 'APPLY'
      CALL USRGETC ('Telescop', TELESCOP, 1, NDUMMY)
      CALL USRGETR ('Teldiam', TELDIAM, 1, NDUMMY)
      CALL USRGETR ('PBLEVEL', PBLEVEL, 1, NDUMMY)
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETL ('FixPosition', FIXPOS, 1, NDUMMY)
      CALL USRGETD ('Frequency', NEWFREQ, 1, NDUMMY)
      IF (INFILE .NE. ' ') THEN
         CALL FILIMGGE ('Image', INFILE, ' ')
      ELSE
         CALL ERRREPOR  (ERRBDARG, ROUTINE,
     $      'Must give an actual image at this time')
         GOTO 999
      ENDIF
      IF (PBLEVEL .GT. .001) THEN
         CALL DATPUTR ('Image', 'PBLEVEL', PBLEVEL, 1)
      ENDIF
      IF(TELESCOP.NE.' ') THEN
         CALL DATPUTC ('Image', 'TELESCOP', TELESCOP, 1)
         CALL DATPUTR ('Image', 'TELDIAM', TELDIAM, 1)
      END IF
C
C Fix Coordinates
C
      IF (FIXPOS) THEN
         CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, 
     $      DELT, ROTA)
         CALL DATPUTD ('Image', 'OBSRA', RVAL(1), 1)
         CALL DATPUTD ('Image', 'OBSDEC', RVAL(2), 1)
      ENDIF
C
C Change Frequency
C
      IF (NEWFREQ .GT. 0.D0) THEN
         CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, 
     $      DELT, ROTA)
         RVAL(3) = NEWFREQ
         CALL CRDPUT ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, 
     $      DELT, ROTA)
      ENDIF
C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates for input Image:', 'I')
      CALL CRDLIST ('Image')
C
C Do the work
C
      CALL ARRSETCO ('Image', 0., 1.)
      CALL IMGPB ('Image', 'Image', MODE)
C
C Write result 
C
      CALL USRGETC('Output', OUTFILE, 1, NDUMMY)
      CALL FILIMGPU('Image', OUTFILE, ' ')
C
 999  CONTINUE
      END
