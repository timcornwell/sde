C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgp2pb.f	1.6    8/4/94
C
      SUBROUTINE IMGP2PB (IMG, BEAM, SIMG)
C
CD Convert the units of an image to per beam
C
C	IMG	CH*(*)	input	Name of image to be scaled
C	BEAM	REAL(*)	input	Major axis of convolving function,
C				measured in asec
C	SIMG	CH*(*)	input	Name of scaled image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	1-D case added
C				D.S.Briggs	Oct 21 1992
C       Stash number of (square) pixels per beam in header
C       			D.S.Briggs	Feb 16 1993
C	Copy header when creating output
C				D.S.Briggs	Apr 19 1993
C	SIMG=' ' means just calculate points per beam and hang
C	it off the input.
C				D.S.Briggs	Aug 4 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG
      REAL		BEAM(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGP2PB')
C
      INTEGER		NDUMMY, NAX, NAXIS(SYSMXDIM), RNAX, ADD
      CHARACTER*1	ATYPE
      REAL		FACT, DELT(SYSMXDIM), BFACT
C
      INTEGER		CRDRNAX
      LOGICAL		DATEXIST
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, ADD)
      RNAX = CRDRNAX(NAX, NAXIS)
C
      FACT = SQRT(ATAN(1.0)/LOG(2.0))/3600.0
      IF (RNAX.EQ.3) THEN
         BFACT = FACT**3 * BEAM(1) * BEAM(2) * BEAM(4)
     $     / ABS(DELT(1)**2 * DELT(3))
      ELSE IF (RNAX.EQ.2) THEN
         BFACT = FACT**2 * BEAM(1) * BEAM(2) / DELT(1)**2
      ELSE IF (RNAX.EQ.1) THEN
         BFACT = FACT * BEAM(1) / ABS(DELT(1))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad number of axes')
         GO TO 999
      END IF
C
      IF (SIMG.NE.' ') THEN
         IF (.NOT.DATEXIST(SIMG)) CALL IMGCLONE (IMG, SIMG)
         CALL ARRSCALE (IMG, BFACT, 0.0, SIMG)
C
C Fill in header
C
         CALL DATPUTR (SIMG, 'BMAJ', BEAM(1)/3600.0, 1)
         CALL DATPUTR (SIMG, 'BMIN', BEAM(2)/3600.0, 1)
         CALL DATPUTR (SIMG, 'BPA',  BEAM(3), 1)
         CALL DATPUTR (SIMG, 'BZ', BEAM(4)/3600.0, 1)
         CALL DATPUTC (SIMG, 'BUNIT', 'JY/BEAM', 1)
         CALL DATPUTR (SIMG, 'BPPB', BFACT, 1)
C
      ELSE
         CALL DATPUTR (IMG, 'BPPB', BFACT, 1)
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
