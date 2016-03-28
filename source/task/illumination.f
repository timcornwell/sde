C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)illumination.f	1.1	 03 Aug 1995
C
      SUBROUTINE SDEMAIN
C
C Make an aperture illumination pattern
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	Aug 3 1995
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ILLUMINATION')
C
C Names of input variables
C
      REAL		POWER, DIAM, PED
      CHARACTER*(SYSMXNAM) 	INFILE, OUTFILE
      INTEGER		NDUMMY
      CHARACTER*8       TYPE(SYSMXDIM)
      REAL              DELT(SYSMXDIM), RPIX(SYSMXDIM),
     $     		ROTA(SYSMXDIM), MINRAD, MAXRAD
      DOUBLE PRECISION  FREQ, RVAL(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*1	T
      INTEGER		ADD, I, STRSEARC
C==================================================================
C
      CALL MSGWELCO ('I make aperture illumination patterns')
C
      CALL USRCTL
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Output', OUTFILE, 1, NDUMMY)
      CALL USRGETR ('Power', POWER, 1, NDUMMY)
      CALL USRGETR ('Pedastal', PED, 1, NDUMMY)
      CALL USRGETR ('Diameter', DIAM, 1, NDUMMY)
      CALL FILIMGGE ('Image', INFILE, ' ')
C
      CALL CRDGET  ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GO TO 999
      FREQ = 0.0
      DO 10 I = 1, SYSMXDIM
         IF (TYPE(I) .EQ. 'FREQ') FREQ = RVAL(I)
 10   CONTINUE
      IF (FREQ.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No frequency axis')
         GO TO 999
      END IF
C
      MINRAD = 0.0
      MAXRAD = DIAM/2.0 * FREQ / 3.0E+8
      CALL ARRSETCO ('Image',  0.0, 1.0)
      CALL ARRRDCLP ('Image', MINRAD, MAXRAD, 0.0)
      CALL DATGETAR ('Image', NAX, NAXIS, T, ADD)
      CALL ARRILLUM (MEMR(ADD), NAXIS(1), NAXIS(2), RPIX(1), RPIX(2),
     $     DELT(1), DELT(2), MAXRAD, POWER, PED)
C
C Write result 
C
      CALL FILIMGPU('Image',OUTFILE,' ')
C
 999  CONTINUE
      END
