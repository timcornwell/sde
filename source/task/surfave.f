C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)surfave.f	1.2	 12/2/91
C
      SUBROUTINE SDEMAIN
C
CD Program for averaging voltage patterns to get effective PB
C
C Audit trail:
C	First Code
C				M.A.Holdaway	May 2 1991
C	If PB exists in MOSIN, delete it
C				M.A.Holdaway	Dec 2 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SURFAVE')
C
      REAL                      TELDIAM
      CHARACTER*(SYSMXNAM)	MAPFILE, VPNAME, VPNAME2,
     $   			MOSIN, MOSOUT, PBAVE, PBRAVE
      CHARACTER*6               STRINT
      CHARACTER*(SYSMXNAM)	STRRMBL
C
      REAL		FREQ
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NANT, I, NDUMMY
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I create an average PB from many VPs')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('Map', MAPFILE, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR ('Teldiam', TELDIAM, 1, NDUMMY)
      CALL USRGETC ('VPname', VPNAME, 1, NDUMMY)
      CALL USRGETI ('NANT', NANT, 1, NDUMMY)
      CALL USRGETC ('MOSIN', MOSIN, 1, NDUMMY)
      CALL USRGETC ('MOSOUT', MOSOUT, 1, NDUMMY)
      CALL USRGETC ('PBAVE', PBAVE, 1, NDUMMY)
      CALL USRGETC ('PBRAVE', PBRAVE, 1, NDUMMY)
C
C Read in all Voltage Patterns
C
      CALL DATCREAT ('VP')
      CALL DATPUTI ('VP', 'NANT', NANT, 1)
      IF (ERROR) GOTO 999
      DO 4 I = 1, NANT
         VPNAME2 = STRRMBL (VPNAME(1:40)//STRINT(I)//'.SDE')
         CALL FILIMGGE ('VP/ANT'//STRINT(I), VPNAME2, ' ')
C
C If it croaked, try to read the next one in its place
C
         IF (ERROR) THEN
            CALL ERRCANCE
            VPNAME2 = STRRMBL (VPNAME(1:40)//STRINT(I+1)//'.SDE')
            CALL FILIMGGE ('VP/ANT'//STRINT(I), VPNAME2, ' ')
            IF (ERROR) GOTO 999
            WRITE (MESSAGE, 9192) I, I+1
 9192       FORMAT ('VP',I3,' was missing, replaced it by VP',I3)
            CALL MSGPUT (MESSAGE, 'W')
         ENDIF
         CALL DATPUTL  ('VP/ANT'//STRINT(I), 'ROTATABLE', .TRUE., 1)
 4    CONTINUE
C
C Read the model image
C
      CALL FILIMGGE ('Map', MAPFILE, ' ')
C
C Set up model for mosaic sampling
C
      CALL CRDGET ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
      NAX = 3
      CRVAL(1) = 0.D0
      CRVAL(2) = 0.D0
      CRVAL(3) = DBLE(FREQ)
      CTYPE(3) = 'FREQ'
      NAXIS(3) = 1
      CRPIX(3) = 1.
      CDELT(3) = 0.1 * FREQ
      CROTA(3) = 0.
      CALL CRDPUT ('Map', NAX, CTYPE, NAXIS, CRVAL, CRPIX, CDELT,
     $   CROTA)
C
C Make the "average" primary beam, attach to VISROOT/PB directory
C
      IF (MOSIN .NE. ' ' ) THEN
         CALL VISMOSGE ('M',  MOSIN)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No MOSIN')
         GOTO 999
      ENDIF
      CALL IMGDOUBL ('Map', 'Map2')
      CALL MSGPUT ('Making Average TP PB', 'I')
      IF (DATEXIST ('M/PB')) CALL DATDELET ('M/PB')
      CALL IMGPBAVE ('M', 'Map2', 'VP', TELDIAM)
      CALL MSGPUT ('Coordinates for Map2', 'D')
      CALL CRDLIST ('Map2')
C
      IF (MOSOUT .NE. ' ') THEN
         CALL VISMOSPU ('M',  MOSOUT)
      ENDIF
      IF (PBAVE .NE. ' ') THEN
         CALL FILIMGPU ('Map2', PBAVE, ' ')
      ENDIF
      IF (PBRAVE .NE. ' ') THEN
         CALL DATPUTC  ('Map2', 'TELESCOP', 'M', 1)
         CALL ARRSETCO ('Map2', 0.0, 1.0)
         CALL DATGETR  ('Map2', 'CRPIX', CRPIX, SYSMXDIM, NDUMMY)
         IF (ERROR) GOTO 999
         CALL IMGPB    ('Map2', 'Map2', 'APPLY')
         CALL FILIMGPU ('Map2',  PBRAVE, ' ')
      ENDIF
C
 999  CONTINUE
      END



