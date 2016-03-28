C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mosobsof.f	1.3	 21 Jul 1995
C
      SUBROUTINE SDEMAIN
C
CD Aids in the scheduling of MOSSIAC OBSERVATIONS with OFFSET cards
C  This NEW AND IMPROVED mosobs requires template or mask images
C  to tell it where to point the antennas.
C
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Jan 19 1995
C	DeNAXISed IMGPOINT call
C					T.J. Cornwell   Jul 21 1995
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MOSOBSOF')
C
      CHARACTER*(SYSMXNAM)	MASKFILE, OUTFILE, STYLE, SENSFILE
      REAL			FREQ, DIAM, PDELTA
C
      INTEGER		NDUMMY
      CHARACTER*1	ATYPE
      LOGICAL		LIBERAL
      INTEGER           NAX, NAXIS(SYSMXDIM), MADD
      CHARACTER*8	TYPE
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL		WAVE
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      INTEGER		DUR
      CHARACTER*(SYSMXNAM)	SOURCE, BAND, USERDEF, EPOCH
C=======================================================================
      CALL MSGWELCO ('I aid in scheduling MOSAIC OBSERVATIONS')
C
      CALL USRCTL
C
      CALL USRGETC ('MaskFile', MASKFILE, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, 1, NDUMMY)
      CALL USRGETL ('Liberal', LIBERAL, 1, NDUMMY)
      CALL USRGETC ('Source', SOURCE, 1, NDUMMY)
      CALL USRGETC ('BAND', BAND, 1, NDUMMY)
      CALL USRGETC ('UserDef', USERDEF, 1, NDUMMY)
      CALL USRGETC ('EPOCH', EPOCH, 1, NDUMMY)
      CALL USRGETI ('DURATION', DUR, 1, NDUMMY)
      CALL USRGETC ('Style', STYLE, 1, NDUMMY)
      CALL USRGETC ('OutFile', OUTFILE,   1, NDUMMY)
      CALL USRGETC ('Sensitivity', SENSFILE,   1, NDUMMY)
C
      DIAM = 25.0
      WAVE = 3.0E+8/FREQ
      PDELTA = WAVE/(2 * DIAM) * 180.0 / 3.14159
      CALL FILIMGGE ('Mask', MASKFILE, ' ')
C
      CALL DATGETAR ('Mask', NAX, NAXIS, ATYPE, MADD)
      CALL CRDGET ('Mask', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA) 
      TYPE(3) = 'FREQ'
      RVAL(3) = FREQ
      DELT(3) = FREQ/10.0
      CALL CRDPUT ('Mask', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL DATPUTC ('Mask', 'TELESCOP', 'VLA', 1)
C
      CALL IMGCLONE ('Mask', 'Sensitivity')
      CALL IMGCLONE ('Mask', 'Flat')
      CALL IMGCLONE ('Mask', 'PB')
      CALL ARRSETCO ('Flat', 0.0, 1.0)
      CALL ARRSETCO ('Sensitivity', 0.0, 0.0)
C
      CALL IMGPOINT ('Mask', MEMR(MADD), NAXIS(1), NAXIS(2), RVAL, 
     $     RPIX, DELT, ROTA, PDELTA, LIBERAL, SOURCE, BAND, USERDEF, 
     $     EPOCH, DUR,
     $     'Sensitivity', 'Flat', 'PB',
     $     STYLE, OUTFILE)
      IF (ERROR) GOTO 990
C
      IF (OUTFILE .NE. ' ') THEN
         CALL MSGPUT ('Pointings are listed in '//OUTFILE, 'I')
      ENDIF
C
      IF (SENSFILE .NE. ' ') THEN
         CALL MSGPUT ('Sensitivity Image is in '//SENSFILE, 'I')
         CALL FILIMGPU ('Sensitivity', SENSFILE, ' ')
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
