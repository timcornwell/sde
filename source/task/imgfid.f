C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgfid.f	1.5    7/5/95
C
      SUBROUTINE SDEMAIN
C
CD Evaluates the fidelity of a reconstructed image given the true image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 12 1991
C	Change to PLTSCATT tracked.  (In commented code, no less.)
C				D.S.Briggs	July 15, 1992
C	Deleted unused code, added MOMENT option
C				M.A. Holdaway	July 5 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGFID')
C
      CHARACTER*(SYSMXNAM) 	IMGFILE,
     1				MODFILE, MODE,
     2                          FIDFILE, DEV1, SENS
      INTEGER		NDUMMY
      REAL		AVE1, AVE2, AVERAGE, POWER
      REAL		MEDIAN, 
     $   		CLIPMAX, CLIPMIN, SMOOTH(4)
C==================================================================
C
      CALL MSGWELCO ('I find fidelity images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('Model',MODFILE, 1, NDUMMY)
      CALL USRGETC ('SENS',SENS, 1, NDUMMY)
      CALL USRGETC ('Mode', MODE, 1, NDUMMY)
      CALL USRGETR ('Power', POWER, 1, NDUMMY)
      CALL USRGETR ('Smooth', SMOOTH, 4, NDUMMY)
      CALL USRGETC ('FidIm',FIDFILE,1,NDUMMY)
      CALL USRGETR ('ClipMin', CLIPMIN, 1, NDUMMY)
      CALL USRGETR ('ClipMax', CLIPMAX, 1, NDUMMY)
      CALL USRGETC ('DevScatter', DEV1, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMGFILE, '*')
      CALL FILIMGGE ('Model', MODFILE, '*')
      IF (SENS .NE. ' ') THEN
         CALL FILIMGGE ('Sensitivity', SENS, '*')
         CALL ARRMASK  ('Sensitivity',  'Mask', 1.0, 99., 
     $      0., 1., 1.)
         CALL ARRMULT  ('Model', 'Mask', 'Model')
         CALL DATDELET ('Mask')
         CALL DATDELET ('Sensitivity')
      ENDIF
      IF (ERROR) GOTO 999
C
C Smooth and Scale MODEL as appropriate
C
      CALL IMGBMCMP ('Model', 'Image', 'Model')
      IF (SMOOTH(1) .GT. 0.0) THEN
         WRITE (MESSAGE, 9028) SMOOTH(1), SMOOTH(2)
 9028    FORMAT ('Smoothing MODEL and IMAGE with beam: ',2(F10.5,'"'))
         CALL MSGPUT (MESSAGE, 'I')
         CALL IMGCLONE ('Image', 'Mask')
         CALL ARRMASK ('Model', 'Mask', .0001, 1.E+8, 
     $      0., 1.0, 1.0)
         CALL IMGSMOOT ('Model', SMOOTH, 'Model', 'Scratch')
         CALL IMGSMOOT ('Image', SMOOTH, 'Image', 'Scratch')
         CALL ARRMULT  ('Model', 'Mask', 'Model')
         CALL ARRMULT  ('Image', 'Mask', 'Image')
         CALL DATDELET ('Mask')
      ENDIF
C
C Calculate FIDELITY IMAGE
C
      CALL ARRLC ('Image', -1.0, 'Model', 1.0, 'Diff')
      CALL ARRABS ('Diff', 'Diff')
      CALL IMGCLONE ('Image', 'Fidelity')
      CALL ARRDIV ('Image', 'Diff', 'Fidelity')
C
      IF (ERROR) GOTO 999
C
C Write result 
C
      IF (FIDFILE .NE. ' ') THEN
         CALL FILIMGPU('Fidelity', FIDFILE,' ')
      ENDIF
C
C get some stats
C 
      IF (MODE(1:6) .EQ. 'MOMENT') THEN
         CALL ARRMASK ('Model', 'Mask', CLIPMIN, 1.0E+20, 0.0, 1.0, 1.0)
         CALL ARRPOWER ('Model', POWER, 0.0, 'ModelP') 
         CALL ARRMULT ('ModelP', 'Mask', 'ModelP')
         CALL ARRMULT ( 'Fidelity', 'ModelP', 'Fidelity')
         CALL ARRSTAT ( 'Fidelity', ' ')
         CALL ARRSTAT ( 'ModelP', ' ')
         CALL DATGETR ('Fidelity', 'ARRAVE', AVE1, 1, NDUMMY)
         CALL DATGETR ('ModelP', 'ARRAVE', AVE2, 1, NDUMMY)
         AVERAGE = AVE1 / AVE2
         WRITE (MESSAGE, 110) AVERAGE
 110     FORMAT ('Momemnt Average fidelity is ', F10.2)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL ARRMEDIA ('Fidelity', CLIPMIN, CLIPMAX, MEDIAN)
         WRITE (MESSAGE, 100) MEDIAN
 100     FORMAT ('Normal Median fidelity is ', F10.2)
         CALL MSGPUT (MESSAGE, 'I')
      ENDIF
C
 999  CONTINUE
      END
