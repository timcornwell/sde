C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmodel.f	1.6    12/3/94
C
      SUBROUTINE SDEMAIN
C
CD Program to insert a model into an image. The model is read from an
C external ASCII file.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Shift, RA, Dec inputs added
C				D.S.Briggs	May 6 1993
C	Write history information to output
C				D.S.Briggs	March 16 1994
C	Pass TimeOffset to MODIMG
C				D.S.Briggs	Nov 20 1994
C	Added analytic smoothing to model
C				D.S.Briggs	Dec 2 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMODEL')
C
      CHARACTER*(SYSMXNAM)	MODFILE, NIMGFILE, IMGFILE, BEAMFILE,
     $   		NORM, FITALG
      REAL		SHIFT(2), RRA(3), RDEC(3), TIMEOFF
C
      INTEGER		NDUMMY, I, RAADD, DECADD, NCOMP
      DOUBLE PRECISION	DRA, DDEC, WORLD(SYSMXDIM)
      REAL		PIXEL(SYSMXDIM), RPIX(SYSMXDIM), DELT(SYSMXDIM),
     $   		BEAM(4)
      CHARACTER*15	ARA, ADEC
C
      INTEGER		DATADD, ARRNPIX, STRLEN
C==================================================================
C
      CALL MSGWELCO ('I insert a model into an image')
      CALL USRCTL
C
C Get information from user
C
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL USRGETR ('TimeOffset', TIMEOFF, 1, NDUMMY)
      CALL USRGETC ('NImage', NIMGFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETR ('RA', RRA, 3, NDUMMY)
      CALL USRGETR ('Dec', RDEC, 3, NDUMMY)
      CALL USRGETR ('Shift', SHIFT, 2, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 4, NDUMMY)
      CALL USRGETC ('BeamFile', BEAMFILE, 1, NDUMMY)
      CALL USRGETC ('FitAlg', FITALG, 1, NDUMMY)
      CALL USRGETC ('Normalize', NORM, 1, NDUMMY)
C
C Now get relevant files
C
      CALL FILIMGGE ('Image', IMGFILE, ' ')
      CALL FILGETMO ('Model', MODFILE)
C
      CALL ARRSETCO('Image', 0.0, 0.0)
C
      CALL MSGPUT ('Listing of model', 'I')
      CALL MODLIST ('Model')
C
C Deal with smoothing, if requested
C
      IF ((BEAM(1).NE.0.0).OR.(BEAM(2).NE.0.0).OR.
     $   (BEAMFILE.NE.' ')) THEN
         CALL FILBEMGE (BEAMFILE, BEAM, FITALG, 'TAPER')
         CALL MODSMO ('Model', BEAM, 'Model')
         CALL MSGPUT ('Smoothed model:','I')
         CALL MODLIST ('Model')
      END IF
C
      CALL DATGETR ('Image', 'CRPIX', RPIX, SYSMXDIM, NDUMMY)
      CALL DATGETR ('Image', 'CDELT', DELT, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
C
C Deal with absolute center for model
C
      IF ((RRA(1).NE.0.0).OR.(RRA(2).NE.0.0).OR.(RRA(3).NE.0.0).OR.
     $    (RDEC(1).NE.0.0).OR.(RDEC(2).NE.0.0).OR.(RDEC(3).NE.0.0)) THEN
         CALL CRDRD2D (RRA, RDEC, DRA, DDEC)
         WORLD(1) = DRA
         WORLD(2) = DDEC
         CALL CRDDWTOP ('Image', WORLD, PIXEL)
         SHIFT(1) = SHIFT(1) + (PIXEL(1) - RPIX(1)) * DELT(1) * 3600.D0
         SHIFT(2) = SHIFT(2) + (PIXEL(2) - RPIX(2)) * DELT(2) * 3600.D0
         IF (ERROR) GO TO 990
      END IF
C
C Apply any shifts, explicit or calculated to model
C
      IF ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)) THEN
         NCOMP = ARRNPIX ('Model/RA')
         RAADD = DATADD ('Model/RA')
         DECADD = DATADD ('Model/DEC')
         DO 100 I = 1, NCOMP
            MEMR(RAADD+I-1) = MEMR(RAADD+I-1) + SHIFT(1)
            MEMR(DECADD+I-1) = MEMR(DECADD+I-1) + SHIFT(2)
 100     CONTINUE
C
         IF ((ABS(SHIFT(1)).GT.1.E-4).OR.(ABS(SHIFT(2)).GT.1.E-4)) THEN
            PIXEL(1) = RPIX(1) + SHIFT(1) / (DELT(1) * 3600.D0)
            PIXEL(2) = RPIX(2) + SHIFT(2) / (DELT(2) * 3600.D0)
            CALL CRDDPTOW ('Image', PIXEL, WORLD)
            CALL CRDD2RD (WORLD(1), WORLD(2), RRA, RDEC, ARA, ADEC)
            WRITE (MESSAGE, 1010) ARA(1:STRLEN(ARA)),
     $         ADEC(1:STRLEN(ADEC))
 1010       FORMAT ('Model center is RA = ',A,'  Dec = ',A)
            CALL MSGPUT (MESSAGE,'I')
            WRITE (MESSAGE, 1000) SHIFT(1), SHIFT(2)
 1000       FORMAT ('Effective shift = ',2F10.4)
            CALL MSGPUT (MESSAGE,'I')
            CALL MSGPUT ('Shifted model:','I')
            CALL MODLIST ('Model')
         END IF
         IF (ERROR) GO TO 990
      END IF
C
C Hang the reference time off the model
C
      CALL DATPUTR ('Model','TIMEOFFSET', TIMEOFF/24., 1)
C
C Make model Image
C
      CALL MODIMG ('Model', 'Image')
      IF (ERROR) GO TO 990
C
C Normalize the smoothing, if needed
C
      IF ((BEAM(1).NE.0.0).OR.(BEAM(2).NE.0.0)) THEN
         IF (NORM(1:1) .NE. 'P') THEN
            CALL MSGPUT ('Smoothing beam has integral of 1.0', 'I')
         ELSE
            CALL MSGPUT ('Smoothing beam has peak of 1.0', 'I')
            CALL IMGP2PB ('Image', BEAM, 'Image')
         END IF
      END IF
C
C Write history information
C
      CALL HISINPUT ('Image')
C
C Write output file
C
      CALL FILIMGPU ('Image', NIMGFILE, ' ')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
