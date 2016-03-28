C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)wiener.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to do Wiener filtering plus subsequent smoothing with an 
C elliptical Gaussian 
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include 		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'WIENER')
C
      INTEGER 		NDUMMY, ADD, IAX
      CHARACTER*1	ATYPE
      REAL		BEAM(3), SBEAM(3), MIND, TAPER(3), FACT
      INTEGER           NAX, NAXIS(SYSMXDIM), BCEN(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	INFILE, BMFILE, WFSFILE
C
C==================================================================
C
      CALL MSGWELCO ('I do Wiener filtering and smoothing')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Deconv', BMFILE, 1, NDUMMY)
      CALL USRGETC ('WFS', WFSFILE, 1, NDUMMY)
      CALL USRGETR ('Beam', BEAM, 3, NDUMMY)
      CALL USRGETR ('MinD', MIND, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C Get input Images
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL FILIMGGE ('Deconv', BMFILE, ' ')
      CALL IMGCLONE ('Image', 'WFS')
      CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, 
     1      DELT, ROTA)
      IF (ERROR) GOTO 999
C         
      SBEAM(1) = BEAM(1)/3600.0
      SBEAM(2) = BEAM(2)/3600.0
      SBEAM(3) = BEAM(3)
C
C Convert specified beam to taper: Warning: this is not general
C enough! Fix soon!
C
      FACT = LOG(2.0)/ATAN(1.0)
      TAPER(1) = FACT*FLOAT(NAXIS(2))/ABS(BEAM(1)/DELT(1))
      TAPER(2) = FACT*FLOAT(NAXIS(2))/ABS(BEAM(2)/DELT(1))
      TAPER(3) = 90.0 + BEAM(3)
C
C Now perform taper
C
      CALL IMGFFT ('Image', 'IVis')
      CALL IMGMAKEX ('Deconv', 'XFR')
C
C Set center of tapering to middle of u,v plane
C
      CALL DATGETAR ('XFR', NAX, NAXIS, ATYPE, ADD)
      IF (ERROR) GO TO 999
      IF (ATYPE.NE.'X') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for XFR')
         GO TO 999
      END IF
      BCEN(1) = 1
      DO 10 IAX = 2, NAX
         IF (NAXIS(IAX).GT.1) THEN
            BCEN(IAX) = NAXIS(IAX) / 2
         ELSE
            BCEN(IAX) = 1
         END IF
  10  CONTINUE
      CALL ARRWIENE ('IVis', 'XFR', MIND, 'IVis')
      CALL ARRGTAPE ('IVis', TAPER, BCEN, 'IVis')
      CALL IMGFFT ('IVis', 'WFS')
C
      CALL DATPUTR ('WFS', 'BMAJ', BEAM(1)/3600.0, 1)
      CALL DATPUTR ('WFS', 'BMIN', BEAM(2)/3600.0, 1)
      CALL DATPUTR ('WFS', 'BPA',  BEAM(3), 1)
      CALL FILIMGPU ('WFS', WFSFILE, ' ')
C
 999  CONTINUE
      END
  
