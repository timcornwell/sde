C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgcontr.f	1.3 24 Feb 1995
C
      SUBROUTINE IMGCONTR (IMAGE, TITLE, DEVICE, PTYPE, PIXR, BLC, TRC, 
     $   CLEV, PLEV, NC, PCLOSE)
C
CD Contours an image, optionally leaves the file open for further plotting
C
C	IMAGE	CH*(*)	inp	DEM image
C	TITLE	CH*(*)	inp	TITLE
C	DEVICE	CH*(*)	inp	Plot device
C	PTYPE	CH*(*)	inp	Plot Type (GR, QU, other)
C	PIXR	R(2)	inp	Pixel Range to plot
C	BLC	INT(*)	inp	BLC
C	TRC	INT(*)	inp	TRC
C	CLEV	REAL	inp	Absolute contour level
C	PLEV	REAL	inp	Percentage contour level
C	NC	INT	inp	Number of Contours
C	PCLOSE	L	inp	Close device upon exit?
C	
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 15 1993
C
C       Eliminate second declaration of variables.
C				M. Stupar       Dec 28 1994
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	IMAGE, TITLE, DEVICE, PTYPE
      INTEGER		BLC(*), TRC(*), NC
      REAL		CLEV, PLEV, PIXR(2)
      LOGICAL		PCLOSE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGCONTR')
C
      INTEGER		ADD, IAX, NAX, NREAL, NAXIS(SYSMXDIM)
      REAL		DMIN, DMAX
      INTEGER		MAXNC, ILEV, MXLABEL, IFONT
      INTEGER	        NL4MX
      PARAMETER		(MAXNC = 200)
      PARAMETER		(MXLABEL= 80)
      PARAMETER		(NL4MX= 3)
      INTEGER		NDUMMY, ISTAT
      REAL		CACTUAL(MAXNC), TR(6)
      REAL              CHEIGHT
      REAL		WBLC(SYSMXDIM), WTRC(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM)
      CHARACTER*1	ATYPE
      LOGICAL		FLABEL
C
      INTEGER		PGBEGIN, CRDRNAX
      REAL		DATFGETR
C
      DATA TR	/0.0, 100.0, 0.0, 0.0, 0.0, 100.0/
      DATA IFONT /2/
      DATA CHEIGHT /.6/
C
C=======================================================================
C
C If an error on input then exit immediately
C
      FLABEL = .FALSE.
C      
      IF (ERROR) GO TO 999

C
C Dump coordinates
C
      CALL MSGPUT ('Coordinates:', 'I')
      CALL CRDLIST ('Image')
C
C Find number of real axes
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, ADD)
      NREAL = CRDRNAX(NAX, NAXIS)
      IF (NREAL.LE.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No real axes!')
         GO TO 999
      END IF
C
C Validate BLC and TRC
C
      DO 10 IAX = 1, NREAL
         IF(BLC(IAX).EQ.0) BLC(IAX) = 1
         IF(TRC(IAX).EQ.0) TRC(IAX) = NAXIS(IAX)
         BLC (IAX) = MAX (1, MIN (NAXIS(IAX), BLC (IAX)))
	 TRC (IAX) = MAX (1, MIN (NAXIS(IAX), TRC (IAX)))
  10  CONTINUE
      DO 15 IAX = NREAL+1, SYSMXDIM
         BLC(IAX) = 1
         TRC(IAX) = 1
 15   CONTINUE
      CALL MSGPUT ('Plotting window: ', 'I')
      DO 20 IAX = 1, NREAL
         WRITE (MESSAGE, 4000) IAX, BLC (IAX), IAX, TRC (IAX)
 4000    FORMAT ('BLC(',I1,') = ',I4,', TRC(',I1,') = ',I4)
         CALL MSGPUT (MESSAGE, 'I')
  20  CONTINUE
C
C Find contour levels
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL ARRSTAT ('Image', 'Window')
      DMAX = DATFGETR('Image', 'ARRMAX')
      DMIN = DATFGETR('Image', 'ARRMIN')
      IF ((PIXR(1).EQ.0.0).AND.(PIXR(2).EQ.0.0)) THEN
         PIXR(1) = DMIN
         PIXR(2) = DMAX
      END IF
      IF (CLEV.NE.0.0) THEN
         DO 16 ILEV = 1, NC
             CACTUAL(ILEV) = (ILEV-1) * CLEV + PIXR(1)
 16       CONTINUE
      ELSE
         IF (PLEV.EQ.0.0) THEN
            PLEV = 10.0
         END IF
         DO 25 ILEV = 1, NC
            CACTUAL(ILEV) = (ILEV -1) * (PLEV/100.0) * DMAX
  25     CONTINUE
      END IF
C
C Now get scaling info: convert coordinates to relative with
C respect to the reference pixel. Do this by hand since CRDPTOW
C has insufficient precision for all cases
C
      CALL DATGETD ('Image', 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETR ('Image', 'CDELT', CDELT, SYSMXDIM, NDUMMY)
      CALL DATGETR ('Image', 'CRPIX', CRPIX, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 999
      DO 30 IAX = 1, NREAL
         WBLC(IAX) = 1.0*CDELT(IAX)*(FLOAT(BLC(IAX))-CRPIX(IAX))
         WTRC(IAX) = 1.0*CDELT(IAX)*(FLOAT(TRC(IAX))-CRPIX(IAX))
 30   CONTINUE
      TR(2) = 1.0 * CDELT(1)
      TR(1) = - 1.0 * CDELT(1) * CRPIX(1)
      TR(6) = 1.0 * CDELT(2)
      TR(4) = - 1.0 * CDELT(2) * CRPIX(2)
C
C Call PGPLOT routines: Get device, etc
C
      ISTAT = PGBEGIN(0, DEVICE, 1, 1)
      CALL PGENV (WBLC(1), WTRC(1), WBLC(2), WTRC(2), 1, 0)
      CALL PGIDEN
      CALL PGLABEL ('Meter', 'Meter', TITLE)
      IF (PTYPE(1:2).EQ.'GR') THEN
         CALL PGGRAY (MEMR(ADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), PIXR(1), PIXR(2), TR)
      ELSE IF(PTYPE(1:2).EQ.'QU') THEN
         CALL PGCONS (MEMR(ADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), CACTUAL, NC, TR)
      ELSE
         CALL PGCONT (MEMR(ADD), NAXIS(1), NAXIS(2), BLC(1), TRC(1), 
     1      BLC(2), TRC(2), CACTUAL, NC, TR)
      END IF
C
      IF (PCLOSE) THEN
         CALL PGEND
      ENDIF
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      ENDIF
C
 999  CONTINUE
      END
