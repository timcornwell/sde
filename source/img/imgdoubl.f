C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdoubl.f	1.4    2/7/91
C
      SUBROUTINE IMGDOUBL (INF, PDF)
C
CD Pad input grid to at least double size, update header. The output
C size is chosen to be a factor of two. The shift is made so that the
C reference pixel is moved to the central pixel of the new image e.g
C NX/2, NY/2 for a 2-D image.
C
C
C      INF     CH*(*)    input   Name of input image
C      PDF     CH*(*)    input   Name of output image
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed to DATEXIAR to check if output array exists. We need
C	this for links
C				T.J.Cornwell	June 21 1990
C	Added NINT to LSHIFT to get a more central reference pixel
C				M.A.Holdaway	Feb 7 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDOUBL')
C
      INTEGER 		NDUMMY, PNAXIS(SYSMXDIM)
      INTEGER           LSHIFT(SYSMXDIM)
      INTEGER           NAX, NAXIS(SYSMXDIM), PADD, IADD, IAX
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*1       OTYPE
      CHARACTER*(*) 	INF, PDF
C
      INTEGER		FFTPWR2
      LOGICAL		DATEXIAR
C==================================================================
      IF (ERROR) GOTO 999
C
      DO 10 IAX = 1, SYSMXDIM
         NAXIS(IAX) = 1
         PNAXIS(IAX) = 1
         RPIX(IAX) = 1.0
         LSHIFT(IAX) = 0
 10   CONTINUE
C
      CALL CRDGET (INF, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL DATGETAR (INF, NAX, NAXIS, OTYPE, IADD)
      IF (ERROR) GOTO 990
C
C Shift reference pixels
C
      DO 20 IAX = 1, NAX
         NAXIS(IAX) = MAX(1, NAXIS(IAX))
         IF (NAXIS(IAX).GT.1) THEN
            PNAXIS(IAX) = MAX(1, FFTPWR2(2*NAXIS(IAX)))
            LSHIFT(IAX) = NINT ( PNAXIS(IAX)/2 - RPIX(IAX) )
         ELSE
            PNAXIS(IAX) = 1
            LSHIFT(IAX) = 0
         END IF
         RPIX(IAX) = RPIX(IAX) + LSHIFT(IAX)
 20   CONTINUE
C
C Create output array with coordinates
C
      IF (.NOT.DATEXIAR(PDF)) THEN
         CALL DATCREAT (PDF)
         CALL DATMAKAR (PDF, NAX, PNAXIS, OTYPE, PADD)
         CALL HEDCOPY (INF, PDF)
      END IF
      CALL CRDPUT (PDF, NAX, TYPE, PNAXIS, RVAL, RPIX, DELT, ROTA)
C
      CALL ARRSETCO (PDF, 0.0, 0.0)
      CALL PIXRPAD (MEMR(IADD), MEMR(PADD), NAXIS(1), NAXIS(2),
     1   NAXIS(3), NAXIS(4), NAXIS(5), NAXIS(6), NAXIS(7),
     2   PNAXIS(1), PNAXIS(2), PNAXIS(3), PNAXIS(4), PNAXIS(5), 
     3   PNAXIS(6), PNAXIS(7), LSHIFT)
C
C                        Trace errors
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
