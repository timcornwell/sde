C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpad.f	1.7    6/9/93
C
      SUBROUTINE IMGPAD (INF, PDF, PNAXIS, PADVAL)
C
CD Pad input grid to output size with padding value, 
C update header.
C
C      INF     CH*(*)    input   Name of input image
C      PDF     CH*(*)    input   Name of output image
C      PNAXIS  INT(*)    input   Size of output image
C      PADVAL  REAL      input   Padding value
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Changed call to pixrpad
C				T.J. Cornwell	Nov 20 1989
C	Added NINT to LSHIFT to get a more central pixel
C				M.A.Holdaway	Feb 7 1991
C	Do LSHIFT calcs if PNAXIS.GT.1, not NAXIS.GT.1.  Added
C	complex data type, and bitch if it can't handle the
C	arguments.
C				D.S.Briggs	Aug 26 1992
C       Added PADTYPE flag and some sanity checks
C                               R.G. Marson     May 21 1992
C	Corrected shift calculations to allow for off sized images
C				D.S.Briggs	Jun 9 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPAD')
C
      CHARACTER*(SYSMXNAM) STRM2
      LOGICAL           DATEXIST
C
      INTEGER 		PNAXIS(SYSMXDIM)
      INTEGER           LSHIFT(SYSMXDIM)
      INTEGER           NAX, NAXIS(SYSMXDIM), PADD, IADD, IAX, NDUMMY
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM), PADVAL
      CHARACTER*(SYSMXNAM) PADTYPE
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*1       ATYPE
      CHARACTER*(*) 	INF, PDF
C
C==================================================================
      IF (ERROR) GOTO 999
C
      CALL CRDGET (INF, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL DATGETAR (INF, NAX, NAXIS, ATYPE, IADD)
      IF (ERROR) GOTO 990
C
C Find out what type of padding we should do possibilities are:
C  REFerence (default) - move reference pixel to center of padded image
C  CENter              - put old image in center of padded image
C  Note if RPIX is in the center of the old image the two modes are identical
C
      PADTYPE = 'REFERENCE'
      IF (DATEXIST(STRM2(INF, 'PADTYPE'))) THEN
         CALL DATGETC(INF, 'PADTYPE', PADTYPE, 1, NDUMMY)
         CALL STRUC(PADTYPE, STRBUF)
         IF (STRBUF(1:3).EQ.'CEN') THEN 
            PADTYPE = 'CENTER'
         ELSE
            PADTYPE = 'REFERENCE'
         END IF
      END IF
C
C                         Shift reference pixels
C
      DO 20 IAX = 1, SYSMXDIM
         NAXIS(IAX) = MAX(1, NAXIS(IAX))
         PNAXIS(IAX) = MAX(NAXIS(IAX), PNAXIS(IAX))
         IF (PNAXIS(IAX).GT.1) THEN
            IF (PADTYPE(1:3).EQ.'CEN') THEN
               LSHIFT(IAX) = (PNAXIS(IAX)+1)/2 - (NAXIS(IAX)+1)/2
            ELSE
               LSHIFT(IAX) = (PNAXIS(IAX)+1)/2 - RPIX(IAX)
            END IF
         ELSE
            LSHIFT(IAX) = 0
         END IF
C
C Do some bounds checking of LSHIFT - better than getting a 'bus error'!
C
         IF ((LSHIFT(IAX) + NAXIS(IAX)).GT.PNAXIS(IAX).OR.
     $        (LSHIFT(IAX).LT.0)) THEN
            NDUMMY = MIN(PNAXIS(IAX) - NAXIS(IAX), MAX(0, LSHIFT(IAX)))
            WRITE (MESSAGE, '(A, I4, A, I1, A, I4, A)') 
     $           'Cannot shift by ', LSHIFT(IAX),
     $           ' pixels on axis ', IAX, ' Shifting by ', 
     $           NDUMMY, ' pixels only'
            CALL MSGPUT(MESSAGE, 'W')
            LSHIFT(IAX) = NDUMMY
         END IF
C
         RPIX(IAX) = RPIX(IAX) + LSHIFT(IAX)
 20   CONTINUE
C
C                        Create output array with coordinates
C
      CALL DATMAKAR (PDF, NAX, PNAXIS, ATYPE, PADD)
      CALL HEDCOPY (INF, PDF)
      CALL CRDPUT (PDF, NAX, TYPE, PNAXIS, RVAL, RPIX, DELT, ROTA)
      CALL ARRSETCO (PDF, 0.0, PADVAL)
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRPAD (MEMR(IADD), MEMR(PADD), NAXIS(1), NAXIS(2),
     $      NAXIS(3), NAXIS(4), NAXIS(5), NAXIS(6), NAXIS(7),
     $      PNAXIS(1), PNAXIS(2), PNAXIS(3), PNAXIS(4), PNAXIS(5),
     $      PNAXIS(6), PNAXIS(7), LSHIFT)
      ELSE IF (ATYPE.EQ.'X') THEN
         CALL PIXXPAD (MEMX(IADD), MEMX(PADD), NAXIS(1), NAXIS(2),
     $      NAXIS(3), NAXIS(4), NAXIS(5), NAXIS(6), NAXIS(7),
     $      PNAXIS(1), PNAXIS(2), PNAXIS(3), PNAXIS(4), PNAXIS(5),
     $      PNAXIS(6), PNAXIS(7), LSHIFT)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Can''t handle arrays of type ' // ATYPE )
      END IF
C
C                        Trace errors
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
