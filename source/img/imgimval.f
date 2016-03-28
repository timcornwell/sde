C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgimval.f	1.1    11/7/94
C
      SUBROUTINE IMGIMVAL (IMG, X, Y, VALS, NPTS, COORDS, ORDER)
C
CD Return image values
C
C	IMG	CH*(*)		input	Name of image
C	X	CH*(*)		input	Name of X array
C	Y	CH*(*)		input	Name of Y array
C	VALS	CH*(*)		input	Name of output values
C	NPTS	INT		input	Use only this many points
C	COORDS	CH*(*)		input	PIXEL or WORLD  ' ' -> 'PIXEL'
C	ORDER	INT		input	order of interpolator
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	July 30 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, X, Y, VALS, COORDS
      INTEGER		ORDER, NPTS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGIMVAL')
C
      INTEGER		IADD, XADD, YADD, VADD, NX, NY, NDUMMY
      INTEGER		NAX, NAXIS(SYSMXDIM), NREAL, I, J
      LOGICAL		DOWORLD
      REAL		PIXEL(SYSMXDIM)
      DOUBLE PRECISION	WORLD(SYSMXDIM), CRVAL(SYSMXDIM)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	CTYPE(SYSMXDIM)
C
      REAL		R2D
      PARAMETER		(R2D=57.29578)
C
      INTEGER		CRDRNAX, DATADD
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (IMG, NAX, NAXIS, ATYPE, IADD)
      IF (ERROR) GO TO 990
C
      IF (ATYPE.NE.'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Bad array type '//ATYPE//' for Image')
         GO TO 990
      END IF
C
      IF (COORDS.EQ.' ') THEN
         DOWORLD = .FALSE.
      ELSE IF (COORDS(1:3).EQ.'PIX') THEN
         DOWORLD = .FALSE.
      ELSE IF (COORDS(1:3).EQ.'WOR') THEN
         DOWORLD = .TRUE.
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unrecognized coord type')
         GO TO 999
      END IF
C
      NREAL = CRDRNAX (NAX, NAXIS)
      IF (NREAL.NE.2) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Can only use 2D images')
         GO TO 990
      END IF
      NX = NAXIS(1)
      NY = NAXIS(2)
C
      CALL DATGETAR (X, NAX, NAXIS, ATYPE, XADD)
      IF (NPTS.GT.NAXIS(1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Not enough points in X')
         GO TO 990
      END IF
      YADD = DATADD(Y)
      IF (.NOT.DATEXIST(VALS)) THEN
         CALL ARRCOPY (X, VALS)
      END IF
      VADD = DATADD(VALS)
      IF (ERROR) GO TO 990
C
      IF (DOWORLD) THEN
         IF (DATEXIST(STRM2(IMG,'CTYPE'))) THEN
            CALL DATGETC (IMG, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
            CALL DATGETD (IMG, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
            IF (ERROR) GO TO 990
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Missing coordinates')
            GO TO 999
         END IF
      END IF
C
C Main loop over values
C
      DO 500 I = 1, NPTS
         IF (DOWORLD) THEN
            DO 100 J = 1, SYSMXDIM
               WORLD(J) = 1.D0
 100        CONTINUE
            IF (CTYPE(1)(1:2).EQ.'RA') THEN
               WORLD(1) = MEMR(XADD+I-1) / (3600.0 * COS(CRVAL(2)/R2D))
     $            + CRVAL(1)
               WORLD(2) = MEMR(YADD+I-1) / 3600.0 + CRVAL(2)
            ELSE IF (CTYPE(1)(1:2).EQ.'UU') THEN
               WORLD(1) = MEMR(XADD+I-1) * 1000.0 + CRVAL(1)
               WORLD(2) = MEMR(YADD+I-1) * 1000.0 + CRVAL(2)
            ELSE
               CALL ERRREPOR (ERRFATAL, ROUTINE,
     $            'Unrecognized world coordinates')
               GO TO 999
            END IF
            CALL CRDDWTOP (IMG, WORLD, PIXEL)
         ELSE
            PIXEL(1) = MEMR(XADD+I-1)
            PIXEL(2) = MEMR(YADD+I-1)
         END IF
C
         IF ((PIXEL(1).LT.0.5).OR.(PIXEL(1).GT.NX+0.5) .OR.
     $      (PIXEL(2).LT.0.5).OR.(PIXEL(2).GT.NY+0.5)) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Bad point coordinates')
            GO TO 999
         END IF
C
         CALL PIXRGV2D (MEMR(IADD), NX, NY, PIXEL(1), PIXEL(2),
     $      MEMR(VADD+I-1), ORDER)
C
 500  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END

