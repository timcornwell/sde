C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgk2p.f	1.4	 7/17/95
C
      SUBROUTINE IMGK2P (IMG, SIMG, FREQ)
C
CD Convert the units of an image to per pixel
C
C
C	IMG	CH*(*)	input	Name of image to be scaled
C	SIMG	CH*(*)	input	Name of scaled image
C	FREQ	REAL	input	Frequency
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Only check that first character of BUNIT is K (ie, K_Tb)
C				M.A.Holdaway	March 17 1993
C	Sets FREQ from passed parameters if non zero.
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	IMG, SIMG
      REAL		FREQ
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGK2P')
C
      INTEGER		NDUMMY, STRSEARC, I
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	CVALUE
      REAL		FACT, DELT(SYSMXDIM)
      DOUBLE PRECISION	K, C, JY, D2R
      DATA		K / 1.380658D-23 /
      DATA		C / 2.99792458D8 /
      DATA		JY / 1.0D-26 /
C=====================================================================
      D2R = ATAN(1.0D0) / 45.0D0
C
C If an error on input then exit immediately
C
      CALL DATGETC (IMG, 'BUNIT', CVALUE, 1, NDUMMY)
      IF(CVALUE(1:1).NE.'K' .AND. CVALUE(1:2) .NE. 'MK' .AND.
     $     CVALUE(1:2) .NE. 'mK') THEN
         CALL ERRREPOR (ROUTINE, ERRLOGIC, 'Units not K or mK')
         GO TO 999
      END IF
C
      IF (ERROR) GO TO 999
      IF (FREQ .LE. 0) THEN
         CALL DATGETC (IMG, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
         I = STRSEARC ('FREQ', CTYPE, NDUMMY)
         IF(I.EQ.0) THEN
            CALL ERRREPOR (ROUTINE, ERRLOGIC, 
     $           'Cannot find Frequency axis')
            GOTO 999
         END IF
         CALL DATGETD (IMG, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
         FREQ=CRVAL(I)
      ENDIF
C      
      CALL DATGETR (IMG, 'CDELT', DELT, SYSMXDIM, NDUMMY)
C
      FACT = 2.0D0 * K * (FREQ/C)**2 * ABS(DELT(1)*DELT(2)) * D2R**2 
     $   / JY
      IF (  CVALUE(1:2) .EQ. 'MK' .OR. CVALUE(1:2) .EQ. 'mK' ) THEN
         FACT = FACT / 1000.0
      ENDIF
C
      CALL ARRSCALE (IMG, FACT, 0.0, SIMG)
C
C Fill in header
C
      CALL DATPUTC (SIMG, 'BUNIT', 'JY/PIXEL', 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
