C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visflagv.f	1.3    7/17/92
C
       SUBROUTINE VISFLAGV (NAME, SUB, IMG, THRES)
C
CD Edit visibility data on the basis on distance from v axis
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	IMG	CH*(*)	input   Name of image to define coords
C      THRES   REAL    input   Threshold in sigma
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added capability to rotate uvw
C				T.J.Cornwell	July 17 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, IMG
       REAL		THRES
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISFLAGV')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, VADD,
     &                  UADD, WADD, UU, VV, WW,
     1			WTADD, NVIS, NFLAG, IVIS
      DOUBLE  PRECISION	SMAT (3,3)
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
      NFLAG = 0
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      NVIS = NAXIS(1)
      UADD = DATADD (STRM2(NAME, 'UU'))
C
C With rotation: need to find rotation matrix required to align data
C
      IF(IMG.NE.' ') THEN
         VADD = DATADD (STRM2(NAME, 'VV'))
         WADD = DATADD (STRM2(NAME, 'WW'))
         CALL CRDSHIFT (STRM2(NAME, SUB), IMG, SMAT)
         DO 10 IVIS = 1, NVIS
            IF(MEMR(WTADD+IVIS-1).LE.0.0) GO TO 10
            UU=MEMR(UADD+IVIS-1)
            VV=MEMR(VADD+IVIS-1)
            WW=MEMR(WADD+IVIS-1)
            UU = SMAT(1,1) * UU + SMAT(2,1) * VV + SMAT(3,1) * WW
            IF (ABS(UU).LT.THRES) THEN
               NFLAG = NFLAG + 1
               MEMR(WTADD+IVIS-1) = -1.0
            END IF
 10      CONTINUE
      ELSE
C
C Without rotation
C
         DO 20 IVIS = 1, NVIS
            IF(MEMR(WTADD+IVIS-1).LE.0.0) GO TO 20
            UU=MEMR(UADD+IVIS-1)
            IF (ABS(UU).LT.THRES) THEN
               NFLAG = NFLAG + 1
               MEMR(WTADD+IVIS-1) = -1.0
            END IF
 20      CONTINUE
      END IF
      IF (ERROR) GO TO 990
C
      WRITE (MESSAGE, 1100) NFLAG
 1100 FORMAT ('Edited ',I8,' records')
      CALL MSGPUT (MESSAGE, 'I')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
