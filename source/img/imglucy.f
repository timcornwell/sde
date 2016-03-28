C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imglucy.f	1.3    11/7/90
C
      SUBROUTINE IMGLUCY (DRT, XFR, LUCY, RES, MVIS)
C
CD LUCY an image.
C
C
C	DRT	CH*(*)	input	Name of Dirty image
C	XFR	CH*(*)	input	Name of Transfer Function
C	LUCY	CH*(*)	input	Name of LUCY image
C	RES	CH*(*)	input	Name of Residual image
C	MVIS	CH*(*)	input	Name of FT file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added output of dispersion
C				T.J.Cornwell	July 25 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, XFR, LUCY, RES, MVIS
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGLUCY')
C
      REAL		MAXRES
      INTEGER		ITER, NITER, BITER, ANITER
      INTEGER		NDUMMY
      REAL		AMAX, AMIN, ARMS, AAVE
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get control parameters
C
      IF (DATEXIST(STRM2(LUCY, 'NITER'))) THEN
         CALL DATGETI(LUCY, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 100
         CALL DATPUTI(LUCY, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(LUCY, 'BITER'))) THEN
         CALL DATGETI(LUCY, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(LUCY, 'BITER', BITER, 1)
      END IF
C
C Finally do something
C
      DO 10 ITER = BITER, NITER
         CALL IMGCONV (LUCY, ' ', XFR, RES, MVIS)
         CALL ARRDIV (DRT, RES, RES)
         CALL IMGCORR (RES, ' ', XFR, RES, MVIS)
         CALL ARRMULT (LUCY, RES, LUCY)
         CALL ARRSTAT (RES, ' ')
         CALL DATGETR (RES, 'ARRAVE', AAVE, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRMAX', AMAX, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRMIN', AMIN, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRRMS', ARMS, 1, NDUMMY)
         MAXRES = MAX(ABS(AMAX), ABS(AMIN))
         WRITE (MESSAGE, 1000) ITER, AMIN, AMAX,
     $      SQRT(ABS(ARMS**2 - AAVE**2))
 1000    FORMAT ('It: ',I4,' Ratio image: min, max, disp = ',
     1      3(1PE11.3))
         CALL MSGPUT (MESSAGE, 'I')
         IF (ERROR)GO TO 990
  10  CONTINUE
      ANITER = NITER
  11  CONTINUE
C
C Store goodies
C
      CALL DATPUTI (LUCY, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
