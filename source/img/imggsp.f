C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imggsp.f	1.4    5/20/92
C
      SUBROUTINE IMGGSP (DRT, XFR, GSP, BOX, RES, MVIS)
C
CD GSP an image.
C
C
C	DRT	CH*(*)	input	Name of Dirty image
C	XFR	CH*(*)	input	Name of Transfer Function
C	GSP	CH*(*)	input	Name of GSP image
C	BOX	CH*(*)	input	Name of box file
C	RES	CH*(*)	input	Name of Residual image
C	MVIS	CH*(*)	input	Name of FT file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added 'Clean Box'
C				D.S.Briggs	Mar 5 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DRT, XFR, GSP, BOX, RES, MVIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGGSP')
C
      REAL		GAIN, FLUX, MAXRES, ACCEL
      INTEGER		ITER, NITER, BITER, ANITER, NDUMMY
      REAL		AMAX, AMIN, ARMS, AAVE, Q
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find number of points per beam
C
      CALL ARRSTAT (XFR, ' ')
      CALL DATGETR (XFR, 'ARRAVE', AAVE, 1, NDUMMY)
      CALL DATGETR (XFR, 'ARRRMS', ARMS, 1, NDUMMY)
      Q = AAVE / ARMS
      WRITE (MESSAGE, 1200) Q
 1200 FORMAT ('Fourier plane filling factor = ',F10.2)
      CALL MSGPUT (MESSAGE, 'I')
C
C Get control parameters
C
      IF (DATEXIST(STRM2(GSP, 'NITER'))) THEN
         CALL DATGETI(GSP, 'NITER', NITER, 1, NDUMMY)
      ELSE
         NITER = 100
         CALL DATPUTI(GSP, 'NITER', NITER, 1)
      END IF
      IF (DATEXIST(STRM2(GSP, 'BITER'))) THEN
         CALL DATGETI(GSP, 'BITER', BITER, 1, NDUMMY)
      ELSE
         BITER = 1
         CALL DATPUTI(GSP, 'BITER', BITER, 1)
      END IF
      IF (DATEXIST(STRM2(GSP, 'GAIN'))) THEN
         CALL DATGETR(GSP, 'GAIN', GAIN, 1, NDUMMY)
      ELSE
         GAIN = 0.01
         CALL DATPUTR(GSP, 'GAIN', GAIN, 1)
      END IF
      IF (DATEXIST(STRM2(GSP, 'ACCEL'))) THEN
         CALL DATGETR(GSP, 'ACCEL', ACCEL, 1, NDUMMY)
      ELSE
         ACCEL = -0.5
         CALL DATPUTR(GSP, 'ACCEL', ACCEL, 1)
      END IF
      IF (DATEXIST(STRM2(GSP, 'FLUX'))) THEN
         CALL DATGETR(GSP, 'FLUX', FLUX, 1, NDUMMY)
      ELSE
         FLUX = 0.0
         CALL DATPUTR(GSP, 'FLUX', FLUX, 1)
      END IF
C
C Finally do something
C
      CALL IMGRESID (GSP, DRT, XFR, RES, MVIS)
      DO 10 ITER = BITER, NITER
         CALL ARRMULT (GSP, BOX, GSP)
         CALL ARRLC (GSP, 1.0, RES, GAIN * Q, GSP)
         CALL ARRGSP (GSP, ACCEL, GSP)
         CALL IMGRESID (GSP, DRT, XFR, RES, MVIS)
         CALL ARRSTAT (RES, ' ')
         CALL DATGETR (RES, 'ARRMAX', AMAX, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRMIN', AMIN, 1, NDUMMY)
         CALL DATGETR (RES, 'ARRRMS', ARMS, 1, NDUMMY)
         MAXRES = MAX(ABS(AMAX), ABS(AMIN))
         WRITE (MESSAGE, 1000) ITER, AMIN, AMAX, ARMS
 1000    FORMAT ('It: ',I4,' Residuals: min, max, rms = ',
     1      3(1PE11.3))
         CALL MSGPUT (MESSAGE, 'I')
         IF (MAXRES.LT.FLUX) GO TO 11
  10  CONTINUE
      ANITER = NITER
  11  CONTINUE
C
C Store goodies
C
      CALL DATPUTR (GSP, 'FLUX', MAXRES, 1)
      CALL DATPUTI (GSP, 'NITER', ANITER, 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
