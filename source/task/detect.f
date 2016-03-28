C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)detect.f	1.1 6/3/92
C
      SUBROUTINE SDEMAIN
C
CD Program to detect a feature using LSQ.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 27 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DETECT')
C
      CHARACTER*(SYSMXNAM)	PSFFILE, INFILE, SIGFILE
      INTEGER		NDUMMY, BLC(SYSMXDIM), TRC(SYSMXDIM)
      REAL		TOP, BOT, DET, DATFGETR
      DATA              BLC / SYSMXDIM*1/
      DATA              TRC / SYSMXDIM*1/
C==================================================================
      CALL MSGWELCO ('I detect signals in dirty images')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC('Dirty', INFILE, 1, NDUMMY)
      CALL USRGETC('PSF', PSFFILE, 1, NDUMMY)
      CALL USRGETC('Signal', SIGFILE, 1, NDUMMY)
C
      CALL FILIMGGE ('Dirty', INFILE, ' ')
      CALL FILIMGGE ('Signal', SIGFILE, ' ')
C
C Make the signal convolved with the PSF
C
      IF(PSFFILE.NE.' ') THEN
         CALL FILIMGGE ('PSF', PSFFILE, ' ')
         CALL IMGMAKEX ('PSF', 'XFR')
         CALL DATDELET ('PSF')
         CALL IMGCLONE ('XFR', 'Modvis')
         CALL CRDHALF ('Signal', BLC, TRC)
         CALL DATCREAT ('Window')
         CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
         CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
         CALL IMGSUBSE ('Signal', 'SSignal', 'Window')
         CALL IMGFFT ('SSignal', 'Modvis')
         CALL ARRMULT ('Modvis', 'XFR', 'Modvis')
         CALL IMGFFT ('Modvis', 'Signal')
      ENDIF
C
C Now calculate convolution
C
      CALL ARRMULT ('Dirty', 'Signal', 'Dirty')
      CALL ARRSTAT ('Dirty', 'Window')
      CALL ARRMULT ('Signal', 'Signal', 'Signal')
      CALL ARRSTAT ('Signal', 'Window')
C
C Output result
C
      TOP = DATFGETR ('Dirty', 'ARRSUM')
      BOT = DATFGETR ('Signal', 'ARRSUM')
      IF(ERROR) GO TO 999
      IF(BOT.NE.0.0) THEN
         DET = TOP/BOT
         WRITE (MESSAGE, 1000) DET
 1000    FORMAT ('Detected Signal = ',1PE12.5,' units')
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         WRITE (MESSAGE, 1100)
 1100    FORMAT ('Denominator is zero!')
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
 999  CONTINUE
      END
