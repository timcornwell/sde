C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgfndsd.f	1.1    3/6/91
C
      SUBROUTINE IMGFNDSD (PSF)
C
CD Find worse sidelobe by looking outside fitted Gaussian
C  The sidelobe value is to be found the the header item EXTSIDE
C  which is real.
C
C	PSF	CH*(*)	input	Directory name of PSF
C
C Audit trail:
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	PSF
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='IMGFNDSD')
C
C Local variables
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		NDUMMY, BP
      REAL		DATFGETR, EXTSIDE, CELL(SYSMXDIM)
C=====================================================================
      IF (ERROR) GO TO 999
C
C Get array attributes
C
      CALL DATGETR (PSF, 'CDELT', CELL, SYSMXDIM, NDUMMY)
      IF (ERROR) GO TO 990
      BP = MAX(1, 3*NINT(DATFGETR(PSF,'BMAJ')/ABS(CELL(1))))
C
      CALL IMGSUBPS (PSF, STRM2(PSF, 'SPSF'), BP)
C
C Add exterior sidelobe level
C
      EXTSIDE = DATFGETR (STRM2(PSF, 'SPSF'), 'EXTSIDE')
C
      CALL DATPUTR(PSF, 'EXTSIDE', EXTSIDE, 1)
C
      WRITE (MESSAGE, 1000) BP, EXTSIDE
 1000 FORMAT ('Worst sidelobe outside main lobe (',I3,' pixels) is ',
     $   F7.3)
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DATDELET (STRM2(PSF, 'SPSF'))
C
C Trace errors
C
  990 IF (ERROR) CALL ERRTRACE (ROUTINE)
C
  999 CONTINUE
      END
