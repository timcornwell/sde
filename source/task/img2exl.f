C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img2exl.f	1.3    7/13/94
C
      SUBROUTINE SDEMAIN
C
CD Converts between SDE image and Excel formats
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Apr 7 1992
C	Tweaked and restored to original form.  Largely superceded
C	by img2list
C				D.S.Briggs	June 21 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG2EXL')
C
      CHARACTER*(SYSMXNAM)	IMAGE, EXCEL, MASK
      LOGICAL		DOI2XL, DOPIX
C
      INTEGER		NDUMMY
C
      INTEGER		STRLEN
C=======================================================================
      CALL MSGWELCO ('I comvert between Image and Excel formats')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Excel', EXCEL, 1, NDUMMY)
      CALL USRGETC ('Box', MASK, 1, NDUMMY)
      CALL USRGETL ('I2XL', DOI2XL, 1, NDUMMY)
      CALL USRGETL ('Pixels', DOPIX, 1, NDUMMY)
C
C Switch on major direction
C
      IF (DOI2XL) THEN
         CALL FILIMGGE ('Image', IMAGE, ' ')
         CALL FILMASGE ('Mask', MASK, 'Image')
         IF (EXCEL.EQ.'*') EXCEL = IMAGE(1:STRLEN(IMAGE)) // '.EXL'
         CALL FILEXLPU ('Image', EXCEL, 'Mask')
      ELSE
         CALL FILEXLGE ('Image', EXCEL, .NOT.DOPIX)
         CALL FILIMGPU ('Image', IMAGE, ' ')
      END IF
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
