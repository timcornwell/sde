C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)img2list.f	1.1    7/13/94
C
      SUBROUTINE SDEMAIN
C
CD Converts between SDE image and list based formats formats
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	June 21 1994
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMG2LIST')
C
      CHARACTER*(SYSMXNAM)	IMAGE, LIST, MASK, MODE
      LOGICAL		DOPIX, TOLIST, FROMLIST
C
      INTEGER		NDUMMY
C
      INTEGER		STRLEN
C=======================================================================
      CALL MSGWELCO ('I convert between image and list formats')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('List', LIST, 1, NDUMMY)
      CALL USRGETC ('Box', MASK, 1, NDUMMY)
      CALL USRGETC ('Mode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, MODE)
      CALL USRGETL ('GetPixels', DOPIX, 1, NDUMMY)
C
C Parse direction
C
      TOLIST = .FALSE.
      IF (MODE(1:1).EQ.'>') TOLIST = .TRUE.
      FROMLIST = .NOT.TOLIST
C
      IF (TOLIST) THEN
         CALL FILIMGGE ('Image', IMAGE, ' ')
         CALL FILMASGE ('Mask', MASK, 'Image')
      END IF
C
C Switch on mode
C
      IF (MODE.EQ.'>XL') THEN
         IF (LIST.EQ.'*') LIST = IMAGE(1:STRLEN(IMAGE)) // '.EXL'
         CALL FILEXLPU ('Image', LIST, 'Mask')
      ELSE IF (MODE.EQ.'XL>') THEN
         CALL FILEXLGE ('Image', LIST, .NOT.DOPIX)
      ELSE IF (MODE.EQ.'>CC') THEN
         IF (LIST.EQ.'*') LIST = IMAGE(1:STRLEN(IMAGE)) // '.CC'
         CALL FILCCPU ('Image', LIST, 'Mask')
      ELSE
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Unrecognized MODE')
         GO TO 999
      END IF
C
      IF (FROMLIST) THEN
         CALL FILIMGPU ('Image', IMAGE, ' ')
      END IF
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
