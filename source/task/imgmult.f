C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgmult.f	1.5	 12/20/94
C
      SUBROUTINE SDEMAIN
C
C Program to multiply or divide two images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	July 25 1990
C	Added OPCODE = 'MC' for complex multiplication, takes
C	the complex conjugate of image2 before multiplication
C				M.A.Holdaway	Nov 27 1991
C	Added FIT logical option.  If true, will fit image2
C	to image1 before multiplication, subsectioning or padding
C	with zeros as needed.
C				D.S.Briggs	Mar 5 1992
C	Added MASK logical option.  If true, image2 may be a
C	mask, either a truncated image or a saoimage region file.
C				D.S.Briggs	Nov 20 1992
C	Added clip level for Divide option
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGMULT')
C
      CHARACTER*(SYSMXNAM) 	IMAGE1, IMAGE2,
     2                          OUTPUT, OPCODE
      INTEGER		NDUMMY, STRLEN
      LOGICAL		DOFIT, DOMASK
      REAL		CLIP
C==================================================================
      CALL MSGWELCO ('I multiply/divide images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Image1', IMAGE1, 1, NDUMMY)
      CALL USRGETC ('Image2', IMAGE2, 1, NDUMMY)
      CALL USRGETC ('Output', OUTPUT, 1 ,NDUMMY)
      CALL USRGETC ('Opcode', OPCODE, 1, NDUMMY)
      CALL USRGETR ('Clip', CLIP, 1, NDUMMY)
      CALL USRGETL ('Fit', DOFIT, 1, NDUMMY)
      CALL USRGETL ('Mask', DOMASK, 1, NDUMMY)
C
      CALL FILIMGGE ('Image1', IMAGE1, '*')
      IF (DOMASK) THEN
         CALL FILMASGE ('Image2', IMAGE2, 'Image1')
      ELSE
         CALL FILIMGGE ('Image2', IMAGE2, '*')
      END IF
C
C Force axis agreement if requested
C
      IF (DOFIT) THEN
         CALL DATRENAM ('Image2', 'TempImage')
         CALL IMGFITTO ('TempImage', 'Image1', 'Image2')
         CALL DATDELET ('TempImage')
      END IF
C
C Conjugate image if required by law
C
      IF (STRLEN(OPCODE) .GT. 1) THEN
         IF (OPCODE(2:2) .EQ. 'C') THEN
            CALL ARRCONJ ('Image2')
         ENDIF
      ENDIF
C
      CALL IMGCLONE ('Image1', 'Output')
C      
C Call main routine to multiply or divide
C
      IF (OPCODE(1:1) .EQ. 'm' .OR. OPCODE(1:1) .EQ. 'M') THEN
         CALL ARRMULT ('Image1', 'Image2', 'Output')
      ELSE IF (OPCODE(1:1) .EQ. 'd' .OR. OPCODE(1:1) .EQ. 'D') THEN
         CALL ARRCDIV ('Image1', 'Image2', CLIP, 'Output')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Unknown OPCODE: '//OPCODE)
         GOTO 999
      ENDIF
C
      CALL HISOPEN ('Output')
      CALL HISINPUT ('Output')
C
C Write result 
C
      CALL FILIMGPU('Output', OUTPUT, ' ')
C
 999  CONTINUE
      END
