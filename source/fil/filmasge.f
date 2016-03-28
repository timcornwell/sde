C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filmasge.f	1.3    7/21/94
C
      SUBROUTINE FILMASGE (MASK, TMASKFIL, IMGTMPL)
C
CD Get mask file.  Load it as an image from disk or recalculate it as
C   needed.
C
C	MASK	CH*(*)	input	Name of mask db entry to be created
C	TMASKFIL CH*(*)	input	Name of mask disk file
C	IMGTMPL	CH*(*)	input	Name of template image
C
C If the MASKFILE exists on disk, it is opened and the initial two
C characters read.  If they are "# ", it is assumed that the file is
C an SAOImage cursor file, and that the mask must be recalculated.
C IMGTMPL may be used during the recalculation -- see IMGSAOMS.F
C If the mask already exists as a disk file, it is simply read in
C and padded/trimmed to fit the template image.  If the maskfile is
C is blank, the mask is simple cloned from the template, and set to
C unity.  The default of '*' will cause the environment variable
C SDECURSOR to be serached, followed by a default of 'saoimage.reg'
C
C Audit trail:
C	Original version
C				D.S.Briggs	3 April 1992
C	Mask is now promoted to type of image, after being read in.
C				D.S.Briggs	27 August 1992
C	Improved error reporting
C				D.S.Briggs	July 21 1994
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	MASK, TMASKFIL, IMGTMPL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILMASGE')
C
      INTEGER		NDUMMY
      LOGICAL		LDUMMY
      CHARACTER*(SYSMXNAM)	MASKFILE, TEMPNAM
      CHARACTER		ID*2, RESULT*5
C
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
      MASKFILE = TMASKFIL
      IF (MASKFILE.EQ.' ') THEN
         CALL IMGCLONE (IMGTMPL, MASK)
         CALL ARRSETCO (MASK, 0.0, 1.0)
         GO TO 990
      END IF
C
      IF (MASKFILE.EQ.'*') THEN
         CALL SYSGTENV ('SDECURSOR', MASKFILE)
         IF (MASKFILE.EQ.' ') MASKFILE = 'saoimage.reg'
      END IF
C
      CALL TXTOPEN ('Mask', MASKFILE, 'READ')
      IF (ERROR) THEN
         MESSAGE = 'Can''t open file ' // MASKFILE
         CALL MSGPUT (MESSAGE,'E')
         GO TO 990
      END IF
      CALL TXTREAD ('Mask', ID, NDUMMY, LDUMMY)
      IF (ERROR) GO TO 990
      CALL TXTCLOSE ('Mask')
C
      IF (ID.EQ.'# ') THEN
         CALL IMGSAOMS (MASKFILE, IMGTMPL, MASK, 1.0, 0.0, 0.0)
         CALL ARRPRO (MASK, IMGTMPL)
      ELSE
         CALL FILIMGGE (MASK, MASKFILE, ' ')
         CALL ARRPRO (MASK, IMGTMPL)
         CALL IMGMSCHK (IMGTMPL, MASK, 'FIT', RESULT)
         IF (RESULT.EQ.'FIT') THEN
            TEMPNAM = STRM2('TEMP',MASK)
            CALL DATRENAM (MASK, TEMPNAM)
            CALL IMGFITTO (TEMPNAM, IMGTMPL, MASK)
            CALL DATDELET (TEMPNAM)
         END IF
      END IF
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
