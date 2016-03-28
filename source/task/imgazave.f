C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgazave.f	1.6    12/22/94
C
      SUBROUTINE SDEMAIN
C
C     make 1-D az-averaged RMS plots of 2-D images
C
C Audit trail:
C	Original version: 
C				M.A. Holdaway	June 3 1990
C	Added LOG-LOG capability
C				M.A. Holdaway	???
C	Changed default scaling to LOG-10.  Tweaked the docs a little.
C				D.S.Briggs	June 10 1992
C	Added BOX option
C				D.S.Briggs	April 1993
C	Added text file output
C				D.S.Briggs	Oct 30 1994
C	Added averaging mode
C				D.S.Briggs	Dec 22 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGAZAVE')
C
      CHARACTER*(SYSMXNAM) 	IMAGE, BOXFILE, LISTFILE, TITLE,
     $   	XLAB, YLAB, DEV, AMODE
      REAL	XMIN, XMAX, YMIN, YMAX, SCALE, UNDEF, P
      INTEGER	NDUMMY, NPIX, AXISTYPE, I, PADD, RADD
      LOGICAL	DOCOPY
C
      INTEGER	DATADD, STRLEN
C==================================================================
C
      CALL MSGWELCO ('I make 1-D az-averaged RMS plots of 2-D images')
      CALL USRCTL
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETC ('Dev', DEV, 1, NDUMMY)
      CALL USRGETC ('List', LISTFILE, 1, NDUMMY)
      CALL USRGETC ('Average', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, AMODE)
      CALL USRGETR ('Xmin', XMIN, 1, NDUMMY)
      CALL USRGETR ('Xmax', XMAX, 1, NDUMMY)
      CALL USRGETR ('Ymin', YMIN, 1, NDUMMY)
      CALL USRGETR ('Ymax', YMAX, 1, NDUMMY)
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL USRGETI ('Nbins', NPIX, 1, NDUMMY)
      CALL USRGETL ('Copy', DOCOPY, 1, NDUMMY)
      CALL USRGETR ('Undef', UNDEF, 1, NDUMMY)
      CALL USRGETC ('Title', TITLE, 1, NDUMMY)
      CALL USRGETC ('Xlabel', XLAB, 1, NDUMMY)
      CALL USRGETC ('Ylabel', YLAB, 1, NDUMMY)
      CALL USRGETI ('AxisType', AXISTYPE, 1, NDUMMY)
      IF (ERROR) GOTO 999
C
C Massage inputs
C
      IF ((AMODE(1:1).EQ.'R').OR.(AMODE.EQ.' ')) THEN
         AMODE = 'RMS'
      ELSE IF (AMODE(1:1).EQ.'M') THEN
         AMODE = 'MEAN'
      ELSE
         CALL MSGPUT ('Unrecognized averaging mode','E')
         GO TO 999
      END IF
      MESSAGE = 'Averaging mode is ' // AMODE
      CALL MSGPUT (MESSAGE,'I')
C
C Get Image
C
      CALL FILIMGGE ('Image', IMAGE, ' ')
      CALL FILMASGE ('Box', BOXFILE, 'Image')
      IF (SCALE .LE. 0.0) SCALE = 1.0
      CALL IMGRAVEB ('Image', 'Box', 'Azave', 'Pixels', NPIX, SCALE,
     $   DOCOPY, UNDEF, AMODE)
C
      IF (LISTFILE.NE.' ') THEN
         CALL MSGPUT ('Writing averages to ASCII file ' //
     $      LISTFILE,'I')
         CALL FILDEL (LISTFILE)
         CALL TXTOPEN ('List', LISTFILE, 'WRITE')
         IF (ERROR) GO TO 999
C
         MESSAGE = '# Radius (pixels * scale)  ' //
     $      AMODE(1:STRLEN(AMODE)) // ' Azimuthal Average'
         CALL TXTWRITE ('List', MESSAGE)
 1000    FORMAT ('# Scale = ',1PE12.4)
         WRITE (MESSAGE, 1000) SCALE
         CALL TXTWRITE ('List', MESSAGE)
         PADD = DATADD ('Pixels')
         RADD = DATADD ('Azave')
         DO 100 I = 1, NPIX
            P = MEMR(PADD+I-1)
            IF (((P.GE.XMIN).AND.(P.LE.XMAX)).OR.(XMIN.EQ.XMAX)) THEN
               WRITE (MESSAGE, 1010) MEMR(PADD+I-1), MEMR(RADD+I-1)
 1010          FORMAT (1P2E12.4)
               CALL TXTWRITE ('List',MESSAGE)
            END IF
 100     CONTINUE
         CALL TXTCLOSE ('List')
      END IF
      IF (ERROR) GO TO 999
C
      IF (DEV.NE.' ') THEN
         CALL MSGPUT ('Plotting averages to device ' // DEV, 'I')
         CALL ARRPGGRF (NPIX, 'Pixels', 'Azave', DEV,
     $      XLAB, YLAB, TITLE, 0, AXISTYPE, XMIN, XMAX, YMIN, YMAX)
      END IF
C
  999 CONTINUE
      END
