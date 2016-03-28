C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgval.f	1.2    11/7/94
C
      SUBROUTINE SDEMAIN
C
C I extract values from an image
C
C Audit trail:
C	Original version
C				D.S.Briggs	July 30 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGVAL')
C
      INTEGER	NPMAX
      PARAMETER	(NPMAX=20)
C
      CHARACTER*(SYSMXNAM)      IMGFILE, LISTFILE, COORDS
      INTEGER		NPTS, ORDER
C
      CHARACTER 	LINEBUFF*1024, NUMBUFF*20
      INTEGER		NDUMMY, I, L, XADD, YADD, VADD
      LOGICAL		DOLIST
C
      INTEGER		STRLEN, STRSTART, DATADD
C==================================================================
      CALL MSGWELCO ('I extract values from images')
      CALL USRCTL
C
C Get input images
C
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETI ('Npts', NPTS, 1, NDUMMY)
      CALL USRGETC ('Coord', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, COORDS)
      CALL USRGETI ('Order', ORDER, 1, NDUMMY)
      CALL USRGETC ('ListFile', LISTFILE, 1, NDUMMY)
C
      CALL DATMAKAR ('X', 1, NPMAX, 'R', XADD)
      CALL DATMAKAR ('Y', 1, NPMAX, 'R', YADD)
      CALL USRGETR ('X', MEMR(XADD), NPMAX, NDUMMY)
      CALL USRGETR ('Y', MEMR(YADD), NPMAX, NDUMMY)
      IF (ERROR) GOTO 999
C
      CALL FILIMGGE ('Image', IMGFILE, ' ')
      CALL IMGIMVAL ('Image', 'X', 'Y', 'Vals', NPTS, COORDS, ORDER)
      IF (ERROR) GO TO 999
C
      DOLIST = LISTFILE.NE.' '
      IF (DOLIST) THEN
         CALL TXTOPEN ('ListFile', LISTFILE, 'WRITE')
         IF (ERROR) THEN
            MESSAGE = 'Error opening ' // LISTFILE(1:STRLEN(LISTFILE))
     $         // ' for WRITE'
            CALL MSGPUT (MESSAGE,'E')
            GO TO 999
         END IF
         MESSAGE = 'Opening ' // LISTFILE(1:STRLEN(LISTFILE)) //
     $      ' for WRITE as ListFile'
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      LINEBUFF = ' '
      VADD = DATADD ('Vals')
      DO 100 I = 1, NPTS
         WRITE (NUMBUFF, 1000) MEMR(VADD+I-1)
 1000    FORMAT (1PE20.4)
         L = STRLEN(LINEBUFF)
         LINEBUFF(L+2:) = NUMBUFF(STRSTART(NUMBUFF):)
 100  CONTINUE
      IF (DOLIST) THEN
         CALL TXTWRITE ('ListFile', LINEBUFF)
      ELSE
         CALL MSGPUT (LINEBUFF,'L')
      END IF
C
      IF (DOLIST) CALL TXTCLOSE ('ListFile')
C
 999  CONTINUE
      END
