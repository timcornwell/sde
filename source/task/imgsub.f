C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgsub.f	1.4    7/15/93
C
      SUBROUTINE SDEMAIN
C
CD Program to sub-section and sum images
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Hooked up STEP, previously not utilized via WINDOW
C				M.A.Holdaway	Aug 3 1990
C	Added summing capability
C				R.G. Marson     Mar 4 1992
C	Added Shrink logical option
C				D.S.Briggs	13 July 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGSUB')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	INFILE, OUTFILE
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM),
     1			STEP(SYSMXDIM), SUM(SYSMXDIM)
      LOGICAL		DOSHRINK
C==================================================================
      CALL MSGWELCO ('I sub-section and Sum Images')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('In', INFILE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL FILIMGGE ('In', INFILE, ' ')
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('STEP', STEP, SYSMXDIM, NDUMMY)
      CALL USRGETI ('SUM', SUM, SYSMXDIM, NDUMMY)
      CALL USRGETL ('Shrink', DOSHRINK, 1, NDUMMY)
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'STEP', STEP, SYSMXDIM)
      CALL DATPUTI( 'Window', 'SUM', SUM, SYSMXDIM)
      CALL IMGSUBSE ('In', 'Out', 'Window')
      CALL HISINPUT ('Out')
C
      IF (DOSHRINK) THEN
         CALL MSGPUT ('Shrinking output IMAGE to minimum support','I')
         CALL IMGSTRIM ('OutFile')
      END IF
      CALL FILIMGPU ('Out', OUTFILE, ' ')
C
 999  CONTINUE
      END
