C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgx2x.f	1.5	 12/8/92
C
      SUBROUTINE SDEMAIN
C
CD Converts between different types of complex images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 3 1990
C	Allowed shorthand of '0' for INFILE1 & INFILE2
C				D.S.Briggs	Aug 22 1992
C	Deleted 'Im1' and 'Im2' after we read them in
C				M.A.Holdawat	Dec 9 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGX2X')
C
      CHARACTER*(SYSMXNAM)	INFILE1, INFILE2, OUTFILE1, OUTFILE2,
     $   			XTYPEIN, XTYPEOUT
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	X, QU, AP, PC
      DATA		X /'X'/
      DATA		QU /'QU'/
      DATA		AP /'AP'/
      DATA		PC /'PC'/
C=======================================================================
      CALL MSGWELCO ('I translate different types of complex images')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('IN1', INFILE1, 1, NDUMMY)
      CALL USRGETC ('IN2', INFILE2, 1, NDUMMY)
      CALL USRGETC ('OUT1', OUTFILE1, 1, NDUMMY)
      CALL USRGETC ('OUT2', OUTFILE2, 1, NDUMMY)
      CALL USRGETC ('XTYPEOUT', XTYPEOUT, 1, NDUMMY)
      CALL USRGETC ('XTYPEIN',  XTYPEIN, 1, NDUMMY)
C
C Get input image
C
      IF (XTYPEIN .EQ. X .AND. INFILE1 .NE. ' ') THEN
        CALL FILIMGGE ('Image', INFILE1, ' ')
      ELSE IF (INFILE1 .NE. ' ' .AND. INFILE2 .NE. ' ') THEN
         IF (INFILE1.EQ.'0') THEN
            CALL FILIMGGE ('Im2', INFILE2, ' ')
            CALL IMGCLONE ('Im2', 'Im1')
            CALL ARRSETCO ('Im1', 0.0, 0.0)
         ELSE IF (INFILE2.EQ.'0') THEN
            CALL FILIMGGE ('Im1', INFILE1, ' ')
            CALL IMGCLONE ('Im1', 'Im2')
            CALL ARRSETCO ('Im2', 0.0, 0.0)
         ELSE
            CALL FILIMGGE ('Im1', INFILE1, ' ')
            CALL FILIMGGE ('Im2', INFILE2, ' ')
         END IF
         IF (XTYPEIN .EQ. QU) THEN
            CALL ARRQU2X ('Im1', 'Im2', 'Image')
         ELSE IF (XTYPEIN .EQ. AP) THEN
            CALL ARRAP2X ('Im1', 'Im2', 'Image')
         ELSE IF (XTYPEIN .EQ. PC) THEN
            CALL ARRPC2X ('Im1', 'Im2', 'Image')
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Unknown Input Xtype: '//XTYPEIN(1:5) )
            GOTO 999
         ENDIF
         CALL HEDCOPY ('Im1', 'Image')
         CALL HISCOPY ('Im1', 'Image')
         CALL DATDELET ('Im1')
         CALL DATDELET ('Im2')
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'You must provide 2 infile names for non-X input')
         GOTO 999
      ENDIF
      IF (ERROR) GOTO 999
C
C Churn it back out in proper Xtype
C
      IF (XTYPEOUT .EQ. X .AND. OUTFILE1 .NE. ' ') THEN
        CALL FILIMGPU ('Image', OUTFILE1, ' ')
      ELSE 
         IF (XTYPEOUT .EQ. QU) THEN
            CALL ARRX2QU ('Image', 'Im1', 'Im2')
         ELSE IF (XTYPEOUT .EQ. AP) THEN
            CALL ARRX2AP ('Image', 'Im1', 'Im2')
            CALL DATPUTC ('Im2', 'BUNIT', 'DEGREES', 1)
         ELSE IF (XTYPEOUT .EQ. PC) THEN
            CALL ARRX2PC ('Image', 'Im1', 'Im2')
            CALL DATPUTC ('Im2', 'BUNIT', 'DEGREES', 1)
         ELSE
            CALL ERRREPOR (ERRBDARG, ROUTINE,
     $         'Unknown Output Xtype: '//XTYPEOUT(1:5) )
            GOTO 999
         ENDIF
         IF (ERROR) GOTO 999
         CALL HEDCOPY ('Image', 'Im1')
         CALL HISCOPY ('Image', 'Im1')
         CALL HEDCOPY ('Image', 'Im2')
         CALL HISCOPY ('Image', 'Im2')
         IF (OUTFILE1 .NE. ' ') THEN
            CALL FILIMGPU ('Im1', OUTFILE1, ' ')
         ENDIF
         IF (OUTFILE2 .NE. ' ') THEN
            CALL FILIMGPU ('Im2', OUTFILE2, ' ')
         ENDIF
      ENDIF
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
