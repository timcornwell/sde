C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)mask.f	1.5    7/27/93
C
      SUBROUTINE SDEMAIN
C
CD Program to create/manipulate image masks
C
C Audit trail:
C	Original version:
C				D.S.Briggs	November 22 1991
C	Added output statistic, INFILE, OUTFILE, OP
C				D.S.Briggs	April 24 1993
C	Split Shrink option into MShrink and IShrink
C				D.S.Briggs	13 July 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MASK')
C
      CHARACTER*(SYSMXNAM)	CURFILE, IMGFILE, MASKFILE, MODE,
     $   		INFILE, OUTFILE, APPLY
      REAL		TRIMVAL, FILLVAL, BACKVAL, EXCLVAL,
     $   		RMAX, RMIN
      INTEGER		NDUMMY, NLOC
      LOGICAL		DOISHRNK, DOMSHRNK
C
      REAL		DATFGETR
      INTEGER		DATFGETI
      LOGICAL		DATEXIST
C==================================================================
      CALL MSGWELCO ('I create or manipulate masks')
C
C Call user interface routine, and set debug status
C
      CALL USRCTL
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Inputs
C
      CALL USRGETC ('Cursor', CURFILE, 1, NDUMMY)
      CALL USRGETC ('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETC ('Mask', MASKFILE, 1, NDUMMY)
      CALL USRGETC ('Mode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, MODE)
      CALL USRGETR ('TrimVal', TRIMVAL, 1, NDUMMY)
      CALL USRGETR ('FillVal', FILLVAL, 1, NDUMMY)
      CALL USRGETR ('BackVal', BACKVAL, 1, NDUMMY)
      CALL USRGETR ('ExclVal', EXCLVAL, 1, NDUMMY)
      CALL USRGETL ('MShrink', DOMSHRNK, 1, NDUMMY)
      CALL USRGETL ('IShrink', DOISHRNK, 1, NDUMMY)
      CALL USRGETC ('InFile', INFILE, 1, NDUMMY)
      CALL USRGETC ('OutFile', OUTFILE, 1, NDUMMY)
      CALL USRGETC ('Apply', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, APPLY)
C
      IF (MODE.EQ.' ') MODE = 'S'
      MODE(2:) = ' '
      IF (MODE.EQ.'S') THEN
         CALL MSGPUT('Creating mask from SAOImage cursor file','I')
      ELSE IF (MODE.EQ.'M') THEN
         CALL MSGPUT('Modifying mask via SAOImage cursor file','I')
      ELSE IF (MODE.EQ.'A') THEN
         CALL MSGPUT('Making mask via absolute trim on image','I')
      ELSE IF (MODE.EQ.'R') THEN
         CALL MSGPUT('Making mask via relative trim on image','I')
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'MODE not recognized')
         GO TO 999
      END IF
C
      IF (APPLY.EQ.' ') APPLY = 'MULT'
      APPLY(2:) = ' '
      IF (INFILE.NE.' ') THEN
         IF (APPLY.EQ.'M') THEN
            CALL MSGPUT ('Will multiply INFILE by MASK','I')
         ELSE IF (APPLY.EQ.'A') THEN
            CALL MSGPUT ('Will add MASK to INFILE','I')
         ELSE IF (APPLY.EQ.'S') THEN
            CALL MSGPUT ('Will set selected pixels to TRIMVAL','I')
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'APPLY not recognized')
            GO TO 999
         END IF
      END IF
C
      IF (INFILE.EQ.'*') INFILE = IMGFILE
      IF (OUTFILE.EQ.'*') OUTFILE = INFILE
C
      IF ((MODE.EQ.'S').OR.(MODE.EQ.'M')) THEN
         IF (MODE.EQ.'M') CALL FILIMGGE ('Mask', IMGFILE, ' ')
         CALL IMGSAOMS (CURFILE, IMGFILE, 'Mask', FILLVAL, BACKVAL,
     $      EXCLVAL)
C
      ELSE
         CALL FILIMGGE ('Image', IMGFILE, ' ')
         IF (INFILE.EQ.IMGFILE) THEN
            CALL IMGCLONE ('Image', 'InFile')
            CALL ARRCOPY ('Image', 'InFile')
         END IF
C
C Convert REL-TRIM case to ABS-TRIM
C
         IF (MODE.EQ.'R') THEN
            CALL ARRSTAT ('Image', ' ')
            RMAX = DATFGETR('Image', 'ARRMAX')
            RMIN = DATFGETR('Image', 'ARRMIN')
            IF (ABS(RMIN).GE.ABS(RMAX)) RMAX = ABS(RMIN)
            TRIMVAL = RMAX * TRIMVAL
         END IF
C
C Make the mask from an existing image by being clever
C
         CALL IMGCLONE ('Image', 'Mask')
         CALL ARRABS ('Image', 'Image')
         CALL ARRCLIP ('Image', -1.0, TRIMVAL, 'Temp')
         CALL ARRLC ('Image', 1.0, 'Temp', -1.0, 'Mask')
         CALL ARRDIV ('Mask', 'Mask', 'Mask')
         CALL ARRCOPY ('Mask', 'InvMask')
         CALL ARRSETCO ('InvMask', 0.0, 1.0)
         CALL ARRLC ('InvMask', 1.0, 'Mask', -1.0, 'InvMask')
         CALL ARRSCALE ('Mask', FILLVAL, 0.0, 'Mask')
         CALL ARRSCALE ('InvMask', BACKVAL, 0.0, 'InvMask')
         CALL ARRADD ('Mask', 'InvMask', 'Mask')
         CALL DATDELET ('Temp')
         CALL DATDELET ('InvMask')
      END IF
C
C Statistics
C
      CALL ARRSTAT ('Mask', ' ')
      NLOC = DATFGETI('Mask','ARRNLOC')
      WRITE (MESSAGE, 1000) NLOC
 1000 FORMAT (I8,' non-zero pixels in mask')
      CALL MSGPUT (MESSAGE,'I')
      IF (ERROR) GO TO 999
C
C Apply the mask if needed
C
      IF (INFILE.NE.' ') THEN
         IF (.NOT.DATEXIST('InFile'))
     $      CALL FILIMGGE ('InFile', INFILE, ' ')
         CALL MSGPUT ('Applying mask to image','I')
         CALL IMGFITTO ('Mask', 'InFile', 'TmpMask')
         IF (ERROR) GO TO 999
         IF (APPLY.EQ.'M') THEN
            CALL ARRMULT ('InFile', 'TmpMask', 'InFile')
         ELSE IF (APPLY.EQ.'A') THEN
            CALL ARRADD ('InFile', 'TmpMask', 'InFile')
         ELSE IF (APPLY.EQ.'S') THEN
            CALL ARRCOPY ('Mask', 'InvMask')
            CALL ARRSETCO ('InvMask', 0.0, 1.0)
            CALL ARRLC ('InvMask', 1.0, 'Mask', -1.0, 'InvMask')
            CALL ARRMULT ('InFile', 'InvMask', 'Save')
            CALL ARRMULT ('InFile', 'Mask', 'Select')
            CALL ARRLC ('Mask', TRIMVAL, 'Save', 1.0, 'InFile')
         END IF
         CALL DATRENAM ('InFile', 'OutFile')
         IF (DOISHRNK) THEN
            CALL MSGPUT ('Shrinking output IMAGE to minimum support',
     $         'I')
            CALL IMGSTRIM ('OutFile')
         END IF
         CALL HISINPUT ('OutFile')
         CALL FILIMGPU ('OutFile', OUTFILE, ' ')
      END IF
C
      IF (MASKFILE.NE.' ') THEN
         IF (DOMSHRNK) THEN
            CALL MSGPUT ('Shrinking MASK to minimum support','I')
            CALL IMGSTRIM ('Mask')
         END IF
         CALL HISINPUT ('Mask')
         CALL FILIMGPU ('Mask', MASKFILE, ' ')
      END IF
C
 999  CONTINUE
      END
