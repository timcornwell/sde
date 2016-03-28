
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filimgxp.f	1.3    2/26/92
C
      SUBROUTINE FILIMGXP (NAME, FILENAME, INMODE, TBLLIST)
C
CD Put (complex) image file.
C
C	NAME	 CH*(*)	input	NAME of file as specified to user
C	FILENAME CH*(*)	input	File name
C       INMODE	 CH*(*) input   File output mode  ' ' => 'AP'
C	TBLLIST  CH*(*)	input	List of tables to write
C
C 'AP' modes writes real two images, the amplitude and phase (in
C degrees).  The filenames are FILEROOT.AMP[.SYS] and FILEROOT.PHS[.SYS]
C where [.SYS] is '.FTS', '.SDE' or ''.  The appropriate filesystem is
C used, and case is preserved in the extension.
C
C 'PC' mode is for use with polarization images, and is identical
C to 'AP' mode except for a factor of two in the definition of phase.
C The filenames are FILEROOT.POL[.SYS] and FILEROOT.CHI[.SYS]
C
C 'QU' mode produces the real and imaginary part of the image, with
C the filenames FILEROOT.REAL[.SYS] and FILEROOT.IMAG[.SYS]
C
C 'X' mode produces a single, complex image.  It must be saved in SDE
C format and the extension will be forced to .SDE if necessary.
C
C 'A', 'P', 'R', and 'I' will produce a single real image, with the
C amplitude, phase, real or imaginary parts of the complec image as
C specified above.  The filename is not altered, in this case.
C
C Audit trail:
C       Cloned from FILIMGPU
C                               D.S.Briggs      Aug 22 1991
C       Now deletes temporary arrays to prevent problems in
C       subsequent calls.  Local storage allocated for MODE.
C                               D.S.Briggs      Sept 23 1991
C       Oops!  Deallocation done erroneously when CMODE = 'X'
C                               D.S.Briggs      Feb 14 1992
C	Added graceful check for NAME's existence
C				D.S.Briggs	Feb 26 1992
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	NAME, FILENAME, INMODE, TBLLIST
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILIMGXP')
C
      CHARACTER*(SYSMXNAM)	FILESYS, FILEEXT, FILEROOT,
     $			FILENAM1, FILENAM2, ACCESS
      CHARACTER		DATE*8, ATYPE*1, MODE*2
      LOGICAL		DO1, DO2
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NDUMMY
C
      LOGICAL		DATEXIST
      INTEGER		STRLEN
C
      DATA		ACCESS / 'WRITE' /
C==================================================================
C
      IF (ERROR) GO TO 999
C
C If it's not a complex image, just pass it to FILIMGPU
C
      CALL DATGETAR (NAME, NAX, NAXIS, ATYPE, NDUMMY)
      IF (ATYPE .NE. 'X') THEN
         CALL FILIMGPU (NAME, FILENAME, TBLLIST)
         GO TO 999
      END IF
C
C Default the MODE
C
      IF (INMODE.EQ.' ') THEN
         MODE = 'AP'
      ELSE
         MODE = INMODE
      ENDIF
C
C Check for null file name
C
      IF (FILENAME.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Null file name')
         GO TO 999
      END IF
C
C Make certain that the directory exists
C
      IF (.NOT.DATEXIST(NAME)) THEN
         MESSAGE = 'Image ' // NAME(1:STRLEN(NAME)) // ' doesn''t exist'
         CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Get the file root and type.
C
      CALL FILPARSE (FILENAME, FILEROOT, FILEEXT, FILESYS)
C
C Make sure that we can deal with the format
C
      IF ((FILESYS.NE.'FTS').AND.(FILESYS.NE.'SDE')) THEN
         MESSAGE = 'File system: ' // FILESYS(1:STRLEN(FILESYS))
     $      // ' not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
      IF ((FILESYS.EQ.'FTS').AND.(MODE.EQ.'X')) THEN
         IF (FILEEXT.NE.' ') THEN
            MESSAGE = 'Cannot write a complex image in FITS format'
            CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
            GO TO 999
         ELSE
            MESSAGE = 'Complex image must be saved in SDE format'
            CALL MSGPUT (MESSAGE, 'W')
            FILEEXT = 'SDE'
            FILESYS = 'SDE'
         END IF
      END IF
C
      IF ((MODE.NE.'AP').AND.(MODE.NE.'PC').AND.(MODE.NE.'QU')
     $   .AND.(MODE.NE.'X').AND.(MODE.NE.'A').AND.(MODE.NE.'P')
     $   .AND.(MODE.NE.'R').AND.(MODE.NE.'I')) THEN
         MESSAGE = 'Image output mode ' // MODE(1:STRLEN(MODE)) //
     $      ' is not supported'
         CALL ERRREPOR (ERRWRGTP, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C Add a few keyword to header:  Date, Access, Filesys
C
      CALL SYSDATEC (DATE)
      CALL DATPUTC (NAME, 'DATE-MAP', DATE, 1)
      CALL DATPUTC (NAME,'FILACCESS', ACCESS, 1)
      CALL DATPUTC (NAME,'FILSYS', FILESYS, 1)
C
C Create appropriate filenames
C
      DO1 = .TRUE.
      DO2 = .TRUE.
      IF ((MODE.EQ.'A').OR.(MODE.EQ.'R')) THEN
         FILENAM1 = FILENAME
         DO2 = .FALSE.
      ELSE IF ((MODE.EQ.'P').OR.(MODE.EQ.'I')) THEN
         FILENAM2 = FILENAME
         DO1 = .FALSE.
      ELSE IF (MODE.EQ.'X') THEN
         FILENAM1 = FILEROOT(1:STRLEN(FILEROOT)) // '.' // FILEEXT
         DO2 = .FALSE.
      ELSE
         IF (MODE.EQ.'AP') THEN
            FILENAM1 = FILEROOT(1:STRLEN(FILEROOT)) // '.AMP'
            FILENAM2 = FILEROOT(1:STRLEN(FILEROOT)) // '.PHS'
         ELSE IF (MODE.EQ.'PC') THEN
            FILENAM1 = FILEROOT(1:STRLEN(FILEROOT)) // '.POL'
            FILENAM2 = FILEROOT(1:STRLEN(FILEROOT)) // '.CHI'
         ELSE IF (MODE.EQ.'QU') THEN
            FILENAM1 = FILEROOT(1:STRLEN(FILEROOT)) // '.REAL'
            FILENAM2 = FILEROOT(1:STRLEN(FILEROOT)) // '.IMAG'
         END IF
         IF (FILEEXT.NE.' ') THEN
            FILENAM1 = FILENAM1(1:STRLEN(FILENAM1)) // '.' // FILEEXT
            FILENAM2 = FILENAM2(1:STRLEN(FILENAM2)) // '.' // FILEEXT
         END IF
      END IF
C
C Now actually do something.
C
      IF (MODE.EQ.'X') THEN
         CALL DATPUTC (NAME,'FILNAME', FILENAM1, 1)
         IF (FILESYS.EQ.'FTS') THEN
            CALL FTSOPEN (NAME)
            CALL FTSIMGWR (NAME)
            IF (TBLLIST.NE.' ') THEN
               CALL FTSTBLSW (NAME, TBLLIST)
            END IF
            CALL FTSCLOSE (NAME)
         ELSE IF (FILESYS.EQ.'SDE') THEN
            CALL DATWRITE (NAME, FILENAM1)
         END IF
      ELSE
         IF ((MODE.EQ.'AP').OR.(MODE.EQ.'A').OR.(MODE.EQ.'P')) THEN
            CALL ARRX2AP (NAME, 'Tmp/Img1', 'Tmp/Img2')
            CALL DATPUTC ('Tmp/Img2', 'BUNIT', 'DEGREES', 1)
         ELSE IF (MODE.EQ.'PC') THEN
            CALL ARRX2PC (NAME, 'Tmp/Img1', 'Tmp/Img2')
            CALL DATPUTC ('Tmp/Img2', 'BUNIT', 'DEGREES', 1)
         ELSE IF ((MODE.EQ.'QU').OR.(MODE.EQ.'R')
     $             .OR.(MODE.EQ.'I')) THEN
            CALL ARRX2QU (NAME, 'Tmp/Img1', 'Tmp/Img2')
         END IF
C
         CALL HEDCOPY (NAME, 'Tmp/Img1')
         CALL HISCOPY (NAME, 'Tmp/Img1')
         CALL DATPUTC ('Tmp/Img1','FILNAME', FILENAM1, 1)
         CALL HEDCOPY (NAME, 'Tmp/Img2')
         CALL HISCOPY (NAME, 'Tmp/Img2')
         CALL DATPUTC ('Tmp/Img2','FILNAME', FILENAM2, 1)
C
         IF (FILESYS.EQ.'FTS') THEN
            IF (DO1) THEN
               CALL FTSOPEN ('Tmp/Img1')
               CALL FTSIMGWR ('Tmp/Img1')
               IF (TBLLIST.NE.' ') CALL FTSTBLSW ('Tmp/Img1', TBLLIST)
               CALL FTSCLOSE ('Tmp/Img1')
            END IF
            IF (DO2) THEN
               CALL FTSOPEN ('Tmp/Img2')
               CALL FTSIMGWR ('Tmp/Img2')
               IF (TBLLIST.NE.' ') CALL FTSTBLSW ('Tmp/Img2', TBLLIST)
               CALL FTSCLOSE ('Tmp/Img2')
            END IF
         ELSE IF (FILESYS.EQ.'SDE') THEN
            IF (DO1) CALL DATWRITE ('Tmp/Img1', FILENAM1)
            IF (DO2) CALL DATWRITE ('Tmp/Img2', FILENAM2)
         END IF
      END IF
C
C Clean up temporary arrays if needed.
C
      IF (DATEXIST('Tmp/Img1')) CALL DATDELET ('Tmp/Img1')
      IF (DATEXIST('Tmp/Img2')) CALL DATDELET ('Tmp/Img2')
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
