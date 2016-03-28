C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filexlpu.f	1.3    7/13/94
C
      SUBROUTINE FILEXLPU (IMAGE, EXLFILE, MASK)
C
CD Write masked image to 'Excel' file
C
C	IMAGE	CH*(*)	input	Name of image
C	EXLFILE	CH*(*)	input	Name of output file
C	MASK	CH*(*)	input	Name of image mask
C
C	The format is primarily designed to be used with the curvefitting
C	program GaussFit, but it is a simple ASCII interchange format that
C	should be understood by most spreadsheet type programs
C
C	Note that it will not deal with non-zero rotions correctly.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 2 1992
C	Tweaked for use with AIPS task TBIN
C				D.S.Briggs	June 14 1994
C	CC table functions moved to FILCCPU, and original version
C       restored
C				D.S.Briggs	June 21 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILEXLPU')
C
      CHARACTER*(*)	IMAGE, EXLFILE, MASK
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD, MADD, I
      CHARACTER		ERRLEV*5, TAB*1, CRDFILE*(SYSMXNAM),
     $   		NUMBUF*20
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
C
      INTEGER		DATADD, STRSTART, STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      CALL IMGMSCHK (IMAGE, MASK, 'EXACT', ERRLEV)
      IF (ERROR) GO TO 990
C
      IADD = DATADD (IMAGE)
      MADD = DATADD (MASK)
      CRDFILE = EXLFILE
      CALL STRAPPEN (CRDFILE, '.CRD')
      CALL CRDGET (IMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      TAB = CHAR(9)
C
C Write the Excel file itself
C
      CALL FILDEL (EXLFILE)
      MESSAGE = 'Opening ' // EXLFILE(1:STRLEN(EXLFILE)) //
     $   ' as Excel file'
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('Excel', EXLFILE, 'WRITE')
      IF (ERROR) GO TO 990
      MESSAGE = 'RAPix'//TAB//'DecPix'//TAB//'dRA'//TAB//'dDec'//TAB//
     $   'Img'
      CALL TXTWRITE ('Excel', MESSAGE)
      MESSAGE = 'double'//TAB//'double'//TAB//'double'//TAB//'double'//
     $   TAB//'double'
      CALL TXTWRITE ('Excel', MESSAGE)
      CALL PIX2DRXL (MEMR(IADD), MEMR(MADD), NAXIS(1), NAXIS(2),
     $   RPIX(1), RPIX(2), DELT(1), DELT(2), 'Excel')

      CALL TXTCLOSE ('Excel')
C
C Now dump the coordinates to <EXCEL>.CRD
C
      CALL FILDEL (CRDFILE)
      MESSAGE = 'Opening ' // CRDFILE(1:STRLEN(CRDFILE)) //
     $   ' as Excel coordinate file'
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('ExcelCRD', CRDFILE, 'WRITE')
      IF (ERROR) GO TO 990
      MESSAGE = 'AXIS'//TAB//'CTYPE'//TAB//'NAXIS'//TAB//'CRVAL'//TAB//
     $   'CDELT'//TAB//'CRPIX'//TAB//'CROTA'
      CALL TXTWRITE ('ExcelCRD', MESSAGE)
      MESSAGE = 'double'//TAB//'char'//TAB//'double'//TAB//'double'//
     $   TAB//'double'//TAB//'double'//TAB//'double'
      CALL TXTWRITE ('ExcelCRD', MESSAGE)
 1000 FORMAT (I12)
 1010 FORMAT (1PE20.9)
 1020 FORMAT (1PE20.11)
      DO 100 I = 1, NAX
         WRITE (NUMBUF, 1000) I
         MESSAGE = NUMBUF(STRSTART(NUMBUF):12) // TAB //
     $      TYPE(I)
         WRITE (NUMBUF, 1000) NAXIS(I)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):12)
         WRITE (NUMBUF, 1020) RVAL(I)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) DELT(I)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) RPIX(I)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) ROTA(I)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         CALL TXTWRITE ('ExcelCRD', STRBUF)
 100  CONTINUE
      CALL TXTCLOSE ('ExcelCRD')
C
C All done
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
