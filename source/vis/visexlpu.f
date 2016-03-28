C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visexlpu.f	1.1    6/7/93
C
      SUBROUTINE VISEXLPU (VIS, EXLFILE, CLASS, DOALL, DOVAR)
C
CD Write visibility data to 'Excel' file
C
C	VIS	CH*(*)	input	Name of image
C	EXLFILE	CH*(*)	input	Name of output file
C	CLASS	CH*(*)	input	Name of class to save e.g. OBS/I
C	DOALL	LOG	input	Either write all vis, or just w/pos weights
C	DOVAR	LOG	input	Write Gaussfit style variances?
C
C	The format is primarily designed to be used with the curvefitting
C	program GaussFit, but it is a simple ASCII interchange format that
C	should be understood by most spreadsheet type programs
C
C	Note that it will not deal with non-zero rotions correctly.
C
C Audit trail:
C	Original version:  Cloned from FILEXLPU.  Something of a hack.
C				D.S.Briggs	May 17 1993
C	Added DOVAR option
C				D.S.Briggs	28 May 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISEXLPU')
C
      CHARACTER*(*)	VIS, EXLFILE, CLASS
      LOGICAL		DOALL, DOVAR
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NVIS, I,
     $   		VISADD, WTADD, UADD, VADD, WADD, BADD, TADD
      CHARACTER		TAB*1, CRDFILE*(SYSMXNAM), NUMBUF*20
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      DOUBLE PRECISION	RVAL(SYSMXDIM)
C
      INTEGER		DATADD, STRSTART, STRLEN, ARRNPIX
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C==================================================================
      IF (ERROR) GO TO 999
C
      VISADD = DATADD (STRM3(VIS,CLASS,'VIS'))
      WTADD = DATADD (STRM3(VIS,CLASS,'WT'))
      UADD = DATADD (STRM2(VIS,'UU'))
      VADD = DATADD (STRM2(VIS,'VV'))
      WADD = DATADD (STRM2(VIS,'WW'))
      BADD = DATADD (STRM2(VIS,'BASELINE'))
      TADD = DATADD (STRM2(VIS,'TIME'))
C
      CRDFILE = EXLFILE
      CALL STRAPPEN (CRDFILE, '.CRD')
      CALL CRDGET (STRM2(VIS,CLASS), NAX, TYPE, NAXIS, RVAL, RPIX,
     $   DELT, ROTA)
      TAB = CHAR(9)
      NVIS = ARRNPIX(STRM2(VIS,'UU'))
C
C Write the Excel file itself
C
      CALL FILDEL (EXLFILE)
      MESSAGE = 'Opening ' // EXLFILE(1:STRLEN(EXLFILE)) //
     $   ' as Excel file'
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('Excel', EXLFILE, 'WRITE')
      IF (ERROR) GO TO 990
      IF (DOVAR) THEN
         MESSAGE = 'ANT1'//TAB//'ANT2'//TAB//'VISR'//TAB//'VISI'//TAB//
     $      'WT'//TAB//'VISR_VISR'//TAB//'VISI_VISI'//TAB//
     $      'UU'//TAB//'VV'//TAB//'WW'//TAB//'TIME'
         CALL TXTWRITE ('Excel', MESSAGE)
         MESSAGE = 'double'//TAB//'double'//TAB//'double'//TAB//
     $      'double'//TAB//'double'//TAB//'double'//TAB//'double'
     $      //TAB//'double'//TAB//'double'//TAB//'double'//TAB//
     $      'double'
         CALL TXTWRITE ('Excel', MESSAGE)
      ELSE
         MESSAGE = 'ANT1'//TAB//'ANT2'//TAB//'VISR'//TAB//'VISI'//TAB//
     $      'WT'//TAB//'UU'//TAB//'VV'//TAB//'WW'//TAB//'TIME'
         CALL TXTWRITE ('Excel', MESSAGE)
         MESSAGE = 'double'//TAB//'double'//TAB//'double'//TAB//
     $      'double'//TAB//'double'//TAB//'double'//TAB//'double'
     $      //TAB//'double'//TAB//'double'
         CALL TXTWRITE ('Excel', MESSAGE)
      END IF
      CALL PIXVISXL (MEMX(VISADD), MEMR(WTADD), MEMR(UADD),
     $   MEMR(VADD), MEMR(WADD), MEMR(BADD), MEMR(TADD), NVIS,
     $   'Excel', DOALL, DOVAR)
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
