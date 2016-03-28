C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filexlge.f	1.1    12/11/92
C
C Audit trail:
      SUBROUTINE FILEXLGE (IMAGE, EXLFILE, DOFULL)
C
CD Get image from 'Excel' file
C
C	IMAGE	CH*(*)	input	Name of image
C	EXLFILE	CH*(*)	input	Name of input file
C	DOFULL	LOG	input	Do full pixels?
C
C	The format is primarily designed to be used with the curvefitting
C	program GaussFit, but it is a simple ASCII interchange format that
C	should be understood by most spreadsheet type programs
C
C	Note that it will not deal with non-zero rotions correctly.
C
C	If DOFULL is true, it is assumed that all the pixels will be
C	read in by a later model-dependent subroutine, and consequently
C	the pixels values in the Excel file are not read.  The file
C	<EXCEL>.CRD is required, however, to generate the initial blank
C	image.  IMAGE must not exist previously.
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 12 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILEXLGE')
C
      CHARACTER*(*)	IMAGE, EXLFILE
      LOGICAL		DOFULL
C
      INTEGER		NAX, NAXIS(SYSMXDIM), IADD, IAX, L, T1, T2,
     $   		I, J
      CHARACTER		TAB*1, CRDFILE*(SYSMXNAM)
      CHARACTER*8	TYPE(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM),
     $   		IMG, RTMP
      DOUBLE PRECISION	RVAL(SYSMXDIM)
      LOGICAL		EOF
C
      INTEGER		CRDRNAX, STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      CRDFILE = EXLFILE
      CALL STRAPPEN (CRDFILE, '.CRD')
      TAB = CHAR(9)
      NAX = 0
C
      DO 10 IAX = 1, SYSMXDIM
         TYPE(IAX) = ' '
         RVAL(IAX) = 0.0D0
         RPIX(IAX) = 0.0
         DELT(IAX) = 0.0
         ROTA(IAX) = 0.0
         NAXIS(IAX) = 1
 10   CONTINUE
C
      MESSAGE = 'Opening Excel Coordinate file ' //
     $   CRDFILE(1:STRLEN(CRDFILE))
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('ExcelCRD', CRDFILE, 'READ')
      IF (ERROR) GO TO 990
      CALL TXTREAD ('ExcelCRD', STRBUF, L, EOF)
      CALL TXTREAD ('ExcelCRD', STRBUF, L, EOF)
C
 100  CONTINUE
      CALL TXTREAD ('ExcelCRD', STRBUF, L, EOF)
      IF (EOF) GO TO 200
      T1 = INDEX(STRBUF,TAB)
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(1:T1-1),*) RTMP
      IAX = NINT(RTMP)
      NAX = MAX(NAX,IAX)
      TYPE(IAX) = STRBUF(T1+1:T2-1)
      T1 = T2
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(T1+1:T2-1),*) RTMP
      NAXIS(IAX) = NINT(RTMP)
      T1 = T2
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(T1+1:T2-1),*) RVAL(IAX)
      T1 = T2
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(T1+1:T2-1),*) DELT(IAX)
      T1 = T2
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(T1+1:T2-1),*) RPIX(IAX)
      T1 = T2
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      IF (T2.GT.T1) THEN
         READ (STRBUF(T1+1:T2-1),*) ROTA(IAX)
      ELSE
         READ (STRBUF(T1+1:),*) ROTA(IAX)
      END IF
      GO TO 100
C
 200  CONTINUE
      CALL TXTCLOSE ('ExcelCRD')
C
C Now actually make the image
C
      CALL DATCREAT (IMAGE)
      CALL DATMAKAR (IMAGE, NAX, NAXIS, 'R', IADD)
      CALL ARRSETCO (IMAGE, 0.0, 0.0)
      CALL CRDPUT (IMAGE, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) THEN
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t make image')
         GO TO 999
      END IF
      MESSAGE = 'Coordinate system for ' // IMAGE(1:STRLEN(IMAGE))
      CALL MSGPUT (MESSAGE, 'I')
      CALL CRDLIST (IMAGE)
C
C Read in the pixels from the Excel file itself, if needed.
C
      IF (DOFULL) GO TO 990
C
C Check for 2-D limitation
C
      IF (CRDRNAX(NAX,NAXIS).GT.2) THEN
         CALL MSGPUT (
     $      'Warning!  Only 1 & 2-D Excel files implemented', 'E')
         GO TO 990
      END IF
C
      MESSAGE = 'Opening Excel pixel file ' //
     $   EXLFILE(1:STRLEN(EXLFILE))
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN ('Excel', EXLFILE, 'READ')
      CALL TXTREAD ('Excel', STRBUF, L, EOF)
      CALL TXTREAD ('Excel', STRBUF, L, EOF)
C
 300  CONTINUE
      CALL TXTREAD ('Excel', STRBUF, L, EOF)
      IF (EOF) GO TO 400
      T1 = INDEX(STRBUF,TAB)
      READ (STRBUF(1:T1-1),*) RTMP
      I = NINT(RTMP)
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      READ (STRBUF(T1+1:T2-1),*) RTMP
      J = NINT(RTMP)
      T1 = INDEX(STRBUF(T2+1:),TAB) + T2
      T1 = INDEX(STRBUF(T1+1:),TAB) + T1
      T2 = INDEX(STRBUF(T1+1:),TAB) + T1
      IF (T2.GT.T1) THEN
         READ (STRBUF(T1+1:T2-1),*) IMG
      ELSE
         READ (STRBUF(T1+1:),*) IMG
      END IF
      MEMR(IADD + (J-1)*NAXIS(1)+I-1) = IMG
      GO TO 300
C
 400  CONTINUE
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
