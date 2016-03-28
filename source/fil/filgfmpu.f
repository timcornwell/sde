C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgfmpu.f	1.1    6/9/93
C
C Audit trail:
      SUBROUTINE FILGFMPU (MODEL, GAUSFILE)
C
CD Write model to Gaussfit parameter file (in Excel format)
C
C	MODEL	CH*(*)	input	Name of model
C	GAUSFILE CH*(*) input	Name of output file
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C	The format is primarily designed to be used with the curvefitting
C	program GaussFit, but it is a simple ASCII interchange format that
C	should be understood by most spreadsheet type programs
C
C Audit trail:
C	Original version:
C				D.S.Briggs	April 2 1992
C	Added types 'SHELL' and 'SPHERE'
C				D.S.Briggs	May 17 1993
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGFMPU')
C
      CHARACTER*(*)	MODEL, GAUSFILE
C
      CHARACTER		NUMBUF*20, TAB*1, ATYPE*1, TYPE*10
      INTEGER		I, NCOMP, FLUXADD, RAADD, DECADD, BMAJADD,
     $   		BMINADD, BPAADD, TYPEADD
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		DATADD, STRSTART, STRLEN
C==================================================================
      IF (ERROR) GO TO 999
C
      TAB = CHAR(9)
C
      CALL FILDEL (GAUSFILE)
      MESSAGE = 'Opening ' // GAUSFILE(1:STRLEN(GAUSFILE)) //
     $   ' as Gaussfit parameter file'
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN('Gaussfit', GAUSFILE, 'WRITE')
      IF (ERROR) GO TO 990
C
      MESSAGE = 'comp'//TAB//'Ncomp'//TAB//'Flux'//TAB//'dRA0'//TAB//
     $   'dDec0'//TAB//'BMaj'//TAB//'BMin'//TAB//'BPa'//TAB//'type'
     $   //TAB//'itype'//TAB//'Ratio'
      CALL TXTWRITE ('Gaussfit', MESSAGE)
      MESSAGE = 'double'//TAB//'double'//TAB//'double'//TAB//'double'//
     $   TAB//'double'//TAB//'double'//TAB//'double'//TAB//'double'//
     $   TAB//'char'//TAB//'double'//TAB//'double'
      CALL TXTWRITE ('Gaussfit', MESSAGE)
C
C Dig out the model data
C
      CALL DATGETAR (STRM2(MODEL, 'FLUX'), I, NCOMP, ATYPE, 
     1   FLUXADD)
      RAADD = DATADD (STRM2(MODEL, 'RA'))
      DECADD = DATADD (STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD (STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD (STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD (STRM2(MODEL, 'BPA'))
      TYPEADD = DATADD (STRM2(MODEL, 'TYPE'))
C
C Write it out
C
 1000 FORMAT (I12)
 1010 FORMAT (1PE20.9)
      DO 100 I = 1, NCOMP
         WRITE (NUMBUF, 1000) I
         MESSAGE = NUMBUF(STRSTART(NUMBUF):12)
         IF (I.EQ.1) THEN
            WRITE (NUMBUF, 1000) NCOMP
         ELSE
            NUMBUF = '0'
         END IF
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):12)
         WRITE (NUMBUF, 1010) MEMR(FLUXADD+I-1)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
C
         WRITE (NUMBUF, 1010) MEMR(RAADD+I-1)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) MEMR(DECADD+I-1)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
C
         WRITE (NUMBUF, 1010) MEMR(BMAJADD+I-1)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) MEMR(BMINADD+I-1)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
         WRITE (NUMBUF, 1010) MEMR(BPAADD+I-1)
         STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
C
         TYPE = MEMC(TYPEADD+I-1)
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      TYPE(1:STRLEN(TYPE))
         IF (TYPE(1:4).EQ.'POIN') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '1'
         ELSE IF (TYPE(1:4).EQ.'GAUS') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '2'
         ELSE IF (TYPE(1:4).EQ.'RECT') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '3'
         ELSE IF (TYPE(1:4).EQ.'DISK') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '4'
         ELSE IF (TYPE(1:4).EQ.'SPHE') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '5'
         ELSE IF (TYPE(1:4).EQ.'SHEL') THEN
            STRBUF = MESSAGE(1:STRLEN(MESSAGE)) // TAB // '6'
         END IF
C
         IF (ABS(MEMR(BMINADD+I-1)).LT.1.E-15) THEN
            WRITE (NUMBUF, 1010) 0.0
         ELSE
            WRITE (NUMBUF, 1010) MEMR(BMAJADD+I-1) / MEMR(BMINADD+I-1)
         END IF
         MESSAGE = STRBUF(1:STRLEN(STRBUF)) // TAB //
     $      NUMBUF(STRSTART(NUMBUF):20)
C
         CALL TXTWRITE ('Gaussfit', MESSAGE)
 100  CONTINUE
      CALL TXTCLOSE ('Gaussfit')
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
