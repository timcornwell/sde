C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgfmge.f	1.1    6/9/93
C
C Audit trail:
      SUBROUTINE FILGFMGE (MODEL, GAUSFILE)
C
CD Get model from Gaussfit parameter file (Excel format)
C
C	MODEL	CH*(*)	input	Name of model
C	GAUSFILE CH*(*)	input	Name of Gaussfit parameter file
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
C				D.S.Briggs	April 15 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGFMGE')
C
      CHARACTER*(*)	MODEL, GAUSFILE
C
      INTEGER		NCOMP, ICOMP, FLADD, RAADD, DECADD,
     $   		TADD, BMAJADD, BMINADD, BPAADD, NCHAR, T1, T2
      CHARACTER		TAB*1
      LOGICAL		EOF
C
      CHARACTER*(SYSMXNAM)	STRM2
C==================================================================
      IF (ERROR) GO TO 999
C
      TAB = CHAR(9)
      NCOMP = 0
      CALL TXTOPEN (ROUTINE, GAUSFILE, 'READ')
      IF (ERROR) GO TO 990
    1 CONTINUE
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 2      
      IF (NCHAR.EQ.0) GO TO 1
      NCOMP = NCOMP + 1
      GO TO 1
    2 CONTINUE
      NCOMP = NCOMP - 2
      CALL TXTCLOSE (ROUTINE)      
      IF (ERROR) GO TO 990
      WRITE (MESSAGE, 1000) NCOMP
 1000 FORMAT ('Found ',I6,' components')
      CALL MSGPUT (MESSAGE, 'I')
C
C Make directory
C
      CALL DATCREAT (MODEL)
      CALL DATMAKAR (STRM2(MODEL, 'FLUX'), 1, NCOMP, 'R', FLADD)
      CALL DATMAKAR (STRM2(MODEL, 'RA'), 1, NCOMP, 'R', RAADD)
      CALL DATMAKAR (STRM2(MODEL, 'DEC'), 1, NCOMP, 'R', DECADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMIN'), 1, NCOMP, 'R', BMINADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'TYPE'), 1, NCOMP, 'S', TADD)
C
C Now read file
C
      CALL TXTOPEN (ROUTINE, GAUSFILE, 'READ')
      IF (ERROR) GO TO 990
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
      CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
C
      DO 100 ICOMP = 1, NCOMP
         CALL TXTREAD (ROUTINE, STRBUF, NCHAR, EOF)
         IF (NCHAR.EQ.0) GO TO 100
         IF (EOF) THEN
            CALL ERRREPOR (ERRINPUT, ROUTINE, 'Premature EOF')
            GO TO 999
         END IF
C
         T1 = INDEX(STRBUF,TAB)
         T1 = INDEX(STRBUF(T1+1:),TAB) + T1
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(FLADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(RAADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(DECADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(BMAJADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(BMINADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         READ (STRBUF(T1+1:T2-1),*) MEMR(BPAADD+ICOMP-1)
         T1 = T2
         T2 = INDEX(STRBUF(T1+1:),TAB) + T1
         MEMC(TADD+ICOMP-1) = STRBUF(T1+1:T2-1)
C
  100 CONTINUE
      CALL TXTCLOSE(ROUTINE)
      GO TO 300
  200 CONTINUE
      CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//STRBUF)
      GO TO 999
  300 CONTINUE
      CALL MSGPUT ('Read model', 'I')
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
