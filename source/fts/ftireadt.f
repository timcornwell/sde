C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ftireadt.f	1.3    11/7/90
C
      SUBROUTINE FTIREADT (FTSBUFF, NROWS, NCOLS, SCOLS, ECOLS,
     1   IADD, AADD, CFMT)
C
CD Read Table columns into array locations in MEMR
C
C     IADD, AADD, CFMT)
C
C	FTSBUFF	INT	input	Input buffer of FITS cards
C	NROWS		INT	input	Number of rows to process
C	NCOLS		INT	input	Number of columns
C	SCOLS		INT(*)	input	Start position of each column
C	ECOLS		INT(*)	input	End position of each column
C	IADD		INT	input	offset to AADD
C	AADD		INT(*)	input	Array address for first element
C					of each column
C	CFMT		CH*(*)	input	Format statement for each
C					column
C Audit trail:
C	Added write of CARD if SYSDEBUG true
C				T.J.Cornwell	Jan 8 1989
C	Removed write of CARD
C				T.J. Cornwell	Jan 9 1989
C
C------------------------------------------------------------------------
#include	"stdinc.h"
#include	"ftsinc.h"
C
      INTEGER		FTSBUFF(*), NROWS, NCOLS, SCOLS(*), ECOLS(*),
     1			IADD, AADD(*)
      CHARACTER		CFMT(*)*8
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FTIREADT')
C
      INTEGER		ICOL, IROW, RSTART, LCOLS
      CHARACTER*(FTSLEN)	CARD
C========================================================================
      IF (ERROR) GO TO 999
C
      LCOLS = ECOLS(NCOLS)
      DO 20 IROW = 1, NROWS
         RSTART = (LCOLS / SYSCHINT) * (IROW - 1) + 1
         CALL STRINTCH (FTSBUFF(RSTART), CARD, LCOLS)
         DO 10 ICOL = 1, NCOLS
            READ (CARD(SCOLS(ICOL):ECOLS(ICOL)),
     1         FMT = CFMT(ICOL)(1:8), ERR = 100)
     1         MEMR(AADD(ICOL)+IADD+IROW-1)
  10     CONTINUE
  20  CONTINUE
      GO TO 990
C
C Error on read
C
  100 CONTINUE
      CALL ERRREPOR (ERRFATAL, ROUTINE, 'Error converting table')
      GO TO 999
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END 
