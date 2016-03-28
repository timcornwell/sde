C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filav2pu.f	1.2	 7/18/97
C
      SUBROUTINE FILAV2PU (INDIR, OUTFILE)
C
CD Writes out an accumulated style Allan Variance file. (horizontal)
C
C	INDIR	CHAR*(*)	in	Directory with Allan Variance Infor
C	OUTFILE	CHAR*(*)	in	Output File (can exist already)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 10 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INDIR, OUTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILAV2PU')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), AVADD, RMSADD, TADD, I
      CHARACTER*1	ATYPE
      CHARACTER*132	LINE
      REAL		D(9)
      REAL		WSPEED, WDIR, TEMP, OPAC
      INTEGER		IDATE, ITIME
C
      CHARACTER*(SYSMXNAM)	STRM2
      DATA			D /9*0.0/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2 (INDIR, 'RMS'), NAX, NAXIS, ATYPE, RMSADD)
      CALL DATGETAR (STRM2 (INDIR, 'ALVARIANCE'), NAX, NAXIS, ATYPE,
     $   AVADD)
      CALL DATGETAR (STRM2 (INDIR, 'TINT'), NAX, NAXIS, ATYPE,
     $   TADD)
C
      IF (ERROR) GOTO 990
      CALL MSGPUT ('Writing Accumulated Allan Variance file '
     $   //OUTFILE, 'I')
C
      CALL TXTOPEN (ROUTINE, OUTFILE, 'WRITE')
C
      DO 200 I = 1, MIN (9, NAXIS(1))
         D(I) = MEMR(AVADD +  I - 1)
 200  CONTINUE
      IDATE = 990101
      ITIME = 1
      OPAC  = .06
      TEMP = 0.0
      WSPEED = -15.0
      WDIR   = -15.0
      WRITE (LINE, 300) IDATE, ITIME, OPAC, TEMP, WSPEED, WDIR,
     $   (D(I), I = 1, 9)
 300  FORMAT (I6, I4, F8.3, F7.2, F9.2, F8.2, 9(F8.3, 2X))
C
      CALL TXTWRITE (ROUTINE, LINE)
C
      CALL TXTCLOSE (ROUTINE)
C      
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
