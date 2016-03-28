C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filalvpu.f	1.2    7/18/97
C
      SUBROUTINE FILALVPU (INDIR, OUTFILE)
C
CD Writes out a Frazier Owen style Allan Variance file.
C
C	INDIR	CHAR*(*)	in	Directory with Allan Variance Infor
C	OUTFILE	CHAR*(*)	in	Output File
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	June 25 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INDIR, OUTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILAVPU')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), AVADD, RMSADD, TADD,
     $   		UVADD, I
      CHARACTER*1	ATYPE
      LOGICAL		UVINFO
      CHARACTER*(SYSMXNAM)	LINE
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL			DATEXIST
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
      CALL MSGPUT ('Writing Allan Variance file '//OUTFILE, 'I')
C
      CALL TXTOPEN (ROUTINE, OUTFILE, 'WRITE')
      LINE = 'Calibration Run'
      CALL TXTWRITE (ROUTINE, LINE)
      LINE = '0000'
      CALL TXTWRITE (ROUTINE, LINE)
C
      DO 200 I = 0, NAXIS(1)-1
         IF (MEMI(TADD+I) .NE. 0) THEN
            WRITE (LINE, 201) MEMR(RMSADD + I), MEMR(AVADD+I), 
     $         MEMI(TADD+I)
            CALL TXTWRITE (ROUTINE, LINE)
         ENDIF
 200  CONTINUE
 201  FORMAT (F14.7, F9.4, I4)
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
