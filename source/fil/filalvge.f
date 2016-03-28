C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filalvge.f	1.2    7/18/97
C
      SUBROUTINE FILALVGE (ALVAR, ALVFILE)
C
CD Reads in a Frazier Owen style Allan Variance file.
C
C	ALVAR	CHAR*(*)	in	Directory with Allan Variance Infor
C	ALVFILE	CHAR*(*)	in	Output File
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
      CHARACTER*(*)	ALVAR, ALVFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILALVGE')
C
      INTEGER		 AVADD, RMSADD, TADD,
     $   		I, NALV, NCHAR
      LOGICAL		EOF
      CHARACTER*(SYSMXNAM)	LINE
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NALV = 0
      CALL TXTOPEN (ROUTINE, ALVFILE, 'READ')
      IF (ERROR) GO TO 990
    1 CONTINUE
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 2
      IF (NCHAR.EQ.0) GO TO 1
      NALV = NALV + 1
      GO TO 1
    2 CONTINUE
      CALL TXTCLOSE (ROUTINE)
      IF (ERROR) GO TO 990
      NALV = NALV - 2
      WRITE (MESSAGE, 1000) NALV
 1000 FORMAT ('Found ',I6,' Allan Variances')
      CALL MSGPUT (MESSAGE, 'I')
C
      CALL DATCREAT (ALVAR)
      CALL DATMAKAR (STRM2 (ALVAR, 'RMS'), 1, NALV, 'R', RMSADD)
      CALL DATMAKAR (STRM2 (ALVAR, 'ALVARIANCE'), 1, NALV, 'R', AVADD)
      CALL DATMAKAR (STRM2 (ALVAR, 'TINT'), 1, NALV, 'I', TADD)
C
      IF (ERROR) GOTO 990
C
      CALL TXTOPEN (ROUTINE, ALVFILE, 'READ')
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
C
      DO 200 I = 0, NALV-1
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF (NCHAR.EQ.0) GOTO 200
         IF (EOF) THEN
            CALL ERRREPOR (ERRINPUT, ROUTINE, 'Premature EOF')
            GO TO 999
         ENDIF
         READ (LINE, *, ERR = 300) MEMR(RMSADD + I), MEMR(AVADD+I), 
     $      MEMI(TADD+I)
 200  CONTINUE
      CALL TXTCLOSE (ROUTINE)
      GOTO 400
 300  CONTINUE
      CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//LINE)
      GO TO 999
 400  CONTINUE
      CALL MSGPUT ('Read Allan Variance file '//ALVFILE, 'I')
C      
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
