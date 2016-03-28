C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdaget.f	1.1	 5/4/93
C
      SUBROUTINE VSDAGET (PDT, FILE)
C
CD Read in polarization D Terms from an ASCI file
C
C	PDT	CH*(*)	inp	D Terms Database
C	FILE	CH*(*)	inp	Output Asci File
C
C Audit trail:
C	New
C				M.A.Holdaway	Sept 24 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT, FILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDAGET')
C
      INTEGER		NANT, NUMDINT, IINT, IANT
      INTEGER		DLADD, DRADD, TADD
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      REAL		D2R, DRAMP, DLAMP, DRPHA, DLPHA
      REAL		TIME
      REAL		DUMMY1, DUMMY2
      CHARACTER*132	LINE
      LOGICAL		EOF
      INTEGER		NCHAR, JANT
C
      INTEGER			STRLEN
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      D2R = ATAN(1.0)/45.0
      
      CALL TXTOPEN (ROUTINE, FILE, 'READ')
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      READ (LINE, *, ERR = 200) NANT, NUMDINT
      CALL DATCREAT (PDT)
      NAX = 2
      NAXIS(1) = NANT
      NAXIS(2) = NUMDINT
      CALL DATMAKAR (STRM2(PDT, 'DR'), NAX, NAXIS, 'X', DRADD)
      CALL DATMAKAR (STRM2(PDT, 'DL'), NAX, NAXIS, 'X', DLADD)
      CALL DATMAKAR (STRM2(PDT, 'DTIME'), 1, NUMDINT, 'R', TADD)
C
      DO 550 IINT = 1, NUMDINT
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         READ (LINE, *, ERR = 200) TIME
         MEMR(TADD + IINT-1) = TIME
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         DO 530 IANT = 1, NANT
            IF (IANT .EQ. 10 .AND. IINT .EQ. 4) THEN
               CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
               CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
            ENDIF
            CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
            READ (LINE, *, ERR=200) JANT, DRAMP, DRPHA, DUMMY1,
     $         DLAMP, DLPHA, DUMMY2
            MEMX(DRADD + NANT*(IINT-1) + IANT-1) = DRAMP *
     $         CMPLX ( COS( DRPHA * D2R ), SIN( DRPHA * D2R ))
            MEMX(DLADD + NANT*(IINT-1) + IANT-1) = DLAMP *
     $         CMPLX ( COS( DLPHA * D2R ), SIN( DLPHA * D2R ))
 530     CONTINUE
 550  CONTINUE
C
      GOTO 300
 200  CONTINUE
      CALL ERRREPOR (ROUTINE, ERRBDARG, 
     $   'Error in line '//LINE(:STRLEN(LINE)))
      GOTO 999
 300  CONTINUE
      
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



      
