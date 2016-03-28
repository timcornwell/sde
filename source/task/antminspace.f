C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)antminspace.f	1.1	19 Oct 1995
C
      SUBROUTINE SDEMAIN
C
CD Program for non-interactive, ring like array design
C
C Audit trail:
C	Original version
C					M.A.Holdaway	Oct 19 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ANTMINSPACE')
C
      CHARACTER*(SYSMXNAM)	ANTFILE, NANTFILE
      REAL		MINSPACE, DR, DTHETA
      INTEGER			NDUMMY
      CHARACTER*132     LINE
      REAL		X(200), Y(200), Z(200), R(200),DIAM
      REAL		AC2M, AD2M, XAVE, YAVE, RMIN
      REAL		SITELAT, SITELONG
      INTEGER		KANT(200), NANT, IANT, JANT, NCHAR
      LOGICAL		EOF
C
      INTEGER		STRLEN
CC==================================================================
C
      CALL MSGWELCO ('I enforce minimum spacing requirements on arrays')
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC('Antfile', ANTFILE, 1, NDUMMY)
      CALL USRGETC('NAntfile', NANTFILE, 1, NDUMMY)
      CALL USRGETR('Minspace', MINSPACE, 1, NDUMMY)
      CALL USRGETC('Dr', DR, 1, NDUMMY)
      CALL USRGETC('Dtheta', DTHETA, 1, NDUMMY)
C
C  Read in antenna file
C
      CALL TXTOPEN (ROUTINE, ANTFILE, 'READ')
 1    CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 1
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) NANT
C 
 2    CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GOTO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 2
      READ (LINE(:STRLEN(LINE)), *, ERR = 200, END=10) SITELAT, SITELONG
      GO TO 20
 10   CONTINUE
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) SITELAT
      SITELONG = 0.D0
 20   CONTINUE
C
 21   CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 990
      IF (LINE(1:1).EQ.'#') GO TO 21
      READ (LINE(:STRLEN(LINE)), *, ERR = 200) AC2M, AD2M
      JANT = 0
      DO 100 IANT= 1, NANT
 50      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF(ERROR) GOTO 990
         IF (EOF) THEN
            CALL ERRREPOR (ERRINPUT, ROUTINE, 'Premature EOF')
            GO TO 999
         END IF
         IF (NCHAR.EQ.0) GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 50
         IF (LINE(1:1).NE.'@') THEN
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $           'ANTMINSPACE works on Local files only')
         ENDIF
         JANT = JANT + 1
         READ (LINE(2:STRLEN(LINE)), *, ERR=200) X(JANT),
     $        Y(JANT), Z(JANT), DIAM
 100  CONTINUE
      NANT  = JANT
C
C Now enforce minimum spacing requirements from the center out
C    a) find center!
C
      XAVE = 0.0
      YAVE = 0.0
      DO 110 IANT = 1, NANT
         XAVE = X(IANT) + XAVE
         YAVE = Y(IANT) + YAVE
 110  CONTINUE
      XAVE = XAVE/FLOAT(NANT)
      YAVE = YAVE/FLOAT(NANT)
      RMIN = 99999999.
      DO 120 IANT = 1, NANT
         R(IANT) = SQRT( (X(IANT)- XAVE)**2 + (Y(IANT)- YAVE)**2 )
         KANT(IANT) = IANT
 120  CONTINUE
      CALL UTL2SORT (NANT, R, KANT, R, KANT)
C
C    b) now go through antennas from inside to out to make sure they
C       are not too close
C
      DO 200 IANT = 2, NANT
         CALL ANTFIDLE (IANT, NANT, KANT, X, Y, MINSPACE, DR, DTHETA)
 200  CONTINUE
C
      CALL TXTOPEN (ROUTINE, NANTFILE, 'WRITE')
      WRITE (LINE, *) NANT
      CALL TXTWRITE (ROUTINE, LINE)
      WRITE (LINE, *) SITELAT, SITELONG
      CALL TXTWRITE (ROUTINE, LINE)
      AC2M = 1.0
      AD2M = 1.0
      WRITE (LINE, *) AC2M, AD2M
      CALL TXTWRITE (ROUTINE, LINE)
      DO 700 IANT = 1, NANT
            WRITE (LINE, 800) X(IANT), Y(IANT), Z(IANT), DIAM
 800        FORMAT( '@     ',4F12.2)
            CALL TXTWRITE (ROUTINE, LINE)
 90      CONTINUE
 700  CONTINUE
      CALL TXTCLOSE(ROUTINE)
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
C
      SUBROUTINE ANTFIDLE (IANT, NANT, KANT, X, Y, MINSPACE, DR, DTHETA)
C
C Fiddles the IANT'th antenna locations X, Y (KANT is the order) such that
C no antenna lower down in the order KANT is within MINSPACE of this ant.
C DR and DTHETA are the increments for moving the IANT antenna until a valid
C position is found.
C
C
C-----------------------------------------------------------------------
C
#include                "stdinc.h"
C
      INTEGER		IANT, NANT, KANT(*)
      REAL		X(*), Y(*), MINSPACE, DR, DTHETA
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ANTFIDLE')
C
      INTEGER		IR, NR, ITHETA, NTHETA
      REAL		X0, Y0
      LOGICAL		OK
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL ANTCHECK (IANT, NANT, KANT, X, Y, MINSPACE, OK)
      IF (.NOT. OK) THEN
         NTHETA = 360/DTHETA
         NR = 500/DR
         X0 = X(KANT(IANT))
         Y0 = Y(KANT(IANT))
         DO 110 IR = 1, NR
            DO 100 ITHETA = 1, NTHETA
               X(KANT(IANT)) = X0 + (IR * DR) * 
     $              COS( ITHETA*DTHETA / 57.29)
               Y(KANT(IANT)) = Y0 + (IR * DR) * 
     $              SIN( ITHETA*DTHETA / 57.29)
               CALL ANTCHECK (IANT, NANT, KANT, X, Y, MINSPACE, OK)
               IF (OK) GOTO 200
 100        CONTINUE
 110     CONTINUE
      ENDIF
 200  CONTINUE
      CALL ANTCHECK (IANT, NANT, KANT, X, Y, MINSPACE, OK)
      IF (.NOT. OK) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     $        'Could not find a place to put antenna')
      ENDIF
C
 999  CONTINUE
      END
C
      SUBROUTINE ANTCHECK (IANT, NANT, KANT, X, Y, MINSPACE, OK)
C
C Checks the IANT'th antenna locations X, Y (KANT is the order) such that
C no antenna lower down in the order KANT is within MINSPACE of this ant.
C
C-----------------------------------------------------------------------
C
#include                "stdinc.h"
C
      INTEGER		IANT, JANT, NANT, KANT(*)
      REAL		X(*), Y(*), MINSPACE
      LOGICAL		OK
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'ANTCHECK')
C
      REAL		SPACE
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      OK = .TRUE.
      DO 100 JANT = 1, IANT-1
         SPACE = SQRT( (X(KANT(IANT)) - X(KANT(JANT)))**2 +
     $        (Y(KANT(IANT)) - Y(KANT(JANT)))**2)
         IF (SPACE .LT. MINSPACE) THEN
            OK = .FALSE.
            GOTO 200
         ENDIF
 100  CONTINUE
 200  CONTINUE
C
 999  CONTINUE
      END
