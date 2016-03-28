C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgetmo.f	1.5    12/3/94
C
      SUBROUTINE FILGETMO (MODEL, MODFILE)
C
CD Read a model from an ASCII file
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	MODEL/VEL	REAL	Velocity magnitude in mas/day
C	MODEL/VPA	REAL	Velocity position angle in degrees
C
C	MODEL	CH*(*)	input	Name of directory entry
C	MODFILE	CH*(*)	input	Name of input file
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added time variable models.  The time origin in set by the program
C	which uses the model.  If the time variable parameters are not
C	found, the routine simply does not create the associated arrays.
C	Added commenting.  If the first character of the line is '#', it
C	will be ignored.
C				D.S.Briggs	Nov 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, MODFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGETMO')
C
      INTEGER		NCOMP, ICOMP, FLADD, RAADD, DECADD, 
     $			TADD, BMAJADD, BMINADD, BPAADD, VADD, VPADD,
     $   		I, NCHAR
      REAL		RDUMMY
      LOGICAL		EOF, DOTIMEV, CHKTYPE
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*256	LINE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find number of components
C
      NCOMP = 0
      CHKTYPE = .TRUE.
      CALL TXTOPEN (ROUTINE, MODFILE, 'READ')
      IF (ERROR) GO TO 990
    1 CONTINUE
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 50
      IF (LINE(1:1).EQ.'#') GO TO 1
      IF (NCHAR.EQ.0) GO TO 1
      IF (CHKTYPE) THEN
         READ (LINE, *, ERR=10, END=10) RDUMMY, RDUMMY, RDUMMY, RDUMMY,
     $      RDUMMY, RDUMMY, STRBUF, RDUMMY, RDUMMY
         DOTIMEV = .TRUE.
         GO TO 40
 10      CONTINUE
         READ (LINE, *, ERR=20) RDUMMY, RDUMMY, RDUMMY, RDUMMY,
     $      RDUMMY, RDUMMY, STRBUF
         DOTIMEV = .FALSE.
         GO TO 40
 20      CONTINUE
         CALL ERRREPOR (ERRFATAL, ROUTINE, 'Can''t parse model file')
         GO TO 999
 40   CONTINUE
      CHKTYPE = .FALSE.
      END IF
      NCOMP = NCOMP + 1
      GO TO 1
 50   CONTINUE
      CALL TXTCLOSE (ROUTINE)      
      IF (ERROR) GO TO 990
      WRITE (MESSAGE, 1000) NCOMP
 1000 FORMAT ('Found ',I6,' components')
      CALL MSGPUT (MESSAGE, 'I')
C
C Now make directory
C
      CALL DATCREAT (MODEL)
      CALL DATMAKAR (STRM2(MODEL, 'FLUX'), 1, NCOMP, 'R', FLADD)
      CALL DATMAKAR (STRM2(MODEL, 'RA'), 1, NCOMP, 'R', RAADD)
      CALL DATMAKAR (STRM2(MODEL, 'DEC'), 1, NCOMP, 'R', DECADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMIN'), 1, NCOMP, 'R', BMINADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'TYPE'), 1, NCOMP, 'S', TADD)
      IF (DOTIMEV) THEN
         CALL DATMAKAR (STRM2(MODEL, 'VEL'), 1, NCOMP, 'R', VADD)
         CALL DATMAKAR (STRM2(MODEL, 'VPA'), 1, NCOMP, 'R', VPADD)
      END IF
C
C Now read file
C
      CALL TXTOPEN (ROUTINE, MODFILE, 'READ')
      IF (ERROR) GO TO 990
      DO 100 ICOMP = 1, NCOMP
         CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
         IF (NCHAR.EQ.0) GO TO 100
         IF (EOF) THEN
            CALL ERRREPOR (ERRINPUT, ROUTINE, 'Premature EOF')
            GO TO 999
         END IF
         I = ICOMP - 1
         IF (DOTIMEV) THEN
            READ (LINE, *, ERR = 200) MEMR(FLADD+I), MEMR(RAADD+I),
     1         MEMR(DECADD+I), MEMR(BMAJADD+I), MEMR(BMINADD+I), 
     2         MEMR(BPAADD+I), MEMC(TADD+I), MEMR(VADD+I), MEMR(VPADD+I)
         ELSE
            READ (LINE, *, ERR = 200) MEMR(FLADD+I), MEMR(RAADD+I),
     1         MEMR(DECADD+I), MEMR(BMAJADD+I), MEMR(BMINADD+I), 
     2         MEMR(BPAADD+I), MEMC(TADD+I)
         END IF
 100  CONTINUE
      CALL TXTCLOSE(ROUTINE)
      GO TO 300
  200 CONTINUE
      CALL ERRREPOR (ERRINPUT, ROUTINE, 'Error in line '//LINE)
      GO TO 999
  300 CONTINUE
      CALL MSGPUT ('Read model', 'I')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
