C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filgetms.f	1.3    11/7/90
C
      SUBROUTINE FILGETMS (MODEL, MODFILE)
C
CD Read a Conway 'Spectral Index' model from an ASCII
C
C	MODEL/FLUX	REAL	Flux of component (Jy)
C	MODEL/RADIUS	REAL	Distance of Component from field Centre (masec)
C	MODEL/AZ	REAL	Angle of component from ??? (Deg)
C	MODEL/BMAJ	REAL	Major axis of Component (masec)
C	MODEL/AXRAT	REAL	Axial Ratio 
C	MODEL/BPA	REAL	Position angle (deg)
C       MODEL/SI        REAL    Spectral index
C
C	MODEL	CH*(*)	input	Name of directory entry
C	MODFILE	CH*(*)	input	Name of input file
C Audit trail:
C	Cloned from filgetmo
C				R.G. Marson	Nov 5 1990
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	MODEL, MODFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILGETMS')
C
      INTEGER		NCOMP, ICOMP, FLADD, RADADD, AZADD, 
     1			siadd, BMAJADD, AXADD, BPAADD, I, NCHAR
      LOGICAL		EOF
      CHARACTER*(SYSMXNAM)	STRM2
      CHARACTER*132	LINE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find number of components
C
      NCOMP = 0
      CALL TXTOPEN (ROUTINE, MODFILE, 'READ')
      IF (ERROR) GO TO 990
    1 CONTINUE
      CALL TXTREAD (ROUTINE, LINE, NCHAR, EOF)
      IF (ERROR) GO TO 990
      IF (EOF) GO TO 2      
      IF (NCHAR.EQ.0) GO TO 1
      NCOMP = NCOMP + 1
      GO TO 1
    2 CONTINUE
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
      CALL DATMAKAR (STRM2(MODEL, 'RADIUS'), 1, NCOMP, 'R', RADADD)
      CALL DATMAKAR (STRM2(MODEL, 'AZ'), 1, NCOMP, 'R', AZADD)
      CALL DATMAKAR (STRM2(MODEL, 'BMAJ'), 1, NCOMP, 'R', BMAJADD)
      CALL DATMAKAR (STRM2(MODEL, 'AXRAT'), 1, NCOMP, 'R', AXADD)
      CALL DATMAKAR (STRM2(MODEL, 'BPA'), 1, NCOMP, 'R', BPAADD)
      CALL DATMAKAR (STRM2(MODEL, 'SI'), 1, NCOMP, 'R', SIADD)
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
         READ (LINE, *, ERR = 200) MEMR(FLADD+I), MEMR(RADADD+I),
     1      MEMR(AZADD+I), MEMR(BMAJADD+I), MEMR(AXADD+I), 
     2      MEMR(BPAADD+I), MEMR(SIADD+I)
  100 CONTINUE
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
