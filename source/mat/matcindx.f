C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)matcindx.f	1.1    6/7/93
C
      SUBROUTINE MATCINDX (INDEX, NAME)
CD     
C    Re-order the columns of a matrix using the index array to specify how
C    the re-ordering is to be done.
C    
C       INDEX          CH*(*)  input    Directory name of index array
C       NAME           CH*(*)  input    Directory name to be sorted
C     
C    Audit trail:
C    Cloned from ARRINDX
C					D.S.Briggs	16 Nov 1992
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	NAME, INDEX
C     
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='MATCINDX')
C     
C     Local variables
C
      CHARACTER*(SYSMXNAM) SCRNAME
      CHARACTER*(1) 	TYPE, ATYPE
      INTEGER 		NAX, RNAX, NAXIS(SYSMXDIM)
      INTEGER 		NADD, IADD, SCRADD
      INTEGER           I, SIZE, NCOLS
C
      INTEGER		CRDRNAX, DATADD
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C     
C     Get index array attributes
C     
      CALL DATGETAR (INDEX, NAX, NAXIS, TYPE, IADD)
      IF (TYPE.NE.'I') THEN
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data type not Integer')
         GOTO 999
      END IF
      IF(NAX.NE.1)THEN
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $      'Array not one dimensional')
         GOTO 999
      END IF
      NCOLS = NAXIS(1)
C     
C     Get attributes from array to be sorted
C     
      CALL DATGETAR (NAME, NAX, NAXIS, TYPE, NADD)
      RNAX = CRDRNAX(NAX,NAXIS)
      IF ((TYPE.EQ.'R').OR.
     $    (TYPE.EQ.'I').OR.
     $    (TYPE.EQ.'D').OR.
     $    (TYPE.EQ.'X').OR.
     $    (TYPE.EQ.'L')) THEN
         ATYPE = TYPE
         IF ((RNAX.GT.2).OR.(NAX.LE.1)) THEN
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'Matrix has bad dimensions')
            GOTO 999
         END IF
         IF (NCOLS.NE.NAXIS(2)) THEN
            CALL ERRREPOR(ERRWRGTP, ROUTINE, 
     $         'Index array is not of the same'//
     $         ' size as data array')
            GOTO 999
         END IF
         SIZE = NAXIS(1)
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data type '//TYPE//' not suported')
         GOTO 999
      END IF
C     
C     Check sizes
C     
      SIZE = MAX(0,SIZE)
      IF (SIZE.LE.0) THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'No Data points to index')
         GOTO 999
      END IF
      NCOLS = MAX(1,NCOLS)
C
C Create a scratch array
C
      SCRNAME = STRM2(NAME, 'SCRATCH')
      CALL ARRCOPY (NAME, SCRNAME)
      SCRADD = DATADD (SCRNAME)
      IF (ERROR) GOTO 999
C
C Do the indexing
C
      DO 100 I = 1, NCOLS
         IF (ATYPE.EQ.'R') THEN
            CALL PIXRCOPY (MEMR(SCRADD+SIZE*(MEMI(IADD+I-1)-1)), 0, 1,
     $         MEMR(NADD+SIZE*(I-1)), 0, 1, SIZE)
         ELSE IF (TYPE.EQ.'I') THEN
            CALL PIXICOPY (MEMI(SCRADD+SIZE*(MEMI(IADD+I-1)-1)), 1,
     $         MEMI(NADD+SIZE*(I-1)), 1, SIZE)
         ELSE IF (TYPE.EQ.'D') THEN
            CALL PIXDCOPY (MEMD(SCRADD+SIZE*(MEMI(IADD+I-1)-1)), 1,
     $         MEMD(NADD+SIZE*(I-1)), 1, SIZE)
         ELSE IF (TYPE.EQ.'X') THEN
            CALL PIXXCOPY (MEMX(SCRADD+SIZE*(MEMI(IADD+I-1)-1)), 1,
     $         MEMX(NADD+SIZE*(I-1)), 1, SIZE)
         ELSE IF (TYPE.EQ.'L') THEN
            CALL PIXLCOPY (MEML(SCRADD+SIZE*(MEMI(IADD+I-1)-1)), 1,
     $         MEML(NADD+SIZE*(I-1)), 1, SIZE)
         END IF
 100  CONTINUE
C
C delete the scratch array
C
      CALL DATDELAR(SCRNAME)
C     
C     Trace errors
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
