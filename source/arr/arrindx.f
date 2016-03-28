C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrindx.f	1.3    11/7/90
C
      SUBROUTINE ARRINDX (INDEX, NAME)
CD     
C    Re-order an array using the index array to specify how the 
C    re-ordering is to be done.
C    
C       name           CH*(*)  input    Directory name to be sorted
C       index          CH*(*)  input    Directory name of index array
C     
C    Audit trail:
C    Cloned from arrsort                                  
C                                        R.G Marson    Feb 13, 1990
C    
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	NAME, INDEX
C     
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRINDX')
C     
C     Local variables
C
      CHARACTER*(SYSMXDIM) SCRNAME, STRM2
      CHARACTER*(1) 	TYPE, ATYPE
      INTEGER 		NAX, NAXIS(SYSMXDIM)
      INTEGER 		NADD, IADD, SCRADD
      INTEGER           SIZE
C=====================================================================
C     
C     If there is an error on entry then return immediately
C     
      IF (ERROR) GO TO 999
C     
C     Get index array attributes
C     
      CALL DATGETAR (INDEX, NAX, NAXIS, TYPE, IADD)
      IF (TYPE.EQ.'I') THEN
         IF(NAX.EQ.1)THEN
            SIZE = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'Array not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data type not Integer')
         GOTO 999
      END IF
C     
C     Get attributes from array to be sorted
C     
      CALL DATGETAR (NAME, NAX, NAXIS, TYPE, NADD)
      IF ((TYPE.EQ.'R').OR.
     $    (TYPE.EQ.'I').OR.
     $    (TYPE.EQ.'D').OR.
     $    (TYPE.EQ.'X').OR.
     $    (TYPE.EQ.'L')) THEN
         ATYPE = TYPE
         IF(NAX.EQ.1)THEN
            IF (SIZE.NE.NAXIS(1)) THEN
               CALL ERRREPOR(ERRWRGTP, ROUTINE, 
     $              'Index array is not of the same'//
     $              ' size as data array')
               GOTO 999
            END IF
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'Array not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data type '//TYPE//' not suported')
         GOTO 999
      END IF
C     
C     Check sizes
C     
      SIZE = MAX(0,SIZE)
      IF(SIZE .LE. 0)THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'No Data points to index')
         GOTO 999
      END IF
C
C Create a scratch array
C
      SCRNAME = STRM2(NAME, 'SCRATCH')
      NAX = 1
      NAXIS(1) = SIZE
      TYPE = ATYPE
      CALL DATMAKAR(SCRNAME, NAX, NAXIS, TYPE, SCRADD)
      IF (ERROR) GOTO 999
C
C Do the indexing
C
      IF (ATYPE.EQ.'R') THEN
         CALL PIXRINDX (MEMI(IADD), SIZE, MEMR(NADD), MEMR(SCRADD)) 
      ELSE IF (TYPE.EQ.'I') THEN
         CALL PIXIINDX (MEMI(IADD), SIZE, MEMI(NADD), MEMI(SCRADD))
      ELSE IF (TYPE.EQ.'D') THEN
         CALL PIXDINDX (MEMI(IADD), SIZE, MEMD(NADD), MEMD(SCRADD))
      ELSE IF (TYPE.EQ.'X') THEN
         CALL PIXXINDX (MEMI(IADD), SIZE, MEMX(NADD), MEMX(SCRADD))
      ELSE IF (TYPE.EQ.'L') THEN
         CALL PIXLINDX (MEMI(IADD), SIZE, MEML(NADD), MEML(SCRADD))
      END IF
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
