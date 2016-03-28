C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrbsch.f	1.3    11/7/90
C
      SUBROUTINE ARRBSCH (NAME, POSITION)
CD     
C    Search a sorted 1D array for the first value that equals or
C    or is greater than value. (stored as name/SEARCH).
C    Returns -1 if the whole array is less
C    than the search value. Uses a binary search
C    
C       name           CH*(*)  input   Directory name of array
C       position       INTEGER output  position of value in array
C     
C    Audit trail:
C    Cloned from arrsort                                  
C                                        R.G Marson    Feb 13, 1990
C    
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	NAME
      INTEGER           POSITION
C     
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRBSCH')
C     
C     Local variables
C
      CHARACTER*(1) 	TYPE, ATYPE
      DOUBLE PRECISION  DVALUE, DATFGETD
      REAL              RVALUE, DATFGETR
      INTEGER           IVALUE, DATFGETI
      INTEGER 		NAX, NAXIS(SYSMXDIM)
      INTEGER 		NADD
      INTEGER           SIZE
C=====================================================================
C     
C     If there is an error on entry then return immediately
C     
      IF (ERROR) GO TO 999
C     
C     Get array attributes
C     
      CALL DATGETAR (NAME, NAX, NAXIS, TYPE, NADD)
      IF(NAX.EQ.1)THEN
         SIZE = NAXIS(1)
         ATYPE = TYPE
      ELSE
         CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $        'Array not one dimensional')
         GOTO 999
      END IF
      IF ((ATYPE.EQ.'C').OR.
     $    (ATYPE.EQ.'L').OR.
     $    (ATYPE.EQ.'X')) THEN
         CALL ERRREPOR(ERRWRGTP, ROUTINE, 
     $        'Cannot search Character, Complex or Logical Arrays')
         GOTO 999
      END IF
C     
C     Check sizes
C     
      IF(SIZE .LE. 0)THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'No Data points to search')
         GOTO 999
      END IF
C
C     Get the value to search for
C     
      IF (ATYPE.EQ.'R') THEN
         RVALUE = DATFGETR(NAME,'SEARCH')
         CALL PIXRBSCH(MEMR(NADD), SIZE, RVALUE, POSITION)
      ELSE IF (ATYPE.EQ.'I') THEN
         IVALUE = DATFGETI(NAME, 'SEARCH')
         CALL PIXIBSCH(MEMI(NADD), SIZE, IVALUE, POSITION)
      ELSE IF (ATYPE.EQ.'D') THEN
         DVALUE = DATFGETD(NAME, 'SEARCH')
         CALL PIXDBSCH(MEMD(NADD), SIZE, DVALUE, POSITION)
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Unknown data type '//ATYPE)
         GOTO 999
      END IF
C     
C     Trace errors
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END

