C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsort.f	1.5    12/11/92
C
      SUBROUTINE ARRSORT (NAME, INDEX)
CD     
C    Generate an index describing how to sort the array NAME.
C    This can then be used with ARRINDX to sort the array.
C    
C       NAME           CH*(*)  input    Directory name to be sorted
C	NAME/ALGORITHM CH*(*)  input    QUICK or HEAP
C	NAME/DIRECTION CH*(*)  input    ASCENDING or DESCENDING
C       INDEX          CH*(*)  input    Directory name of index array
C
C	Quicksort is the default, and will generally be faster.  Heapsort
C	may be faster in some worst case situations, and preserves
C	the order of elements that are equal.  Quicksort does not.
C	
C	Audit trail:
C	Cloned from arrscat                                  
C                                       R.G Marson     Feb 13, 1990
C	Code added to create index array if needed, and bugs fixed
C	for types D & I. ALGORITHM & DIRECTION keywords taken as options
C	from NAME
C					D.S.Briggs	Nov 16 1992
C----------------------------------------------------------------------
#include	"stdinc.h"
C     
      CHARACTER*(*) 	NAME, INDEX
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSORT')
C     
      CHARACTER*(1) 	TYPE, ATYPE, ALG
      INTEGER 		NAX, NAXIS(SYSMXDIM)
      INTEGER 		NADD, IADD
      INTEGER           SIZE, DIR, NDUMMY
C
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
      IF (ERROR) GO TO 999
C     
C     Get inpout array attributes
C     
      CALL DATGETAR (NAME, NAX, NAXIS, TYPE, NADD)
      IF ((TYPE.EQ.'R').OR.
     $    (TYPE.EQ.'I').OR.
     $    (TYPE.EQ.'D')) THEN
         ATYPE = TYPE
         IF(NAX.EQ.1)THEN
            SIZE = NAXIS(1)
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $           'Array not one dimensional')
            GOTO 999
         END IF
      ELSE
         CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $        'Data type not Real or Integer or Double')
         GOTO 999
      END IF
C
C options
C
      DIR = 1
      ALG = 'Q'
      IF (DATEXIST(STRM2(NAME,'ALGORITHM'))) THEN
         CALL DATGETC (NAME, 'ALGORITHM', STRBUF, 1, NDUMMY)
         IF (STRBUF.EQ.'QUICK') THEN
            ALG = 'Q'
         ELSE IF (STRBUF.EQ.'HEAP') THEN
            ALG = 'H'
         ELSE
            CALL MSGPUT ('Unrecognized sorting algorithm.' //
     $         '  Using QUICK','W')
         END IF
      END IF
      IF (DATEXIST(STRM2(NAME,'DIRECTION'))) THEN
         CALL DATGETC (NAME, 'DIRECTION', STRBUF, 1, NDUMMY)
         IF (STRBUF.EQ.'ASCENDING') THEN
            DIR = 1
         ELSE IF (STRBUF.EQ.'DESCENDING') THEN
            DIR = -1
         ELSE
            CALL MSGPUT ('Unrecognized sorting direction.' //
     $         '  Using ASCENDING','W')
         END IF
      END IF
C     
C     Get index array attributes
C
      IF (DATEXIST(INDEX)) THEN
         CALL DATGETAR (INDEX, NAX, NAXIS, TYPE, IADD)
         IF (TYPE.EQ.'I') THEN
            IF(NAX.EQ.1)THEN
               IF (SIZE.NE.NAXIS(1)) THEN
                  CALL ERRREPOR(ERRWRGTP, ROUTINE, 
     $               'Index array is not of the same'//
     $               ' size as data array')
                  GOTO 999
               END IF
            ELSE
               CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $            'Array not one dimensional')
               GOTO 999
            END IF
         ELSE
            CALL ERRREPOR(ERRWRGTP, ROUTINE,
     $         'Data type not Integer')
            GOTO 999
         END IF
      ELSE
         TYPE = 'I'
         CALL DATMAKAR (INDEX, NAX, NAXIS, TYPE, IADD)
      END IF
C     
C     Check sizes
C     
      SIZE = MAX(0,SIZE)
      IF(SIZE .LE. 0)THEN
         CALL ERRREPOR(ERRNTFND, ROUTINE,
     $        'No Data points to sort')
         GOTO 999
      END IF
C
C Do the sorting
C
      IF (ALG.EQ.'Q') THEN
         IF (ATYPE.EQ.'R') THEN
            CALL SORTR (MEMR(NADD), SIZE, DIR, MEMI(IADD)) 
         ELSE IF (ATYPE.EQ.'I') THEN
            CALL SORTI (MEMI(NADD), SIZE, DIR, MEMI(IADD))
         ELSE IF (ATYPE.EQ.'D') THEN
            CALL SORTD (MEMD(NADD), SIZE, DIR, MEMI(IADD))
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE,
     $         'quicksort not implemented for data type')
         END IF
      ELSE IF (ALG.EQ.'H') THEN
         IF (ATYPE.EQ.'R') THEN
            CALL UTLRHSRT (MEMR(NADD), SIZE, DIR, MEMI(IADD))
         ELSE IF (ATYPE.EQ.'D') THEN
            CALL UTLDHSRT (MEMD(NADD), SIZE, DIR, MEMI(IADD))
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE,
     $         'heapsort not implemented for data type')
         END IF
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Bad algorithm selector')
         GO TO 999
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
