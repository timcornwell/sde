C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
      SUBROUTINE SDEMAIN
C
C Write compile time string
C
C Audit trail:
C	Removed explicit translation of SYSVER. It's done in TXTOPEN
C				T.J.Cornwell	Jan 12 1989
C	Look in SYI/SYSVER if translation fails
C				T.J. Cornwell	Feb 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SYSVER')
C
      CHARACTER*24      DT
      CHARACTER*(SYSMXNAM)	SYSVER
      INTEGER		NDUMMY
C==================================================================
C
      CALL MSGWELCO ('I write the SYSVER include file')
C
      CALL SYSGTENV ('SYSVER', SYSVER)
      IF (SYSVER.EQ.' ') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Environment variable SYSVER not defined')
         GO TO 999
      END IF
      CALL MSGPUT ('SYSVER translates to '//SYSVER, 'I')
      CALL TXTOPEN('sysver',  SYSVER, 'WRITE')
      IF (ERROR) GO TO 999
      WRITE (MESSAGE, 100)
      CALL TXTWRITE('sysver', MESSAGE)
      CALL SYSDATET (DT)
      WRITE (MESSAGE, 200) DT
      CALL TXTWRITE('sysver', MESSAGE)
      CALL TXTCLOSE('sysver')
  999 CONTINUE
C
C The following give the format of the SYSVER include file
C
 100  FORMAT ('      CHARACTER*(*) SYSVRINI')
 200  FORMAT ('      PARAMETER (SYSVRINI = ''',A,''')')
      END
