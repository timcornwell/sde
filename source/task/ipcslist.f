C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcslist.f	1.3    2/16/92
C
      SUBROUTINE SDEMAIN
C
CD Program to list ipcs data
C
C
C Audit trail:
C	Cloned from vislist
C				R.G. Marson     Dec 28 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)     ROUTINE
      PARAMETER	        (ROUTINE = 'IPCSLIST')
C
      CHARACTER*(SYSMXNAM) IPCSFILE, LISTFILE
      INTEGER           NDUMMY
C==================================================================
      CALL MSGWELCO ('I list ipcs data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC('IPCSfile', IPCSFILE, 1, NDUMMY)
      CALL USRGETC('Listfile', LISTFILE, 1, NDUMMY)
C
      CALL IPCSGET ('IPCS', IPCSFILE, '*', ' ')
C      CALL datdump('IPCS')
C
      CALL TXTOPEN ('Listing', LISTFILE, 'WRITE')
      CALL IPCSTYPE ('Listing', 'IPCS')
      CALL TXTCLOSE ('Listing')
C
 999  CONTINUE
      END
