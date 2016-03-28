C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)deplane.f	1.1	 5/13/93
C
      SUBROUTINE SDEMAIN
C
C Program to FIT and REMOVE a plane from an image
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 9 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DEPLANE')
C
      CHARACTER*(SYSMXNAM)	IMAGE, OUTFILE
      INTEGER			BLC1(2), BLC2(2), BLC3(2)
      INTEGER			TRC1(2), TRC2(2), TRC3(2)
      INTEGER			NDUMMY
C
      REAL			P1(3), P2(3), P3(3)
C==================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I set arrays to a constant value')
C
      CALL USRCTL
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Out', OUTFILE, 1, NDUMMY)
      CALL USRGETI ('BLC1', BLC1, 2, NDUMMY)
      CALL USRGETI ('BLC2', BLC2, 2, NDUMMY)
      CALL USRGETI ('BLC3', BLC3, 2, NDUMMY)
      CALL USRGETI ('TRC1', TRC1, 2, NDUMMY)
      CALL USRGETI ('TRC2', TRC2, 2, NDUMMY)
      CALL USRGETI ('TRC3', TRC3, 2, NDUMMY)
C
      CALL FILIMGGE ('Image', IMAGE, ' ')
      CALL IMGCLONE ('Image', 'Out')
      CALL DATCREAT ('Window')
C
      CALL DATPUTI ('Window', 'BLC', BLC1, 2)
      CALL DATPUTI ('Window', 'TRC', TRC1, 2)
      CALL ARRSTAT ('Image', 'Window')
      P1(1) = ( BLC1(1) + TRC1(1) ) / 2.0
      P1(2) = ( BLC1(2) + TRC1(2) ) / 2.0
      CALL DATGETR ('Image', 'ARRAVE', P1(3), 1, NDUMMY)
C
      CALL DATPUTI ('Window', 'BLC', BLC2, 2)
      CALL DATPUTI ('Window', 'TRC', TRC2, 2)
      CALL ARRSTAT ('Image', 'Window')
      P2(1) = ( BLC2(1) + TRC2(1) ) / 2.0
      P2(2) = ( BLC2(2) + TRC2(2) ) / 2.0
      CALL DATGETR ('Image', 'ARRAVE', P2(3), 1, NDUMMY)
C
      CALL DATPUTI ('Window', 'BLC', BLC3, 2)
      CALL DATPUTI ('Window', 'TRC', TRC3, 2)
      CALL ARRSTAT ('Image', 'Window')
      P3(1) = ( BLC3(1) + TRC3(1) ) / 2.0
      P3(2) = ( BLC3(2) + TRC3(2) ) / 2.0
      CALL DATGETR ('Image', 'ARRAVE', P3(3), 1, NDUMMY)
C
      CALL ARRDPLAN ('Image', 'Out', P1, P2, P3)

      CALL FILIMGPU('Out',OUTFILE,' ')
C
      END
