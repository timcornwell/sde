C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgprt.f	1.2    11/7/90
C
      SUBROUTINE SDEMAIN
C
CD Program to print images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPRT')
C
      INTEGER		NDUMMY, DIR, IAX, NAX, NAXIS(SYSMXDIM),
     1			BLC(SYSMXDIM), TRC(SYSMXDIM),
     2			REALNAX, AADD
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	FILENAME
C==================================================================
      CALL MSGWELCO ('I print images')
      CALL USRCTL
C
C Get input image
C
      CALL USRGETC ('Image', FILENAME, 1, NDUMMY)
      CALL FILIMGGE ('Image', FILENAME, ' ')
      IF (ERROR) GO TO 999
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, AADD)
      IF (ERROR) GO TO 999
      REALNAX = NAX
  1   CONTINUE
      IF (NAXIS(REALNAX).EQ.1) THEN
         REALNAX = REALNAX - 1
         GO TO 1
      END IF
C
C List coordinates
C
      CALL MSGPUT ('Coordinates', 'I')
      CALL CRDLIST ('Image')
      CALL MSGPUT (' ', 'I')
C
      CALL USRGETI ('BLC', BLC, REALNAX, NDUMMY)
      CALL ERRCANCE
      CALL USRGETI ('TRC', TRC, REALNAX, NDUMMY)
      CALL ERRCANCE
C
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL ARRPRINT ('Image', 'Window')
C
 999  CONTINUE
      END
