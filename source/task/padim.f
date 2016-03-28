C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)padim.f	1.4    5/21/93
C
      SUBROUTINE SDEMAIN
C
CD Program to pad images to a larger size
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PADIM')
C
      INTEGER 		NDUMMY, PNAXIS(SYSMXDIM)
      INTEGER           NAX, NAXIS(SYSMXDIM), BLANK, IAX
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), 
     1			ROTA(SYSMXDIM), PADVAL
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	INFILE, PDFILE, PADTYPE
C
C==================================================================
C
      CALL MSGWELCO ('I pad images to a larger size')
      CALL USRCTL
C
C                         Get inputs
C
      CALL USRGETC ('Image', INFILE, 1, NDUMMY)
      CALL USRGETC ('Padim', PDFILE, 1, NDUMMY)
      CALL USRGETI ('Padsize', PNAXIS, SYSMXDIM, NDUMMY)
      CALL USRGETR ('Padval', PADVAL, 1, NDUMMY)
      CALL USRGETI ('Blank' , BLANK, 1, NDUMMY)
      CALL USRGETC ('Padtype', PADTYPE, 1, NDUMMY)
      IF (ERROR) GO TO 999
C
C                         Get input Image
C
      CALL FILIMGGE ('Image', INFILE, ' ')
      CALL CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      IF (ERROR) GOTO 999
C         
C                         Check if output larger than input
C
      DO 10 IAX = 1, NAX-1
         IF (PNAXIS(IAX).LT.NAXIS(IAX)) THEN
            CALL MSGPUT ('Requested output is smaller than input', 'I')
            GO TO 999
         END IF
 10   CONTINUE
C
      WRITE (MESSAGE, 1000) PADVAL
 1000 FORMAT ('Padding image with ', 1PE12.5)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (PADTYPE(1:1).NE.' ') THEN
         CALL DATPUTC('Image', 'PADTYPE', PADTYPE, 1)
      END IF
C
C                        Fill output from input
C
      CALL IMGPAD ('Image', 'Padim', PNAXIS, PADVAL)
C
C Do something about the History
C
      CALL HISCOPY('Image', 'Padim')
      CALL HISINPUT('Padim')
C
C Put the data on Disk
C
      CALL FILIMGPU ('Padim', PDFILE, ' ')
C
 999  CONTINUE
      END
