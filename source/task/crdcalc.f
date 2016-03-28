C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdcalc.f	1.1    3/14/94
C
      SUBROUTINE SDEMAIN
C
CD Calculate misc coordinate conversions
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	March 14 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'BEAMCALC')
C
      INTEGER 		NDUMMY, NAX, NAXIS(SYSMXDIM), IAX
      CHARACTER*(SYSMXNAM)	IMG1, IMG2, MODE
      INTEGER		BLC1(SYSMXDIM), TRC1(SYSMXDIM),
     $   		BLC2(SYSMXDIM), TRC2(SYSMXDIM), RNAX
      CHARACTER*1	ATYPE
C
      INTEGER		CRDRNAX
C==================================================================
      CALL MSGWELCO ('I calculate coordinate conversions')
      CALL USRCTL
C
C Get inputs
C
      CALL USRGETC ('Img1', IMG1, 1, NDUMMY)
      CALL USRGETC ('Img2', IMG2, 1, NDUMMY)
      CALL USRGETI ('BLC', BLC1, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC', TRC1, SYSMXDIM, NDUMMY)
      CALL USRGETC ('Mode', STRBUF, 1, NDUMMY)
      CALL STRUC (STRBUF, MODE)
C
      IF (MODE.EQ.' ') MODE = 'BOX'
      IF (MODE.NE.'BOX') THEN
         CALL MSGPUT ('Bad MODE','E')
         GO TO 999
      END IF
C
      CALL FILIMGGE ('Img1', IMG1, ' ')
      CALL FILIMGGE ('Img2', IMG2, ' ')
C
      CALL DATGETAR ('Img1', NAX, NAXIS, ATYPE, NDUMMY)
      RNAX = CRDRNAX (NAX, NAXIS)
C
      IF (MODE.EQ.'BOX') THEN
         CALL MSGPUT('BOX relative to Image 1:','I')
         DO 20 IAX = 1, RNAX
            WRITE (MESSAGE, 1000) IAX, BLC1(IAX), IAX, TRC1(IAX)
 1000       FORMAT ('BLC(',I1,') =',I5,'   TRC(',I1,') =',I5)
            CALL MSGPUT (MESSAGE, 'I')
 20      CONTINUE
C
         CALL CRDSHBOX ('Img1', BLC1, TRC1, 'Img2', BLC2, TRC2)
C
         CALL MSGPUT('BOX relative to Image 2:','I')
         DO 40 IAX = 1, RNAX
            WRITE (MESSAGE, 1000) IAX, BLC2(IAX), IAX, TRC2(IAX)
            CALL MSGPUT (MESSAGE, 'I')
 40      CONTINUE
C         
      END IF
C
 999  CONTINUE
      END
