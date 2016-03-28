C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vpstat.f	1.1	 1/4/92
C
      SUBROUTINE SDEMAIN
C
CD Program to make a linear combination of two images
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 11 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VPSTAT')
C
      CHARACTER*(SYSMXNAM) 	TRUEPB, VPNAME, VP, MASK
      INTEGER		NDUMMY, NPB, IPB
      INTEGER		NAX, NAXIS(SYSMXDIM), ADD
      CHARACTER*1	ATYPE
      REAL       	RMS, SUM, MFACTOR, MPIX, TPIX
      CHARACTER*(SYSMXNAM)	STRRMBL
      CHARACTER*2		STRINT
C==================================================================
C
      CALL MSGWELCO ('I add images')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('TruePB', TRUEPB, 1, NDUMMY)
      CALL USRGETC ('Mask', MASK, 1, NDUMMY)
      CALL USRGETC ('VPNAME',VPNAME, 1, NDUMMY)
      CALL USRGETI ('NPB', NPB, 1, NDUMMY)
      IF (ERROR) GOTO 999
C
      CALL FILIMGGE ('TruePB', TRUEPB, ' ')
      CALL FILIMGGE ('Mask', MASK, ' ')
C
      SUM = 0.0
      DO 100 IPB = 1, NPB
         VP = STRRMBL (VPNAME(1:20)//STRINT(IPB)//'.SDE')
         CALL MSGPUT (VP, 'D')
         CALL FILIMGGE ('VP', VP, ' ')
         CALL IMGCLONE ('VP', 'VPC')
         CALL ARRCOPY  ('VP', 'VPC')
         CALL ARRCONJ ('VPC')
         CALL ARRMULT ('VP', 'VPC', 'PBX')
         CALL HEDCOPY ('VP', 'PBX')
         CALL IMGREAL ('PBX', 'PB')
         CALL ARRLC ('TruePB',1.0,'PB',-1.0,'Temp')
         CALL ARRMULT ('Temp', 'Mask', 'Temp')
         CALL ARRSTAT ('Temp', ' ')
         CALL DATGETR ('Temp', 'ARRRMS', RMS, 1, NDUMMY)
         CALL DATDELET ('VP')
         CALL DATDELET ('VPC')
         CALL DATDELET ('PBX')
         CALL DATDELET ('PB')
         CALL DATDELET ('Temp')
         SUM = SUM + RMS
         IF (ERROR) GOTO 999
 100  CONTINUE
C
C Since 'Mask' is 1 or 0, #pixels = SUM of pixels
C
      CALL ARRSTAT ('Mask', ' ')
      CALL DATGETR ('Mask', 'ARRSUM', MPIX, 1, NDUMMY)
      CALL DATGETAR ('Mask', NAX, NAXIS, ATYPE, ADD)
      TPIX = NAXIS(1) * NAXIS(2)
      MFACTOR = SQRT (TPIX / MPIX)
C
      IF (NPB .NE. 0) THEN
         SUM = MFACTOR * SUM/ FLOAT(NPB)
         WRITE (MESSAGE, 1111) NPB, SUM
 1111    FORMAT ('Average RMS difference of ',I3,' PBs is ',F10.8)
         CALL MSGPUT (MESSAGE, 'I')
      ELSE
         CALL MSGPUT ('No PBs read in!', 'W')
      ENDIF
C
 999  CONTINUE
      END
