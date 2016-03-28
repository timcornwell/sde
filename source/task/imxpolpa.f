C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imxpolpa.f	1.1	 5/4/93
C
      SUBROUTINE SDEMAIN
C
CD Program to find POL position angle of some region
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	May 3 1993
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMXPOLPA')
C
      CHARACTER*(SYSMXNAM) 	QFILE, UFILE
      INTEGER			BLC(SYSMXDIM), TRC(SYSMXDIM)
      INTEGER		NDUMMY
C
      REAL		P, THETA, QAVE, UAVE, D2R
      INTEGER		NAX, NAXIS(SYSMXDIM), ADDR, IAX, NREAL
      CHARACTER*1	ATYPE
C
      INTEGER		CRDRNAX
      REAL		DATFGETR
C==================================================================
C
      CALL MSGWELCO ('I find polarization AMP & PA for some region')
      CALL USRCTL
      D2R = ATAN2(1.0, 1.0) / 45.0
C
C Get Images
C
      CALL USRGETC ('Qmap', QFILE, 1, NDUMMY)
      CALL USRGETC ('Umap', UFILE, 1, NDUMMY)
      CALL USRGETI ('blc', BLC, 7, NDUMMY)
      CALL USRGETI ('trc', TRC, 7, NDUMMY)
C
      CALL FILIMGGE ('Q', QFILE, '*')
      CALL FILIMGGE ('U', UFILE, '*')
C
      CALL CRDCHECK( 'Q', 'U')
      IF (ERROR) GOTO 999
C
      CALL DATGETAR ('Q', NAX, NAXIS, ATYPE, ADDR)
      NREAL = CRDRNAX (NAX, NAXIS)
      DO 10 IAX = 1, NREAL
         IF(BLC(IAX).EQ.0) BLC(IAX) = NAXIS(IAX)/2 -1
         IF(TRC(IAX).EQ.0) TRC(IAX) = NAXIS(IAX)/2 +1
         BLC (IAX) = MAX (1, MIN (NAXIS(IAX), BLC (IAX)))
         TRC (IAX) = MAX (1, MIN (NAXIS(IAX), TRC (IAX)))
  10  CONTINUE
      DO 15 IAX = NREAL+1, SYSMXDIM
         BLC (IAX) = 1
         TRC (IAX) = 1
 15   CONTINUE
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
C
      CALL ARRSTAT ('Q', 'Window')
      CALL ARRSTAT ('U', 'Window')
      QAVE = DATFGETR('Q', 'ARRAVE')
      UAVE = DATFGETR('U', 'ARRAVE')
      P = SQRT(QAVE**2 + UAVE**2)
      THETA = ATAN2( UAVE, QAVE )/D2R / 2.0
C
      WRITE (MESSAGE, 1020) BLC(1), BLC(2), TRC(1), TRC(2)
 1020 FORMAT ('Averaging region BLC: ',2I5, '  to TRC: ',2I5)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1023) P, THETA
 1023 FORMAT ('Pol Amp = ',F10.5, '   Pol PA = ',F10.3)
      CALL MSGPUT (MESSAGE, 'I')
C
 999  CONTINUE
      END
