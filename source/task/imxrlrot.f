C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imxrlrot.f	1.1	 5/4/93
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
      PARAMETER		(ROUTINE = 'IMXRLROT')
C
      CHARACTER*(SYSMXNAM) 	QIN, UIN, QOUT, UOUT, ANGTYPE
      INTEGER		NDUMMY
C
      REAL		THETA, THETA1
C
C==================================================================
C
      CALL MSGWELCO ('I rotate the phase of a complex image')
      CALL USRCTL
C
C Get Images
C
      CALL USRGETC ('Qin', QIN, 1, NDUMMY)
      CALL USRGETC ('Uin', UIN, 1, NDUMMY)
      CALL USRGETR ('Theta', THETA, 1, NDUMMY)
      CALL USRGETC ('AngleType', ANGTYPE, 1, NDUMMY)
      CALL USRGETC ('Qout', QOUT, 1, NDUMMY)
      CALL USRGETC ('Uout', UOUT, 1, NDUMMY)
C
      IF (ANGTYPE(1:3) .EQ. 'CHI') THEN
         THETA1 = 2.0 * THETA
      ELSE
         THETA1 = THETA
      ENDIF
C
      CALL FILIMGGE ('Q', QIN, ' ')
      CALL FILIMGGE ('U', UIN, ' ')
C
      CALL ARRQU2X  ('Q', 'U', 'X')
      CALL ARRXROT  ('X', THETA1, 'XR')
      CALL ARRX2QU  ('XR', 'QR', 'UR')
C
      CALL FILIMGPU ('QR', QOUT, ' ')
      CALL FILIMGPU ('UR', UOUT, ' ')
C
 999  CONTINUE
      END
