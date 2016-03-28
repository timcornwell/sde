C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)shape.f	1.1	 7/5/95
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate image shape
C
C Look through the saoimage region file and search for ELLIPSE
C ELLIPSE(66,63,17.00,55.18,99.273)
C         X   Y   MIN  MAJ  PA
C
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	June 30 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(SYSMXNAM)	SAOFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SHAPE')
C
      CHARACTER*(SYSMXNAM)	IMAGE
      INTEGER		NCHAR
      LOGICAL		EOF
      CHARACTER*132	LINE, LINE2
C
      INTEGER		I1, I2
      INTEGER           NAX, NAXIS(SYSMXDIM), IELLIPSE, NDUMMY
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      REAL		PCENTER(SYSMXDIM), WCENTER(SYSMXDIM)
      REAL		EMAJ, EMIN, EPA
      INTEGER		R1, R2, R3, D1, D2, D3, DSIGN
C==================================================================
C
      CALL MSGWELCO ('I calculate image shape')
      CALL USRCTL
C
      CALL USRGETC ('SAOregion', SAOFILE, 1, NDUMMY)
C
      IF (ERROR) GO TO 999
C
      CALL TXTOPEN (ROUTINE, SAOFILE, 'READ')
      CALL TXTREAD (ROUTINE,  LINE, NCHAR, EOF)
      IMAGE = LINE(3:64)
      CALL FILIMGGE ('Image', IMAGE, ' ')
      CALL  CRDGET ('Image', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
      MESSAGE = '          #       HH MM SS    DD MM SS     '//
     $     'ArcMin ArcMin  Deg'
      CALL MSGPUT (MESSAGE, 'I')
      IELLIPSE = 0
 100  CONTINUE
          CALL TXTREAD (ROUTINE,  LINE, NCHAR, EOF)
          IF (EOF) GOTO 200
          IF (LINE(2:8) .EQ. 'ELLIPSE') THEN
             IELLIPSE = IELLIPSE + 1
C
C Get the center of the ELLIPSE in RA DEC coordinates
C

             I1 = INDEX(LINE, '(')+1
             I2 = INDEX(LINE, ')')-1
             LINE2 = LINE(I1:I2)
             READ (LINE2, *, ERR=200) PCENTER(1), PCENTER(2),
     $            EMAJ, EMIN, EPA
             EMAJ = EMAJ * ABS(DELT(1)) * 60.0
             EMIN = EMIN * ABS(DELT(1)) * 60.0
             CALL CRDPTOW ('Image', PCENTER, WCENTER)
C
             R1 = INT( WCENTER(1)/15. )
             R2 = INT( 60.0* (WCENTER(1)/15. - R1))
             R3 = INT( 60.0* ( 60.0*(WCENTER(1)/15. - R1) -R2 ))
C
             DSIGN = WCENTER(2)/ ABS(WCENTER(2))
             WCENTER(2) = ABS(WCENTER(2))
             D1 = INT( WCENTER(2) )
             D2 = INT( 60.0* (WCENTER(2) - D1))
             D3 = INT( 60.0* ( 60.0*(WCENTER(2) - D1) -D2 ))
             D1 = DSIGN * D1
C
             WRITE (MESSAGE, 1010) IELLIPSE, R1,R2,R3,D1,D2,D3,
     $            EMAJ, EMIN, EPA
 1010        FORMAT ('ELLIPSE ',I3, ' At:   ',I2,1X,I2,1X,I2,3X,
     $            I3,1X,I2,1X,I2,3X, 3F7.2)
             CALL MSGPUT (MESSAGE, 'I')
C 
          ENDIF
      GOTO 100
 200  CONTINUE
C
 999  CONTINUE
      END
