C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)makecube.f	1.1	 9/22/94
C
      SUBROUTINE SDEMAIN
C
CD Program to make a cube from individual planes
C  We assume that the individual images are named
C  {basename}i{extension} and that the i's are
C  in some sence sequential (ie, could skip at regular intervals)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Aug 2 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MAKECUBE')
C
      INTEGER		NDUMMY
      CHARACTER*(SYSMXNAM)	CUBESTEAK, BASENAME, EXT,
     $     		INFILE
      INTEGER		ISTART, IEND, IINT, IREF
      INTEGER		BLC(SYSMXDIM), TRC(SYSMXDIM),
     1			STEP(SYSMXDIM), SUM(SYSMXDIM)
C
      INTEGER		NAX, NAXIS(SYSMXDIM), OADD, CADD, PADD, I, J, NT
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL		RPIX0(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRRMBL
      CHARACTER*4		STRINT
      INTEGER			DATADD
C==================================================================
      CALL MSGWELCO ('I make cubes')
      CALL USRCTL
C
C Specify input images
C
      CALL USRGETC ('BaseName', BASENAME, 1, NDUMMY)
      CALL USRGETC ('Extension', EXT, 1, NDUMMY)
      CALL USRGETI ('Istart', ISTART, 1, NDUMMY)
      CALL USRGETI ('Iend', IEND, 1, NDUMMY)
      CALL USRGETI ('Iinterval', IINT, 1, NDUMMY)
      CALL USRGETI ('Ireference', IREF, 1, NDUMMY)
C
C Specify part of image we process
C
      CALL USRGETI ('BLC', BLC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('TRC', TRC, SYSMXDIM, NDUMMY)
      CALL USRGETI ('STEP', STEP, SYSMXDIM, NDUMMY)
      CALL USRGETI ('SUM', SUM, SYSMXDIM, NDUMMY)
C
      CALL USRGETC ('OutCube', CUBESTEAK, 1, NDUMMY)

      IF (IINT .LE. 0) IINT = 1
      INFILE=STRRMBL( BASENAME(1:40)//STRINT(ISTART)//EXT(1:20))
      CALL FILIMGGE ('One', INFILE, ' ')
C
C Make window
C
      CALL DATCREAT ('Window')
      CALL DATPUTI( 'Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'TRC', TRC, SYSMXDIM)
      CALL DATPUTI( 'Window', 'STEP', STEP, SYSMXDIM)
      CALL DATPUTI( 'Window', 'SUM', SUM, SYSMXDIM)
      CALL IMGSUBSE ('One', 'OneSub', 'Window')
      CALL CRDGET ('OneSub', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      
C
C Create Cube with correct Header
C
      CALL DATGETAR ('OneSub', NAX, NAXIS, ATYPE, OADD)
      IF (ATYPE .NE. 'R') THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $        'Can only deal with REAL images')
         GOTO 999
      ENDIF
      NAXIS(3) = (IEND - ISTART)/IINT + 1
      CALL DATMAKAR ('Cube', NAX, NAXIS, ATYPE, CADD)
      DELT(3) = DELT(3) * IINT
      RPIX(3) = FLOAT(IREF - ISTART) / FLOAT(IINT) + 1
      CALL CRDPUT ('Cube', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Fill Cube
C
      J = 0
      NT = NAXIS(1) * NAXIS(2)
      DO 100 I = ISTART, IEND, IINT
         J = J + 1
         CALL DATDELET ('One')
         CALL DATDELET ('OneSub')
         INFILE=STRRMBL( BASENAME(1:40)//STRINT(I)//EXT(1:20))
         CALL FILIMGGE ('One', INFILE, ' ')
         CALL IMGSUBSE ('One', 'OneSub', 'Window')
         OADD = DATADD ('OneSub')
         PADD = CADD + NT*(J-1)
         CALL PIXRCOPY (MEMR(OADD), 0, 1, MEMR(PADD), 0, 1, NT)
 100  CONTINUE
C
      CALL HISINPUT ('Cube')
      CALL FILIMGPU ('Cube', CUBESTEAK, ' ')
C
 999  CONTINUE
      END
