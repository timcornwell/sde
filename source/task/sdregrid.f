C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)sdregrid.f	1.2	 7/17/95
C
      SUBROUTINE SDEMAIN
C
CD Program to regrid spectral line cube onto different channels
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 17 1993
C	Added Reference Values which may differ for the IN and OUT
C				M.A. Holdaway	July 17, 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SDREGRID')
C
      CHARACTER*(SYSMXNAM)	OUT, IMAGE, METHOD
      INTEGER			NZ
      REAL			RPIXZ, DELTZ, BLANK

      CHARACTER*1	T
      DOUBLE PRECISION  IRVAL(SYSMXDIM), ORVAL(SYSMXDIM), RVALZ
      REAL              IRPIX(SYSMXDIM), IDELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL              ORPIX(SYSMXDIM), ODELT(SYSMXDIM)
      REAL		ARRAVE, ARRMAX, ARRMIN
      CHARACTER*8       TYPE(SYSMXDIM)
      INTEGER 		NDUMMY, BLC(SYSMXDIM),
     $   		TRC(SYSMXDIM), IADD, OADD, IAX
      INTEGER		INAX, INAXIS(SYSMXDIM), ONAX, ONAXIS(SYSMXDIM)
      DATA		BLC /SYSMXDIM*1/
      DATA		TRC /SYSMXDIM*1/
C==================================================================
C
      CALL MSGWELCO ('I regrid single dish data onto new planes')
      CALL MSGPUT   ('Warning:  Does not work for FITS output img', 'W')
      CALL MSGPUT   ('Use .SDE extension for Output Image', 'W')
      CALL USRCTL
C
C Get Image
      CALL USRGETC('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC('Out', OUT, 1, NDUMMY)
      CALL USRGETI('NZ', NZ, 1, NDUMMY)
      CALL USRGETR('DELTZ', DELTZ, 1, NDUMMY)
      CALL USRGETR('RPIXZ', RPIXZ, 1, NDUMMY)
      CALL USRGETD('RVALZ', RVALZ, 1, NDUMMY)
      CALL USRGETR('Blank', BLANK, 1, NDUMMY)
      CALL USRGETC('Method', METHOD, 1, NDUMMY)
C
      CALL FILIMGGE ('Image', IMAGE, ' ')
C
      CALL DATGETAR ('Image', INAX, INAXIS, T, IADD)
      TRC(1) = INAXIS(1)
      TRC(2) = INAXIS(2)
      TRC(3) = NZ
      CALL DATCREAT ('Window')
      CALL DATPUTI ('Window', 'BLC', BLC, SYSMXDIM)
      CALL DATPUTI ('Window', 'TRC', TRC, SYSMXDIM)
      CALL IMGSUBSE ('Image', 'Out', 'Window')
      CALL ARRSETCO ('Out', 0.0, BLANK)
C
      CALL CRDGET ('Image', INAX, TYPE, INAXIS, IRVAL, IRPIX, IDELT,
     1   ROTA)
      DO 10 IAX = 1, 2
         ONAXIS(IAX) = INAXIS(IAX)
         ODELT(IAX) = IDELT(IAX)
         ORPIX(IAX) = IRPIX(IAX)
         ORVAL(IAX) = RVALZ
 10   CONTINUE
      ONAXIS(3) = NZ
      ODELT(3) = DELTZ
      ORPIX(3) = RPIXZ
      CALL CRDPUT ('Out', INAX, TYPE, ONAXIS, IRVAL, ORPIX, ODELT,
     1   ROTA)
      CALL DATGETAR ('Image', INAX, INAXIS, T, IADD)
      CALL DATGETAR ('Out',   ONAX, ONAXIS, T, OADD)
C
      CALL PIXSRGRD (MEMR(IADD), INAXIS(1), INAXIS(2), INAXIS(3),
     $   IDELT(3), IRPIX(3), REAL(IRVAL(3)),
     $   MEMR(OADD), ONAXIS(1), ONAXIS(2), ONAXIS(3),
     $   ODELT(3), ORPIX(3), REAL(ORVAL(3)), BLANK, METHOD )
C
      CALL ARRSTAT ('Out', ' ') 
      CALL DATGETR ('Out', 'ARRMAX', ARRMAX, 1, NDUMMY)
      CALL DATGETR ('Out', 'ARRMIN', ARRMIN, 1, NDUMMY)
      CALL DATGETR ('Out', 'ARRAVE', ARRAVE, 1, NDUMMY)
      WRITE (MESSAGE, 1993) ARRMAX, ARRMIN, ARRAVE
 1993 FORMAT ('Out Max, Min, Ave: ',3F10.3)
      CALL MSGPUT (MESSAGE, 'I')
      IF (ERROR) GOTO 999
      CALL FILIMGPU ('Out', OUT, ' ')
C
 999  CONTINUE
      END



