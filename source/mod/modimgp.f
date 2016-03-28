C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modimgp.f	1.1    6/2/93
C
      SUBROUTINE MODIMGP (MODEL, IMAGE)
C
CD Make a projected image from a description of a model: the model has parts
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C
C
C	MODEL	CH*(*)	input	Name of directory entry
C	IMAGE	CH*(*)	input	Name of directory entry
C Audit trail:
C	Cloned from modimg
C				R.G. Marson    1 June 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, IMAGE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODIMGP')
C
      REAL		IMRPIX (SYSMXDIM), IMDELT(SYSMXDIM),
     1			IMROTA (SYSMXDIM)
      DOUBLE PRECISION	IMRVAL(SYSMXDIM)
      CHARACTER*8	IMTYPE(SYSMXDIM)
      INTEGER		IMNAX, IMNAXIS(SYSMXDIM)
      INTEGER		NAX, NAXIS(SYSMXDIM), RNAX
      INTEGER		NCOMP, FLADD, RAADD, DECADD, TADD,
     1			BMAJADD, BMINADD, BPAADD, IMADD, DATADD
      CHARACTER*1	ATYPE
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		CRDRNAXM
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, IMNAX, IMTYPE, IMNAXIS, IMRVAL, IMRPIX, 
     1   IMDELT, IMROTA)
      RNAX = CRDRNAXM(IMNAX, IMNAXIS)
C
      CALL DATGETAR (STRM2(MODEL, 'FLUX'), NAX, NAXIS, ATYPE,
     1   FLADD)
      NCOMP = NAXIS(1)
      RAADD = DATADD(STRM2(MODEL, 'RA'))
      DECADD = DATADD(STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD(STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD(STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD(STRM2(MODEL, 'BPA'))
      CALL DATGETAR (STRM2(MODEL, 'TYPE'), NAX, NAXIS, ATYPE,
     1   TADD)
      IMADD = DATADD(IMAGE)
C
      IF (RNAX.EQ.1) THEN
         CALL MODIMG1P (NCOMP, MEMR(FLADD), MEMR(RAADD),
     1        MEMR(BMAJADD), MEMR(BMINADD), MEMR(BPAADD), MEMC(TADD),
     $        MEMR(IMADD), IMNAXIS(1),
     $        IMRPIX(1),  3600.0*IMDELT(1))
      ELSE 
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only supports projections to 1-D arrays')
         GO TO 999
      END IF
C
      CALL DATPUTC (IMAGE, 'BUNIT', 'JY/PIXEL', 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
