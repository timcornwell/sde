C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modimg.f	1.5    11/21/94
C
      SUBROUTINE MODIMG (MODEL, IMAGE)
C
CD Make an image from a description of a model: the model has parts
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	MODEL/TIMEOFFSET REAL	time offset for model in days
C
C	MODEL	CH*(*)	input	Name of directory entry
C	IMAGE	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added support for 1-D images
C				D.S.Briggs	Oct 21 1992
C	Added support for time variable models
C				D.S.Briggs	Nov 20 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	MODEL, IMAGE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODIMG')
C
      REAL		IMRPIX (SYSMXDIM), IMDELT(SYSMXDIM),
     1			IMROTA (SYSMXDIM), TIMEOFF, D2R
      DOUBLE PRECISION	IMRVAL(SYSMXDIM)
      CHARACTER*8	IMTYPE(SYSMXDIM)
      INTEGER		IMNAX, IMNAXIS(SYSMXDIM), I
      INTEGER		NAX, NAXIS(SYSMXDIM), RNAX
      INTEGER		NCOMP, FLADD, RAADD, DECADD, TADD,
     $			BMAJADD, BMINADD, BPAADD, IMADD, DATADD,
     $   		VELADD, VPAADD
      CHARACTER*1	ATYPE
C
      LOGICAL		DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		CRDRNAX
      REAL		DATFGETR
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, IMNAX, IMTYPE, IMNAXIS, IMRVAL, IMRPIX, 
     1   IMDELT, IMROTA)
      RNAX = CRDRNAX(IMNAX, IMNAXIS)
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
      IF (DATEXIST(STRM2(MODEL, 'TIMEOFFSET'))) THEN
         TIMEOFF = DATFGETR (MODEL, 'TIMEOFFSET')
         IF (TIMEOFF.NE.0.0) THEN
            VELADD = DATADD(STRM2(MODEL, 'VEL'))
            VPAADD = DATADD(STRM2(MODEL, 'VPA'))
            D2R = 4.0 * ATAN(1.0) / 180.0
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 1000) TIMEOFF
 1000       FORMAT ('Adjusting positions for a time offset of',F9.3,
     $         ' days')
            CALL MSGPUT (MESSAGE,'I')
            DO 100 I = 1, NCOMP
               MEMR(RAADD+I-1) = MEMR(RAADD+I-1) -
     $            TIMEOFF*MEMR(VELADD+I-1)*
     $            COS((MEMR(VPAADD+I-1)+90.0)*D2R)/1000.0
               MEMR(DECADD+I-1) = MEMR(DECADD+I-1) +
     $            TIMEOFF*MEMR(VELADD+I-1)*
     $            SIN((MEMR(VPAADD+I-1)+90.0)*D2R)/1000.0
 100        CONTINUE
         END IF
      END IF
C
      IF (RNAX.EQ.1) THEN
         CALL MODIMG1D (NCOMP, MEMR(FLADD), MEMR(RAADD),
     1      MEMR(BMAJADD), MEMC(TADD), MEMR(IMADD), IMNAXIS(1),
     $      IMRPIX(1),  3600.0*IMDELT(1))
      ELSE IF (RNAX.EQ.2) THEN
         CALL MODIMG2D (NCOMP, MEMR(FLADD), MEMR(RAADD), MEMR(DECADD),
     1      MEMR(BMAJADD), MEMR(BMINADD), MEMR(BPAADD), MEMC(TADD),
     2      MEMR(IMADD), IMNAXIS(1), IMNAXIS(2), IMRPIX(1), 
     3      3600.0*IMDELT(1), IMRPIX(2), 3600.0*IMDELT(2))
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only support 1 or 2 real dimensions')
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
