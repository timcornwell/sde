C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)modlist.f	1.4    11/21/94
C
      SUBROUTINE MODLIST (MODEL)
C
CD List a model
C
C	MODEL/FLUX	REAL	Flux of component in Jy
C	MODEL/RA	REAL	Position of component in Ra offset (asec)
C	MODEL/DEC	REAL	Position of component in Dec offset (asec)
C	MODEL/BMAJ	REAL	Major axis in asec
C	MODEL/BMIN	REAL	Minor axis in asec
C	MODEL/BPA	REAL	Position angle in degrees
C	MODEL/TYPE	CHAR	Type: 'POINT', 'GAUSS', 'RECT', 'DISK'
C	MODEL/VEL	REAL	velocity magnitude in mas/day
C	MODEL/VPA	REAL	velocity position angle in degrees
C
C	MODEL	CH*(*)	input	Name of directory entry
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added time variable models
C				D.S.Briggs	Nov 19 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	MODEL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'MODLIST')
C
      INTEGER		NCOMP, ICOMP, FLADD, RAADD, DECADD, 
     1			TADD, BMAJADD, BMINADD, BPAADD, VADD, VPADD, I
      CHARACTER*1	ATYPE
      LOGICAL		DOTIMEV
C
      LOGICAL		DATEXIST
      INTEGER		DATADD
      CHARACTER*(SYSMXNAM)	STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Find number of components
C
      CALL DATGETAR (STRM2(MODEL, 'FLUX'), I, NCOMP, ATYPE, 
     1   FLADD)
      RAADD = DATADD (STRM2(MODEL, 'RA'))
      DECADD = DATADD (STRM2(MODEL, 'DEC'))
      BMAJADD = DATADD (STRM2(MODEL, 'BMAJ'))
      BMINADD = DATADD (STRM2(MODEL, 'BMIN'))
      BPAADD = DATADD (STRM2(MODEL, 'BPA'))
      TADD = DATADD (STRM2(MODEL, 'TYPE'))
      DOTIMEV = .FALSE.
      IF (DATEXIST(STRM2(MODEL, 'VEL'))) THEN
         DOTIMEV = .TRUE.
         VADD = DATADD (STRM2(MODEL, 'VEL'))
         VPADD = DATADD (STRM2(MODEL, 'VPA'))
      END IF
C
      DO 100 ICOMP = 1, NCOMP
         I = ICOMP - 1
         WRITE (STRBUF, 1000) ICOMP, MEMR(FLADD+I), MEMR(RAADD+I),
     $      MEMR(DECADD+I), MEMR(BMAJADD+I), MEMR(BMINADD+I),
     $      MEMR(BPAADD+I), MEMC(TADD+I)(1:4)
 1000    FORMAT (I4,6(F9.3),1X,A)
         CALL MSGPUT (STRBUF, 'I')
         IF (DOTIMEV) THEN
            WRITE (STRBUF, 1010) ICOMP, MEMR(VADD+I), MEMR(VPADD+I)
 1010       FORMAT (I4,6X,2(F9.3))
            CALL MSGPUT (STRBUF, 'I')
         END IF
  100 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
