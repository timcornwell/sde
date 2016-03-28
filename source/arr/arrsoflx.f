C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsoflx.f	1.1	8/16/91
C
      SUBROUTINE ARRSOFLX (ARR, INFOPOS, INFOFLUX, MODE)
C
CD  From source locations and sizes in INFOPOS, we find FLUX
C	
C	ARR	CH*(*)	input	Directory name of array
C	INFOPOS	CH*(*)	input	Directory name of INFO (source positions) (in)
C	INFOFLUXCH*(*)	input	Directory name of INFO (source fluxes) (out)
C	MODE	CH*(*)	input	'CENTER', 'AVERAGE', 'INTEGERATED' flux
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C				M.A.Holdaway	July 11 1991
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ARR, INFOPOS, INFOFLUX, MODE
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSOFLX')
C
      CHARACTER*(1) 	ATYPE, IMTYPE
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM)
      INTEGER 		AADD
      INTEGER		XADD, YADD, DXADD, DYADD, FADD, I, NSOU
C
      INTEGER		CRDRNAX
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (ARR, ANAX, ANAXIS, IMTYPE, AADD)
      CALL DATGETAR (STRM2(INFOPOS, 'X'), I, NSOU, ATYPE, XADD)
      CALL DATGETAR (STRM2(INFOPOS, 'Y'), I, NSOU, ATYPE, YADD)
      CALL DATGETAR (STRM2(INFOPOS, 'DX'), I, NSOU, ATYPE, DXADD)
      CALL DATGETAR (STRM2(INFOPOS, 'DY'), I, NSOU, ATYPE, DYADD)
      CALL DATMAKAR (INFOFLUX, 1, NSOU, 'R', FADD)
      ANAX = CRDRNAX (ANAX, ANAXIS)
C
      IF (IMTYPE.EQ.'R' .AND. ANAX .EQ. 2) THEN
         CALL PIXSOFLX (MEMR(AADD), ANAXIS(1), ANAXIS(2), MODE,
     $      MEMI(XADD), MEMI(YADD), MEMI(DXADD), MEMI(DYADD), NSOU,
     $      MEMR(FADD) )
      ELSE 
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 
     &      'No routine written for this case ')
         GO TO 999
      END IF
C
C Trace errors
C
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
