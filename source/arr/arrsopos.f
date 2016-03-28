C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsopos.f	1.1	8/16/91
C
      SUBROUTINE ARRSOPOS (ARR, FLUXMIN, NOISE, DMAX, INFO)
C
#define ns 1024
C
CD Determine the location and width of all sources in ARR
C	The X, Y, DX, and DY (in pixels) for any pixels
C	above FLUXMIN will be determined and placed in arrays
C	nameed STRM2(INFO, 'X'), STRM2(INFO, 'Y'), STRM2(INFO, 'DX')
C	DX refers to the distance from pixel X to the point at
C	which the pixel value falls down to NOISE (max of + and -
C	directions).  
C
C	ARR	CH*(*)	input	Directory name of array
C	INFO	CH*(*)	input	Directory name of INFO
C Audit trail:
C	Original version: Audit trail comments go on this line
C				M.A.Holdaway	July 11 1991
C
C----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*) 	ARR, INFO
      REAL		FLUXMIN, NOISE
      INTEGER		DMAX
C
C
      CHARACTER*(*) 	ROUTINE
      PARAMETER 	(ROUTINE='ARRSOPOS')
C
      CHARACTER*(1) 	ATYPE
      CHARACTER		STRM2*(SYSMXNAM)
      INTEGER 		ANAX, ANAXIS(SYSMXDIM)
      INTEGER 		AADD
      INTEGER		X(ns), Y(ns), DX(ns), DY(ns)
      INTEGER		XADD, YADD, DXADD, DYADD, I, NOUT
C
      INTEGER		CRDRNAX
C=====================================================================
C
C If there is an error on entry then return immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (ARR, ANAX, ANAXIS, ATYPE, AADD)
      ANAX = CRDRNAX (ANAX, ANAXIS)
      DMAX = MAX (1, DMAX)
C
      IF (ATYPE.EQ.'R' .AND. ANAX .EQ. 2) THEN
         CALL PIXSOPOS (MEMR(AADD), ANAXIS(1), ANAXIS(2),
     $      FLUXMIN, NOISE, DMAX, X, Y, DX, DY, ns, NOUT)
         IF (NOUT .GE. ns) THEN
            CALL MSGPUT ('WARNING! NOUT .GE. ns in ARRSOPOS', 'W')
            NOUT = ns
         ENDIF
         CALL DATMAKAR (STRM2(INFO, 'X'), 1, NOUT, 'I', XADD)
         CALL DATMAKAR (STRM2(INFO, 'Y'), 1, NOUT, 'I', YADD)
         CALL DATMAKAR (STRM2(INFO, 'DX'), 1, NOUT, 'I', DXADD)
         CALL DATMAKAR (STRM2(INFO, 'DY'), 1, NOUT, 'I', DYADD)
         DO 100 I = 1, NOUT
            MEMI(XADD + I - 1) = X(I)
            MEMI(YADD + I - 1) = Y(I)
            MEMI(DXADD + I - 1) = DX(I)
            MEMI(DYADD + I - 1) = DY(I)
 100     CONTINUE
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
