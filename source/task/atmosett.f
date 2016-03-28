C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)atmosett.f	1.1	 9/15/94
C
      SUBROUTINE SDEMAIN
C
CD Sets the reference time of an atmosphere to the first time of the visibility
C
C	New Program:  
C				M.A.Holdaway	Dec 9 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ATMOSETT')
C
      CHARACTER*(SYSMXNAM)	VIS, ATMOIN, ATMOOUT
      REAL			T0
      INTEGER		I, NDUMMY, TADD
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C
      INTEGER		DATADD
C==================================================================
      CALL MSGWELCO ('I corrupt existing visibility data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('VIS', VIS, 1, NDUMMY)
      CALL USRGETC ('ATMOIN', ATMOIN, 1, NDUMMY)
      CALL USRGETC ('ATMOOUT', ATMOOUT, 1, NDUMMY)
      CALL USRGETR ('T0', T0, 1, NDUMMY)
C
      IF (VIS.NE.' ') THEN
         CALL VISGET ('Vis', VIS,  'I', '*', ' ')
         TADD = DATADD( 'Vis/TIME' )
         T0 = MEMR(TADD)
      ENDIF
C
      CALL FILIMGGE ('Atmos', ATMOIN, ' ')
      CALL CRDGET ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      DO 25 I = 1, NAX
         IF (TYPE(I) .EQ. 'TIME') RVAL(I) =  T0
 25   CONTINUE
      CALL CRDPUT ('Atmos', NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL FILIMGPU ('Atmos', ATMOOUT, ' ')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END

