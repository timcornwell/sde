C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdstat3.f	1.1	 5/4/93
C
      SUBROUTINE VSDSTAT3 (PDT)
C
CD Look at the COMPLEX AVERAGE of DR + DL*
C
C	PDT	CH*(*)	inp	D Terms Database
C
C Audit trail:
C	New
C				M.A.Holdaway	Nov 24 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDSTAT3')
C
      INTEGER		NANT, NUMDINT, IINT
      INTEGER		DLADD, DRADD, DRWADD, DLWADD, TADD
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      COMPLEX		XZERO, DR1, DL2
      INTEGER		IA1, IA2, NAVE
      COMPLEX		AVE
      CHARACTER*1	T
      REAL		AAVE, PAVE, D2R, TIME
      INTEGER		NVIS
C
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL			DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      D2R = ATAN2(1.0, 1.0)  / 45.0
C
      CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, T, DRADD)
      CALL DATGETAR (STRM2(PDT, 'DL'), NAX, NAXIS, T, DLADD)
      IF (T .NE. 'X') THEN
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'D Terms not X')
         GOTO 999
      ENDIF
      CALL DATGETAR (STRM2(PDT,'DTIME'), NAX, NAXIS, T, TADD)
      IF (DATEXIST (STRM2(PDT, 'DRW')) ) THEN      
         CALL DATGETAR (STRM2(PDT, 'DRW'), NAX, NAXIS, T, DRWADD)
         CALL DATGETAR (STRM2(PDT, 'DLW'), NAX, NAXIS, T, DLWADD)
      ELSE
         CALL DATMAKAR (STRM2(PDT, 'DRW'), NAX, NAXIS, 'R', DRWADD)
         CALL DATMAKAR (STRM2(PDT, 'DLW'), NAX, NAXIS, 'R', DLWADD)
         CALL ARRSETCO (STRM2(PDT, 'DRW') , 0.0, 1.0)
         CALL ARRSETCO (STRM2(PDT, 'DLW') , 0.0, 1.0)
      ENDIF
C
      NANT = NAXIS(1)
      NUMDINT = NAXIS(2)
      NVIS = NANT * (NANT-1)
C
      XZERO = CMPLX(0.0, 0.0)
      DO 1000 IINT = 1, NUMDINT
         TIME = MEMR(TADD + IINT-1)
         AVE = XZERO
         NAVE = 0
         DO 570 IA1 = 1, NANT
            DR1 = MEMX(DRADD + NANT*(IINT-1) + IA1-1) 
            DO 560 IA2 = 1, NANT
               IF (IA1 .NE. IA2) THEN
                  DL2 = MEMX(DLADD + NANT*(IINT-1) + IA2-1)
                  IF ( MEMR(DRWADD +NANT*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $               MEMR(DRWADD +NANT*(IINT-1)+IA2-1) .GT. 0.0) THEN
                     NAVE = NAVE + 1
                     AVE = AVE + DR1 + CONJG(DL2)
                  ENDIF
               ENDIF
 560        CONTINUE
 570     CONTINUE
         IF (NAVE .NE. 0 ) THEN
            AVE = AVE/ FLOAT(NAVE)
         ELSE
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Valid Data')
            GOTO 999
         ENDIF
         AAVE = CABS(AVE)
         PAVE = ATAN2(IMAG(AVE), REAL(AVE)) / D2R
C
         WRITE (MESSAGE, 1122) AAVE, PAVE, TIME
         CALL MSGPUT (MESSAGE, 'I')
 1122    FORMAT (' Complex average: ',2F12.5,'  Time: ',F10.5)
C
 1000 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



