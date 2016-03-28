C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdstat2.f	1.1	 5/4/93
C
      SUBROUTINE VSDSTAT2 (PDT)
C
CD Look at statistics of DR + DR* and DR x DR*
C
C	PDT	CH*(*)	inp	D Terms Database
C
C Audit trail:
C	New
C				M.A.Holdaway	Oct 16 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDSTAT2')
C
      INTEGER		NANT, NUMDINT, IINT
      INTEGER		DLADD, DRADD, DRWADD, DLWADD
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      COMPLEX		XZERO, DR1, DR2, DL1, DL2
      INTEGER		IA1, IA2, NAVE
      COMPLEX		RAVE, LAVE
      REAL		RDISP, LDISP
      CHARACTER*1	T
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
      CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, T, DRADD)
      CALL DATGETAR (STRM2(PDT, 'DL'), NAX, NAXIS, T, DLADD)
      IF (T .NE. 'X') THEN
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'D Terms not X')
         GOTO 999
      ENDIF
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
         RAVE = XZERO
         LAVE = XZERO
         RDISP = 0.0
         LDISP = 0.0
         NAVE = 0
         DO 570 IA1 = 1, NANT
            DR1 = MEMX(DRADD + NANT*(IINT-1) + IA1-1) 
            DL1 = MEMX(DLADD + NANT*(IINT-1) + IA1-1) 
            DO 560 IA2 = IA1+1, NANT
               DR2 = MEMX(DRADD + NANT*(IINT-1) + IA2-1)
               DL2 = MEMX(DLADD + NANT*(IINT-1) + IA2-1)
               IF ( MEMR(DRWADD +NANT*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $            MEMR(DRWADD +NANT*(IINT-1)+IA2-1) .GT. 0.0) THEN
                  NAVE = NAVE + 1
                  RAVE = RAVE + DR1 + CONJG(DR2)
                  LAVE = LAVE + DL1 + CONJG(DL2)
               ENDIF
 560        CONTINUE
 570     CONTINUE
         IF (NAVE .NE. 0 ) THEN
            RAVE = RAVE/ FLOAT(NAVE)
            LAVE = LAVE/ FLOAT(NAVE)
         ELSE
            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Valid Data')
            GOTO 999
         ENDIF
         DO 670 IA1 = 1, NANT
            DR1 = MEMX(DRADD + NANT*(IINT-1) + IA1-1) 
            DL1 = MEMX(DLADD + NANT*(IINT-1) + IA1-1) 
            DO 660 IA2 = IA1+1, NANT
               DR2 = MEMX(DRADD + NANT*(IINT-1) + IA2-1)
               DL2 = MEMX(DLADD + NANT*(IINT-1) + IA2-1)
               IF ( MEMR(DRWADD +NANT*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $            MEMR(DRWADD +NANT*(IINT-1)+IA2-1) .GT. 0.0) THEN
                  RDISP = RDISP + (CABS(DR1 + CONJG(DR2) -RAVE))**2
                  LDISP = LDISP + (CABS(DL1 + CONJG(DL2) -LAVE))**2
               ENDIF
 660        CONTINUE
 670     CONTINUE
C
         RDISP = SQRT( RDISP / NAVE )
         LDISP = SQRT( LDISP / NAVE )
C
         WRITE (MESSAGE, 1122) RAVE, RDISP
         CALL MSGPUT (MESSAGE, 'I')
         WRITE (MESSAGE, 1123) LAVE, LDISP
         CALL MSGPUT (MESSAGE, 'I')
 1122    FORMAT (' DR+DR* average: ',2F9.5,'   dispersion: ',F8.5)
 1123    FORMAT (' DL+DL* average: ',2F9.5,'   dispersion: ',F8.5)
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



