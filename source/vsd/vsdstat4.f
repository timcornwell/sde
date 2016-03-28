C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdstat4.f	1.1	 5/4/93
C
      SUBROUTINE VSDSTAT4 (PDT)
C
CD Look at statistics of DR x DR*
C
C	PDT	CH*(*)	inp	D Terms Database
C
C Audit trail:
C	New
C				M.A.Holdaway	4 Jan 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDSTAT4')
C
      INTEGER		NANT, NUMDINT, IINT
      INTEGER		DLADD, DRADD, DRWADD, DLWADD
      INTEGER		NAX, NAXIS(SYSMXDIM)
C
      COMPLEX		XZERO, DR1, DR2, DL1, DL2
      INTEGER		IA1, IA2, NAVE
      COMPLEX		RRAVE, LLAVE
      COMPLEX		RRSCAT, LLSCAT
      COMPLEX		RRSCATGLOB, LLSCATGLOB
      REAL		RRDISP, LLDISP
      REAL		RRAVEGLOB, LLAVEGLOB
      REAL		RRDISGLOB, LLDISGLOB
      CHARACTER*1	T
      INTEGER		NVIS, NGLOB
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
      NGLOB = 0
      XZERO = CMPLX(0.0, 0.0)
      RRAVEGLOB = 0.0
      LLAVEGLOB = 0.0
      RRDISGLOB = 0.0
      LLDISGLOB = 0.0
      RRSCATGLOB = XZERO
      LLSCATGLOB = XZERO
      DO 1030 IA1 = 1, NANT
         DO 1020 IA2 = 1, NANT
            IF (IA1 .EQ. IA2) GOTO 1020
            NAVE = 0
            RRAVE = XZERO
            RRDISP = 0.0
            LLAVE = XZERO
            LLDISP = 0.0
            RRSCAT = XZERO
            LLSCAT = XZERO
            DO 1000 IINT = 1, NUMDINT
               IF ( MEMR(DRWADD +NANT*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $            MEMR(DRWADD +NANT*(IINT-1)+IA2-1) .GT. 0.0) THEN
                  DR1 = MEMX(DRADD + NANT*(IINT-1) + IA1-1) 
                  DL1 = MEMX(DLADD + NANT*(IINT-1) + IA1-1) 
                  DR2 = MEMX(DRADD + NANT*(IINT-1) + IA2-1)
                  DL2 = MEMX(DLADD + NANT*(IINT-1) + IA2-1)
                  RRAVE = RRAVE + DR1 * CONJG (DR2)
                  LLAVE = LLAVE + DL1 * CONJG (DL2)
                  NAVE = NAVE + 1
               ENDIF
 1000       CONTINUE
            IF (NAVE .LE. 0 ) GOTO 1020
            RRAVE = RRAVE /FLOAT(NAVE)
            LLAVE = LLAVE /FLOAT(NAVE)
            NGLOB = NGLOB + 1
C
            DO 1010 IINT = 1, NUMDINT
               IF ( MEMR(DRWADD +NANT*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $            MEMR(DRWADD +NANT*(IINT-1)+IA2-1) .GT. 0.0) THEN
                  DR1 = MEMX(DRADD + NANT*(IINT-1) + IA1-1) 
                  DL1 = MEMX(DLADD + NANT*(IINT-1) + IA1-1) 
                  DR2 = MEMX(DRADD + NANT*(IINT-1) + IA2-1)
                  DL2 = MEMX(DLADD + NANT*(IINT-1) + IA2-1)
                  RRSCAT = RRSCAT + DR1 * CONJG (DR2) - RRAVE
                  LLSCAT = LLSCAT + DL1 * CONJG (DL2) - LLAVE
                  RRDISP = RRDISP + (RRAVE - DR1 * CONJG (DR2)) *
     $               CONJG (RRAVE - DR1 * CONJG (DR2))
                  LLDISP = LLDISP + (LLAVE - DL1 * CONJG (DL2)) *
     $               CONJG (LLAVE - DL1 * CONJG (DL2))
               ENDIF
 1010       CONTINUE
            RRDISP = ( RRDISP/FLOAT(NAVE) )**0.5
            LLDISP = ( LLDISP/FLOAT(NAVE) )**0.5
            RRSCAT = RRSCAT / FLOAT(NAVE)
            LLSCAT = LLSCAT / FLOAT(NAVE)
C
            RRAVEGLOB = RRAVEGLOB + CABS(RRAVE)
            LLAVEGLOB = LLAVEGLOB + CABS(LLAVE)
            RRDISGLOB = RRDISGLOB + RRDISP
            LLDISGLOB = LLDISGLOB + LLDISP
            RRSCATGLOB = RRSCATGLOB + RRSCAT
            LLSCATGLOB = LLSCATGLOB + LLSCAT
 1020    CONTINUE
 1030 CONTINUE
C
      RRAVEGLOB = RRAVEGLOB / FLOAT(NGLOB)
      LLAVEGLOB = LLAVEGLOB / FLOAT(NGLOB)
      RRDISGLOB = RRDISGLOB / FLOAT(NGLOB)
      LLDISGLOB = LLDISGLOB / FLOAT(NGLOB)
C
      WRITE (MESSAGE, 1876) RRAVEGLOB, LLAVEGLOB
 1876 FORMAT ('DrDr* Ave: ', F12.7, '   DlDl* Ave: ', F12.7)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE (MESSAGE, 1886) RRDISGLOB, LLDISGLOB
 1886 FORMAT ('DrDr* Disp: ', F12.7, '   DlDl* Disp: ', F12.7)
      CALL MSGPUT (MESSAGE, 'I')
C
C Form vector average now
C
      RRDISGLOB = CABS(RRSCATGLOB)
      LLDISGLOB = CABS(LLSCATGLOB)
      WRITE (MESSAGE, 1896) RRDISGLOB, LLDISGLOB
 1896 FORMAT ('DrDr* Vector Disp: ', F12.7, '   DlDl* Vector Disp: '
     $   , F12.7)
      CALL MSGPUT (MESSAGE, 'I')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



