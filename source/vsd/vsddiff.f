C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsddiff.f	1.1	 5/4/93
C
      SUBROUTINE VSDDIFF (D1, D2, D3)
C
CD Difference D Terms:  D3 = D1 - D2
C
C	D1	CH*(*)	inp	One D Terms Database
C	D2	CH*(*)	inp	Other D Terms Database
C	D3	CH*(*)	inp	Difference D Terms Database
C
C Audit trail:
C	New
C				M.A.Holdaway	April 27 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	D1, D2, D3
      REAL		SAVE, VAVE, SDIF, VDIF
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDDIFF')
C
      INTEGER		NANT, NUMDINT, IINT
      INTEGER		DL1ADD, DR1ADD, DR1WADD, DL1WADD
      INTEGER		DL2ADD, DR2ADD, DR2WADD, DL2WADD
      INTEGER		DL3ADD, DR3ADD, DR3WADD, DL3WADD
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		NAXIS1(SYSMXDIM),NAXIS2(SYSMXDIM),
     $   		NAXIS3(SYSMXDIM)
C
      COMPLEX		XZERO, DR1I, DR1J, DL1I, DL1J
      COMPLEX		DR2I, DR2J, DL2I, DL2J
      INTEGER		IA1, IA2, NAVE
      COMPLEX		XAVE, XDIF
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
      CALL DATGETAR (STRM2(D1, 'DR'), NAX, NAXIS1, T, DR1ADD)
      CALL DATGETAR (STRM2(D1, 'DL'), NAX, NAXIS1, T, DL1ADD)
      CALL DATGETAR (STRM2(D2, 'DR'), NAX, NAXIS2, T, DR2ADD)
      CALL DATGETAR (STRM2(D2, 'DL'), NAX, NAXIS2, T, DL2ADD)
      CALL DATGETAR (STRM2(D3, 'DR'), NAX, NAXIS3, T, DR3ADD)
      CALL DATGETAR (STRM2(D3, 'DL'), NAX, NAXIS3, T, DL3ADD)
      IF (T .NE. 'X') THEN
         CALL ERRREPOR( ERRBDARG, ROUTINE, 'D Terms not X')
         GOTO 999
      ENDIF
      IF (DATEXIST (STRM2(D1, 'DRW')) ) THEN      
         CALL DATGETAR (STRM2(D1, 'DRW'), NAX, NAXIS, T, DR1WADD)
         CALL DATGETAR (STRM2(D1, 'DLW'), NAX, NAXIS, T, DL1WADD)
         CALL DATGETAR (STRM2(D2, 'DRW'), NAX, NAXIS, T, DR2WADD)
         CALL DATGETAR (STRM2(D2, 'DLW'), NAX, NAXIS, T, DL2WADD)
         CALL DATGETAR (STRM2(D3, 'DRW'), NAX, NAXIS, T, DR3WADD)
         CALL DATGETAR (STRM2(D3, 'DLW'), NAX, NAXIS, T, DL3WADD)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Need weights to run this program: DTERMS.SDE')
         GOTO 990
      ENDIF
      IF (ERROR) GOTO 990
C
      CALL PIXRSETC (MEMX(DR3ADD), 0.0, 0.0)
      CALL PIXRSETC (MEMX(DL3ADD), 0.0, 0.0)
      CALL PIXRSETC (MEMR(DR3WADD), 0.0, 0.0)
      CALL PIXRSETC (MEMR(DL3WADD), 0.0, 0.0)
      NANT = MIN(NAXIS1(1), NAXIS2(1))
      NUMDINT = MIN(NAXIS1(2), NAXIS2(2))
      NVIS = NANT * (NANT-1)
C
      CALL MSGPUT ('Int  Scal Ave    Scal Diff   Vect Ave   Vect Diff',
     $   'I')
      XZERO = CMPLX(0.0, 0.0)
      DO 1000 IINT = 1, NUMDINT
         XAVE = XZERO
         XDIF = XZERO
         SAVE = 0.0
         SDIF = 0.0
         NAVE = 0
         DO 570 IA1 = 1, NANT
            DR1I = MEMX(DR1ADD + NAXIS1(1)*(IINT-1) + IA1-1) 
            DL1I = MEMX(DL1ADD + NAXIS1(1)*(IINT-1) + IA1-1) 
            DR2I = MEMX(DR2ADD + NAXIS2(1)*(IINT-1) + IA1-1) 
            DL2I = MEMX(DL2ADD + NAXIS2(1)*(IINT-1) + IA1-1) 
            IF ( MEMR(DR1WADD+NAXIS1(1)*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $           MEMR(DR2WADD+NAXIS2(1)*(IINT-1)+IA1-1) .GT. 0.0) THEN
               MEMX(DR3ADD + NAXIS3(1)*(IINT-1) + IA1-1) = DR1I-DR2I
               MEMR(DR3WADD +NAXIS3(1)*(IINT-1) + IA1-1) = 1.0
            ELSE
               MEMX(DR3ADD + NAXIS3(1)*(IINT-1) + IA1-1) = 0.0
               MEMR(DR3WADD+ NAXIS3(1)*(IINT-1) + IA1-1) = 0.0
            ENDIF
            IF ( MEMR(DL1WADD+NAXIS1(1)*(IINT-1)+IA1-1) .GT. 0.0 .AND.
     $           MEMR(DL2WADD+NAXIS2(1)*(IINT-1)+IA1-1) .GT. 0.0) THEN
               MEMX(DL3ADD + NAXIS3(1)*(IINT-1) + IA1-1) = DL1I-DL2I
               MEMR(DL3WADD +NAXIS3(1)*(IINT-1) + IA1-1) = 1.0
            ELSE
               MEMX(DL3ADD + NAXIS3(1)*(IINT-1) + IA1-1) = 0.0
               MEMR(DL3WADD+ NAXIS3(1)*(IINT-1) + IA1-1) = 0.0
            ENDIF
C
C            DO 560 IA2 = IA1+1, NANT
C               DR1J = MEMX(DR1ADD + NAXIS1(1)*(IINT-1) + IA2-1)
C               DL1J = MEMX(DL1ADD + NAXIS1(1)*(IINT-1) + IA2-1)
C               DR2J = MEMX(DR2ADD + NAXIS2(1)*(IINT-1) + IA2-1)
C               DL2J = MEMX(DL2ADD + NAXIS2(1)*(IINT-1) + IA2-1)
C               IF (     MEMR(DR1WADD+NAXIS1(1)*(IINT-1)+IA1-1) .GT. 0.0
C     $            .AND. MEMR(DR1WADD+NAXIS1(1)*(IINT-1)+IA2-1) .GT. 0.0
C     $            .AND. MEMR(DR2WADD+NAXIS2(1)*(IINT-1)+IA1-1) .GT. 0.0
C     $            .AND. MEMR(DR2WADD+NAXIS2(1)*(IINT-1)+IA2-1) .GT. 0.0)
C     $            THEN
C                  NAVE = NAVE + 1
C                  XAVE = XAVE + (DR1I + CONJG(DL1J)
C     $                        +  DR1J + CONJG(DL1I)
C     $                        +  DR2I + CONJG(DL2J)
C     $                        +  DR2J + CONJG(DL2I)) / 4.0 
C                  SAVE = SAVE + ( CABS (DR1I + CONJG(DL1J))
C     $                        +   CABS (DR1J + CONJG(DL1I))
C     $                        +   CABS (DR2I + CONJG(DL2J))
C     $                        +   CABS (DR2J + CONJG(DL2I)) ) / 4.0 
C                  XDIF = XDIF + (DR1I + CONJG(DL1J) 
C     $                        -  DR2I + CONJG(DL2J)
C     $                        +  DR1J + CONJG(DL1I)
C     $                        -  DR2J + CONJG(DL2I)) / 2.0
C                  SDIF = SDIF + (CABS(DR1I + CONJG(DL1J) 
C     $                             -  DR2I + CONJG(DL2J))
C     $                        +  CABS(DR1J + CONJG(DL1I)
C     $                             -  DR2J + CONJG(DL2I))) / 2.0
C               ENDIF
C 560        CONTINUE
 570     CONTINUE
C         IF (NAVE .NE. 0 ) THEN
C            XAVE = XAVE / FLOAT(NAVE)
C            XDIF = XDIF / FLOAT(NAVE)
C            VAVE = CABS(XAVE)
C            VDIF = CABS(XDIF)
C            SAVE = SAVE / FLOAT(NAVE)
C            SDIF = SDIF / FLOAT(NAVE)
C            WRITE (MESSAGE, 1010) IINT, SAVE, SDIF, VAVE, VDIF
C 1010       FORMAT (I4, 4F12.8)
C            CALL MSGPUT (MESSAGE, 'I')
C
C         ELSE
C            CALL ERRREPOR (ERRLOGIC, ROUTINE, 'No Valid Data')
C            GOTO 999
C         ENDIF
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



