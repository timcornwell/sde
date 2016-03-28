C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vsdaput.f	1.1	 5/4/93
C
      SUBROUTINE VSDAPUT (PDT, FILE)
C
CD Write out polarization D Terms
C
C	PDT	CH*(*)	inp	D Terms Database
C	FILE	CH*(*)	inp	Output Asci File
C
C Audit trail:
C	New
C				M.A.Holdaway	Oct 16 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	PDT, FILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VSDAPUT')
C
      INTEGER		NANT, NUMDINT, IINT, IANT
      INTEGER		DLADD, DRADD, DTADD
      INTEGER		RMSADD
      CHARACTER*1	ATYPE
      INTEGER		NAX, NAXIS(SYSMXDIM), NOTHERE
      PARAMETER		(NOTHERE = -9999)
C
      REAL		D2R, DRAMP, DLAMP, DRPHA, DLPHA
      REAL		DRRMS, DLRMS
      COMPLEX		DR, DL, DRREF, DRREFC
      INTEGER			DATADD
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL			DATEXIST
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      D2R = ATAN(1.0)/45.0
      CALL DATGETAR (STRM2(PDT, 'DR'), NAX, NAXIS, 
     1   ATYPE, DRADD)
      NANT = NAXIS(1)
      NUMDINT = NAXIS(2)
      DLADD = DATADD (STRM2(PDT, 'DL'))
      DTADD = DATADD (STRM2(PDT, 'DTIME'))
      IF (DATEXIST(STRM2(PDT, 'DRLRMS'))) THEN
         RMSADD = DATADD (STRM2(PDT, 'DRLRMS'))
      ELSE
         RMSADD = NOTHERE
         DRRMS = 0.0
         DLRMS = 0.0
      ENDIF
C
      MESSAGE = 'Writing DTERMS to ASCI file '//FILE
      CALL MSGPUT (MESSAGE, 'I')
      CALL TXTOPEN (ROUTINE, FILE, 'WRITE')
      WRITE (MESSAGE, 1100) NANT, NUMDINT
 1100 FORMAT (1X,2I5)
      CALL TXTWRITE (ROUTINE, MESSAGE)
      DO 550 IINT = 1, NUMDINT
         WRITE (MESSAGE, 1110) MEMR(DTADD + IINT-1)
 1110    FORMAT (1X, F13.10)
         CALL TXTWRITE (ROUTINE, MESSAGE)
         WRITE (MESSAGE, 1111) IINT
 1111    FORMAT ('D Terms,  Integration ',I3)
         CALL TXTWRITE (ROUTINE, MESSAGE)
         MESSAGE = 'ANT    DR     Phase     RMS  '//
     $               '     DL     Phase     RMS  '
         CALL TXTWRITE (ROUTINE, MESSAGE)
C         DRREF = MEMX(DRADD + NANT*(IINT-1) + 3-1)
C         DRREFC = CONJG (DRREF)
         DRREF = CMPLX(0.0, 0.0)
         DRREFC = CMPLX(0.0, 0.0)
         DO 530 IANT = 1, NANT
            DR = MEMX(DRADD + NANT*(IINT-1) + IANT-1) - DRREF
            DL = MEMX(DLADD + NANT*(IINT-1) + IANT-1) + DRREFC
            DRAMP = CABS ( DR )
            DLAMP = CABS ( DL )
            DRPHA = ATAN2 (AIMAG(DR), REAL(DR)) / D2R
            DLPHA = ATAN2 (AIMAG(DL), REAL(DL)) / D2R
            IF (RMSADD .NE. NOTHERE) THEN
               DRRMS = MEMR (RMSADD + 2*NANT*(IINT-1) +IANT-1)
               DLRMS = MEMR (RMSADD + NANT*(IINT-1) +NANT+IANT-1)
            ENDIF
C
            WRITE(MESSAGE, 1113) IANT, DRAMP, DRPHA, DRRMS, DLAMP, 
     $         DLPHA, DLRMS
 1113       FORMAT (I3, 2(1X,F8.5,1X,F7.1,F9.5))
            CALL TXTWRITE (ROUTINE, MESSAGE)
 530     CONTINUE
 550  CONTINUE
C
      CALL TXTCLOSE (ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



