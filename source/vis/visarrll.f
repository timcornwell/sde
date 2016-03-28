C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visarrll.f	1.1    5/4/93
C
      SUBROUTINE VISARRLL (VIS, CLASS, AVERR, AVELL)
C
CD Returns average RR and average LL amplitude
C
C	VIS	CH*(*)	inp	Vis name
C	CLASS	CH*(*)	inp	Class ('OBS', 'MOD')
C	AVERR	REAL	out	Average ABS(I+V)
C	AVELL	REAL	out	Average ABS(I-V)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 8 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	VIS, CLASS
      REAL		AVERR, AVELL
C
      CHARACTER*(*)	ROUTINE

      PARAMETER		(ROUTINE = 'VISARRLL')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), NAVE, I, NVIS
      INTEGER		IADD, VADD, IWTADD, VWTADD
      CHARACTER*1	TYPE
      CHARACTER*(SYSMXNAM)	STRM3
      INTEGER			DATADD
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM3(VIS, CLASS, 'I/VIS'), NAX, NAXIS, TYPE,
     $   IADD)
      IWTADD = DATADD (STRM3(VIS, CLASS, 'I/WT') )
      VADD = DATADD (STRM3(VIS, CLASS, 'V/VIS') )
      VWTADD = DATADD (STRM3(VIS, CLASS, 'V/WT') )
      NVIS = NAXIS(1)
C
      AVERR = 0.0
      AVELL = 0.0
      NAVE = 0
      DO 100 I = 0, NVIS-1
         IF (MEMR(IWTADD+I) .GT. 0.0 .AND. MEMR(VWTADD+I) .GT. 0.0) THEN
            NAVE = NAVE + 1
            AVERR = AVERR + CABS (MEMX(IADD) + MEMX(VADD))
            AVELL = AVELL + CABS (MEMX(IADD) - MEMX(VADD))
         ENDIF
 100  CONTINUE
C
      IF (NAVE .GT. 0) THEN
         AVERR = AVERR / NAVE
         AVELL = AVELL / NAVE
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'No Valid Data')
         GOTO 999
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
