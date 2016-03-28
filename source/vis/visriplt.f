C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visriplt.f	1.1	 12/22/92
C
       SUBROUTINE VISRIPLT (NAME, SUB, DEVRI, ANT1, ANT2, STOKES, STYLE)
C
CD Plot real part vs imaginary part of visibilities 
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	DEVRI	CH*(*)	input	Praphics device
C	ANT1	INT	input	Antenna 1
C	ANT2	INT	input	Antenna 2
C	STOKES, CH*(*)	input	Label for plotting
C	STYLE	INT	input	Style for characters in plot
C	
C
C Audit trail:
C	New routine
C				M.A.Holdaway	Dec 21 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, DEVRI, STOKES
       INTEGER		ANT1, ANT2, STYLE
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRIPLT')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD, 
     1			BADD, WTADD, TADD, VSADD, NVIS,
     3			IVIS, KVIS, DRADD, DIADD, DWADD, IA1, IA2
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      NVIS = NAXIS(1)
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      TADD = DATADD (STRM2(NAME, 'TIME'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL DATMAKAR ('DummyReal', 1, NVIS, 'R', DRADD)
      CALL DATMAKAR ('DummyImag', 1, NVIS, 'R', DIADD)
      CALL DATMAKAR ('DummyWeights', 1, NVIS, 'R', DWADD)
C
      KVIS = 0
      DO 100 IVIS = 0, NVIS-1
         IA1 = NINT( MEMR(BADD+IVIS) /256.0)
         IA2 = NINT( MEMR(BADD+IVIS)-FLOAT(256*IA1))
         IF ( (IA1 .EQ. ANT1 .AND. IA2 .EQ. ANT2) .OR.
     $      (IA2 .EQ. ANT1 .AND. IA1 .EQ. ANT2) ) THEN
            MEMR(DRADD+KVIS) =  REAL (MEMX (VSADD+IVIS))
            MEMR(DIADD+KVIS) =  AIMAG (MEMX (VSADD+IVIS))
            MEMR(DWADD+KVIS) =  MEMR (WTADD+IVIS)
            KVIS = KVIS + 1
         ENDIF
 100  CONTINUE
C
      WRITE (MESSAGE, 1212) STOKES, ANT1, ANT2
 1212 FORMAT (A2, ' Vis for ants ',2I4)
      CALL PLTDPTS (KVIS, MEMR(DRADD), MEMR(DIADD), MEMR(DWADD),
     $   DEVRI, 1, .TRUE., 0.0, 0.0, MESSAGE)
C
      CALL DATDELET ('DummyReal')
      CALL DATDELET ('DummyImag')
      CALL DATDELET ('DummyWeights')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
