C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrwtdt.f	1.2    11/7/94
C
       SUBROUTINE VISRWTDT (NAME, SUB, ANT, RWT)
C
CD Reweight pos weighted visibilites according to DIAM and DT
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	ANT	CH*(*)	input	Antenna directory
C	ANT/DIAM REAL(*) input	Antenna diameters
C	RWT	REAL(3)	input	reweighting constants
C
C Positive weights are modified via WTOUT = WTIN * RWT(1) +
C (DIAM(I)*DIAM(J))**2 * RWT(2) + DT * RWT(3)
C
C DT is in seconds, and is determined from the visbility header or the
C vis data itself.
C
C DIAM is is meters
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Oct 31 1993
C	Ooops!  The weights should be proportional to (D2*D2)**2
C				D.S.Briggs	Oct 7 1994
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, SUB, ANT
      REAL		RWT(3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRWTDT')
C
      INTEGER		I, NAX, NAXIS(SYSMXDIM), DATADD,
     1			WTADD, DADD, BADD, NANT, NWT, IA1, IA2
      REAL		DT, PDIAM2, BASL
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'WT'), NAX, NAXIS, ATYPE, WTADD)
      NWT = NAXIS(1)
      BADD = DATADD(STRM2(NAME, 'BASELINE'))
      CALL DATGETAR (STRM2(ANT,'DIAM'), NAX, NAXIS, ATYPE, DADD)
      NANT = NAXIS(1)
      IF (ERROR) GO TO 999
C
C Assume native time units are days
C
      CALL VISGTINT (NAME, DT)
      DT = DT * 3600.D0 * 24.D0
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) DT
 1000    FORMAT ('Integration time from data is',E12.4)
         CALL MSGPUT (MESSAGE,'I')
      END IF
C
      DO 100 I = 1, NWT
         BASL = MEMR(BADD+I-1)
         IA1 = NINT(BASL/256.0)
         IA2 = NINT(BASL-FLOAT(256*IA1))
         IF ((IA1.GT.NANT).OR.(IA2.GT.NANT)) THEN
            CALL ERRREPOR (ERRFATAL, ROUTINE, 'Bad antenna number')
            GO TO 999
         END IF
         PDIAM2 = (MEMD(DADD+IA1-1)*MEMD(DADD+IA2-1))**2
         MEMR(WTADD+I-1) = MEMR(WTADD+I-1) * RWT(1) +
     $      PDIAM2 * RWT(2) + DT * RWT(3)
 100  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
