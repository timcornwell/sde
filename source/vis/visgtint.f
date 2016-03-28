C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visgtint.f	1.1    12/27/91
C
      SUBROUTINE VISGTINT (NAME, TINT)
C
CD Find the integration time in a visibility data base
C
C	NAME	CH*(*)	input	Name of directory entry
C	TINT	REAL	output	Integration time
C
C If the integration time must be calculated, it is stored in
C NAME/STAT/TINT for future reference.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	7-Oct-91
C	Revised to cooperate with VISMXANT
C				D.S.Briggs	28-Oct-91
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	NAME
      REAL		TINT
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISGTINT')
C
      INTEGER           NAX, NAXIS(SYSMXDIM), BADD, TADD, WADD, DTADD,
     $   		TDTADD, NPTS, NDT, MINANT, MAXANT, I
      CHARACTER*1       ATYPE
C
      LOGICAL		DATEXIST
      REAL		DATFGETR
      CHARACTER*(SYSMXNAM)      STRM2
C=======================================================================
      IF (ERROR) GO TO 999
C
C Assume that any Integration time found in the DB is correct
C
      IF (DATEXIST(STRM2(NAME,'TINT'))) THEN
         TINT = DATFGETR(NAME,'TINT')
         GO TO 990
      END IF
C
      CALL VISMXANT (NAME, MINANT, MAXANT)
C
C Find arrays (assume 1D)
C
      CALL DATGETAR (STRM2(NAME,'BASELINE'), NAX, NAXIS, ATYPE, BADD)
      CALL DATGETAR (STRM2(NAME,'TIME'), NAX, NAXIS, ATYPE, TADD)
      NPTS = NAXIS(1)
      IF (ERROR) GO TO 990
C
C Make work arrays that we will need.
C
      CALL DATMAKAR ('Delta Time', NAX, NAXIS, 'R', DTADD)
      NAX = 2
      NAXIS(1) = MAXANT
      NAXIS(2) = MAXANT
      CALL DATMAKAR ('Work Array', NAX, NAXIS, 'R', WADD)
      IF (ERROR) GO TO 990
C
C Do the majority of the hard work.  (Fill 'Delta Time' array)
C
      CALL VISGTIP (MEMR(TADD), MEMR(BADD), NPTS, MEMR(DTADD), NDT,
     $   MEMR(WADD), MAXANT)
      IF (ERROR) GO TO 990
C
C Trim the delta time array down to the actual number found.
C
      NAX = 1
      NAXIS(1) = NDT
      CALL DATMAKAR ('Trim Delta Time', NAX, NAXIS, 'R', TDTADD)
      DO 100 I = 0, NDT-1
         MEMR(TDTADD+I) = MEMR(DTADD+I)
 100  CONTINUE
C
C Find the MODE and pass it back
C
      CALL ARRMODE ('Trim Delta Time', .01)
      IF (ERROR) GO TO 990
      TINT = DATFGETR('Trim Delta Time','MODE')
      CALL DATPUTR (NAME, 'TINT', TINT, 1)
C
C Clean up and go home
C
      CALL DATDELAR('Work Array')
      CALL DATDELAR('Delta Time')
      CALL DATDELAR('Trim Delta Time')
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
