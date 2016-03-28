C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pltphrms.f	1.3	 7/16/92
C
      SUBROUTINE PLTPHRMS (INDIR, COMMENT, OUTFILE)
C
CD Outputs a LOG-LOG plot of the highest time RMS vs UVDISTANCE
C
C	INDIR	CHAR*(*)	in	Directory with RMS info
C	COMMENT	CHAR*(*)	in	File of visibility's origin
C	OUTFILE	CHAR*(*)	in	Output Plot File
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	July 19 1991
C	Changed to Log base 10, other cosmetic changes
C				M.A.Holdaway	Nov 15 1991
C	Track changes to ARRPGPRF.  Some changes made to produce true
C	LOG-LOG plot.
C				D.S.Briggs	July 15 1992
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	INDIR, COMMENT, OUTFILE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PLTPHRMS')
C
      INTEGER		NAX, NAXIS(SYSMXDIM)

      CHARACTER*1	ATYPE
      INTEGER		MADD, UVADD, TADD, NT, NUV, IUV, IND, PHADD
      REAL		XMIN, XMAX, YMIN, YMAX
C
      CHARACTER*(SYSMXNAM)	STRM2
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2 (INDIR, 'MAT'), NAX, NAXIS, ATYPE, MADD)
      CALL DATGETAR (STRM2 (INDIR, 'TIME'), NAX, NT, ATYPE, TADD)
      CALL DATGETAR (STRM2 (INDIR, 'UV'), NAX, NUV, ATYPE, UVADD)
      CALL DATMAKAR ('PHRMS-PLTPHRMS', NAX, NUV, ATYPE, PHADD)
      IF (ERROR) GOTO 990
      IF (NT .NE. NAXIS(1) .OR. NUV .NE. NAXIS(2)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Inconsistent array sizes for phase RMS')
         GOTO 990
      ENDIF
C
      DO 200 IUV = 1, NUV
         IND = MADD + (IUV - 1) * NT + NT - 1
         MEMR(PHADD + IUV-1) = MEMR(IND)
 200  CONTINUE
C
      XMIN = 0.0001
      XMAX = 4.5
      YMIN = 0.0001
      YMAX = 2.5
      CALL ARRCLIP (STRM2(INDIR, 'UV'), 10.**XMIN, 10.**XMAX,
     $   'UV-PLTPHRMS')
      CALL ARRCLIP ('PHRMS-PLTPHRMS', 10.**YMIN, 10.**YMAX,
     $   'PHRMS-PLTPHRMS')
      CALL ARRPGGRF (NUV, 'UV-PLTPHRMS', 'PHRMS-PLTPHRMS',
     $   OUTFILE, 'Baseline, m', 'RMS Phase, deg',
     $   'Phase Structure Function - '//COMMENT, 1, 30, XMIN, XMAX, 
     $   YMIN, YMAX)
      IF (ERROR) GOTO 990
C
      CALL MSGPUT('Made RMS phase - UVdist plot: '//OUTFILE, 'I')
C      
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
