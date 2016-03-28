C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)filputpf.f	1.1	 3/15/91
C
      SUBROUTINE FILPUTPF (PLOT, PLTFILE)
C
C Write x,y plot data from an ASCII file
C
C	PLOT	CH*(*)	input	Name of directory entry
C	PLTFILE	CH*(*)	input	Name of input file
C
C	PLOT FILE DATABASE FORMAT:
C
C	PLOT/X		real		X data
C	PLOT/Y		real		Y data
C	PLOT/COMMENT	char		comment
C	PLOT/LABEL	char		not used yet
C	PLOT/DISCRETE	logical		plots points if TRUE, lines in FALSE
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 12 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	PLOT, PLTFILE
      LOGICAL		DISCRETE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FILPUTPF')
C
      INTEGER		NAXX, NAXY, NAXISX(SYSMXDIM),
     $   		NAXISY(SYSMXDIM)
      CHARACTER*1	ATYPEX, ATYPEY
      INTEGER		NP, IP, XADD, YADD, NDUMMY
      CHARACTER*(132)	LABEL, COMMENT, LINE, STRM2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (STRM2(PLOT, 'X'), NAXX, NAXISX, ATYPEX, XADD)
      CALL DATGETAR (STRM2(PLOT, 'Y'), NAXY, NAXISY, ATYPEY, YADD)
      IF (ERROR) GO TO 999
C
C check for cases we dont do
C
      IF (NAXX .GT. 1 .OR. NAXY .GT. 1) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Too many axes')
         GOTO 990
      ELSE IF (NAXISX(1) .NE. NAXISY(1)) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Axes Disagree')
         GOTO 990
      ELSE IF (ATYPEX .NE. 'R' .OR. ATYPEY .NE. 'R') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Only plots REAL')
         GOTO 990
      ENDIF
      NP = NAXISX(1)
C
C Now write the file
C
      CALL TXTOPEN (ROUTINE, PLTFILE, 'WRITE')
      IF (ERROR) GO TO 990
      CALL DATGETC (PLOT, 'COMMENT', LINE, 1, NDUMMY)
      COMMENT = LINE(1:64)
      CALL DATGETC (PLOT, 'LABEL', LINE, 1, NDUMMY)
      LABEL = LINE(1:64)
      CALL DATGETL (PLOT, 'DISCRETE', DISCRETE, 1, NDUMMY)
      IF (ERROR) GO TO 990
C
      CALL TXTWRITE (ROUTINE, COMMENT)
      CALL TXTWRITE (ROUTINE, LABEL)
      IF (DISCRETE) THEN
         CALL TXTWRITE (ROUTINE, 'D')
      ELSE
         CALL TXTWRITE (ROUTINE, 'C')
      ENDIF
C
      DO 100 IP = 0, NP - 1
         WRITE (LINE, 9001) MEMR(XADD + IP), MEMR(YADD + IP)
         CALL TXTWRITE (ROUTINE, LINE)
 100  CONTINUE
 9001 FORMAT (2(4X,E14.7))
C
      CALL TXTCLOSE(ROUTINE)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
