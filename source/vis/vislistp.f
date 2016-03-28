C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vislistp.f	1.4    6/5/93
C
      SUBROUTINE VISLISTP (HANDLE, VIS, BASL, TIME, WT, U, V, NVIS,
     $   NLIST, DOALL)
C
CD List visibility data
C
C
C	HANDLE	CH*(*)	input	Handle of output file for listing
C	VIS	CMPLX	input	Input visibilities
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	U, V	REAL(*)	input	U, V coordinates
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	NLIST	INT	input	Maximum number of records to list
C	DOALL	LOG	input	Use weights?
C Audit trail:
C	New routine
C				T.J.Cornwell	Jan 24 1989
C	NLIST & DOALL option added, fixed time output truncation
C				D.S.Briggs	25 May 1993
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NVIS, NLIST
      REAL		BASL(*), TIME(*), WT(*), U(*), V(*)
      COMPLEX		VIS(*)
      LOGICAL		DOALL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISLISTP')
C
      CHARACTER*12	STRTIMC, TSTRING
      INTEGER		IVIS, NCOR, IA1, IA2, ILIST
      REAL		AMP, PHASE, PI
      CHARACTER*132	LINE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = 4.0*ATAN(1.0)
      ILIST = 0
C
C Write heading
C
      MESSAGE =
     $   '    Time     Base         U               V            Amp'//
     $   '    Phase      Wt'
      CALL TXTWRITE (HANDLE, MESSAGE)
C
C Loop over data
C
      DO 100 IVIS = 1, NVIS
         IF ((.NOT.DOALL).AND.(WT(IVIS).LE.0.0)) GO TO 100
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         AMP = ABS(VIS(IVIS))
         IF (AMP.GT.0.0) THEN
            PHASE = 180.0 * ATAN2 (AIMAG(VIS(IVIS)), REAL(VIS(IVIS))) 
     1         / PI
         ELSE
            PHASE = 0.0
         END IF
         TSTRING = STRTIMC(TIME(IVIS))
         WRITE (LINE, 1000) TSTRING, IA1, IA2, U(IVIS), V(IVIS), 
     1      AMP, PHASE, WT(IVIS)
 1000    FORMAT (1X,A12,1X,I2,'-',I2,1X,2(1PG15.7E2,1X),0PG10.3E2,
     $      2(1X,F7.2))
         CALL TXTWRITE (HANDLE, LINE)
         ILIST = ILIST + 1
         IF (ILIST.GE.NLIST) GO TO 990
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
