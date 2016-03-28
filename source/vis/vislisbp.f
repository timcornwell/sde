C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vislisbp.f	1.7	 11/22/94
C
      SUBROUTINE VISLISBP (HANDLE, VIS, BASL, TIME, WT, U, V, NVIS,
     $   NLIST, DOALL)
C
CD List visibility data in baseline order
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
C				M.A.Holdaway	19 Sep 1991
C	NLIST & DOALL option added, fixed time output truncation
C				D.S.Briggs	25 May 1993
C	Fixed to also print autocorrelations
C				M.A. Holdaway	22 Nov 1994
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
      INTEGER		IVIS, NCOR, IA1, IA2, NANT, JA1, JA2, ORDER,
     $   		ILIST
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
     $   '    Base     Time         U               V            Amp'//
     $   '    Phase      Wt'
      CALL TXTWRITE (HANDLE, MESSAGE)
C
C Loop over data
C
      NANT = 0
      DO 50 IVIS = 1, NVIS
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         NANT = MAX (NANT, IA1)
         NANT = MAX (NANT, IA2)
 50   CONTINUE
C
      DO 300 IA1 = 1, NANT
         DO 200 IA2 = IA1, NANT
            DO 100 IVIS = 1, NVIS
               JA1 = NINT(BASL(IVIS)/256.0)
               JA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
               IF (IA1 .EQ. JA1 .AND. IA2 .EQ. JA2) THEN
                  ORDER = 1
               ELSE IF (IA1 .EQ. JA2 .AND. IA2 .EQ. JA1) THEN
                  ORDER = -1
               ELSE
                  ORDER = 0
               ENDIF
               IF ((ORDER.NE.0) .AND.
     $             (DOALL.OR.(WT(IVIS).GT.0.0))) THEN
                  AMP = ABS(VIS(IVIS))
                  IF (AMP.GT.0.0) THEN
                     PHASE = ORDER * 180.0 * ATAN2 (AIMAG(VIS(IVIS)), 
     $                  REAL(VIS(IVIS))) / PI
                  ELSE
                     PHASE = 0.0
                  END IF
                  TSTRING = STRTIMC(TIME(IVIS))
                  WRITE (LINE, 1000) JA1, JA2, TSTRING, U(IVIS), 
     1               V(IVIS), AMP, PHASE, WT(IVIS)
 1000             FORMAT (1X,I2,'-',I2,A12,1X,2(1PG15.7E2,1X),
     $               0PG12.6E2, 2(1X,F9.4))
                  CALL TXTWRITE (HANDLE, LINE)
                  ILIST = ILIST + 1
                  IF (ILIST.GE.NLIST) GO TO 990
               ENDIF
 100        CONTINUE
 200     CONTINUE
 300  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
