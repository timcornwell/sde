C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)trplistp.f	1.3	 7/20/92
C
      SUBROUTINE TRPLISTP (HANDLE, TRP, TRIPLE, TIME, WT, U1,
     $   V1, U2, V2, NTRP)
C
C List triple product data: called by TRPLIST
C
C     $   U1, U2, U2, V2, NTRP)
C
C	HANDLE	CH*(*)	input	Handle of output file for listing
C	TRP	CMPLX	input	Input triple product data
C	TRIPLE	REAL(*)	input	triple of antennas
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	U1, V1	REAL(*)	input	U, V coordinates
C	U2, V2	REAL(*)	input	U, V coordinates
C	NTRP	INT	input	Number of triple products
C Audit trail:
C	New routine
C				T.J.Cornwell	March 20 1989
C      Changed to triples
C                              T.J. Cornwell   March 30 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	HANDLE
      INTEGER		NTRP
      REAL		TRIPLE(*), TIME(*), WT(*)
      REAL              U1(*), V1(*), U2(*), V2(*)
      COMPLEX		TRP(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TRPLISTP')
C
      CHARACTER*12	STRTIMC, TSTRING
      INTEGER		ITRP, NCOR, IA1, IA2, IA3
      REAL		AMP, PHASE, PI
      CHARACTER*132	LINE
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = 4.0*ATAN(1.0)
C
C Write heading
C
      MESSAGE =
     $   '    Time      Base           U1             V1           '//
     $   '   U2              V2         Amp        Phase     Wt'
      CALL TXTWRITE (HANDLE, MESSAGE)
C
C Loop over data
C
      DO 100 ITRP = 1, NTRP
         IA1 = NINT(TRIPLE(ITRP))/256**2
         IA2 = NINT(TRIPLE(ITRP)-FLOAT(256**2*IA1))/256.0
         IA3 = NINT(TRIPLE(ITRP)-FLOAT(256**2*IA1+256*IA2))
         AMP = ABS(TRP(ITRP))
         IF (AMP.GT.0.0) THEN
            PHASE = 180.0 * ATAN2 (AIMAG(TRP(ITRP)), REAL(TRP(ITRP))) 
     1         / PI
         ELSE
            PHASE = 0.0
         END IF
         TSTRING = STRTIMC(TIME(ITRP))
         WRITE (LINE, 1000) TSTRING, IA1, IA2, IA3, U1(ITRP), V1(ITRP),
     $      U2(ITRP), V2(ITRP), AMP, PHASE, WT(ITRP)
 1000    FORMAT (A11,1X,I2,'-',I2,'-',I2,1X,4(1PG15.7E2,1X),0PG10.3E2,
     $      2(1X,F7.2))
         CALL TXTWRITE (HANDLE, LINE)
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
