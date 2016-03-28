C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixxstat.f	1.5    10/21/94
C
      SUBROUTINE PIXXSTAT (A, N1, N2, N3, N4, N5, N6, N7,
     $   BLC, TRC, DMAX, DMIN, AVE, RMS, SUM, DISP, NLOC, BBBLC, BBTRC,
     $   MINLOC, MAXLOC)
C
CD Calculate array statistics of COMPLEX array only
C
C	A	COMPLEX	input	Input array
C	NAXIS	INT	input	Number of elements
C	BLC	INT	input	Start
C	TRC	INT	input	Stop
C	DMAX	REAL	output	Maximum of array
C	DMIN	REAL	output	Minimum of array
C	AVE	REAL	output	Average of array
C	RMS	REAL	output	RMS of array
C	SUM	REAL	output	SUM of array
C	DISP	REAL	output	DISPERSION of array
C	NLOC	INT	output	Number of non-zero pixels
C	BBBRC	INT	output	Bounding Box of non-zero pixels (BLC)
C	BBTRC	INT	output	Bounding Box of non-zero-pixels (TRC)
C	MINLOC	INT	output	Location of minimum value
C	MAXLOC	INT	output	Location of maximum value
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Added Sum capability
C				T.J.Cornwell	Oct 23 1989
C	Added DISPERSION, fixed a bug in MIN, MAX
C                               M.A.Holdaway    Feb 2 1990
C	Added NLOC, BBBLC & BBTRC.
C				D.S.Briggs	Feb 27 1992
C	.... (and screwed it up)  Fixed now.
C				D.S.Briggs	Mar 15 1992
C	Added MINLOC and MAXLOC
C				D.S.Briggs	Oct 21 1994
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		N1, N2, N3, N4, N5, N6, N7, NLOC
      INTEGER		BLC(*), TRC(*), BBBLC(*), BBTRC(*),
     $   		MINLOC(*), MAXLOC(*)
      REAL 		DMAX, DMIN, AVE, RMS, SUM, DISP, ST
      COMPLEX		A(N1,N2,N3,N4,N5,N6,N7), T
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXXSTAT')
C
      INTEGER		NAX, I1, I2, I3, I4, I5, I6, I7, ANPIX
C=====================================================================
C
C Initialize output numbers
C
      ANPIX = 0
      AVE = 0.0
      RMS = 0.0
      SUM = 0.0
      DISP = 0.0
      NLOC = 0
      DMAX = ABS(A(BLC(1),BLC(2),BLC(3),BLC(4),BLC(5),BLC(6),BLC(7)))
      DMIN = ABS(A(BLC(1),BLC(2),BLC(3),BLC(4),BLC(5),BLC(6),BLC(7)))
      NAX = 1
      IF (N2.GT.1) NAX = 2
      IF (N3.GT.1) NAX = 3
      IF (N4.GT.1) NAX = 4
      IF (N5.GT.1) NAX = 5
      IF (N6.GT.1) NAX = 6
      IF (N7.GT.1) NAX = 7
      DO 5 I1 = 1, NAX
         BBTRC(I1) = -1000000
         BBBLC(I1) = 1000000
         MINLOC(I1) = BLC(I1)
         MAXLOC(I1) = BLC(I1)
 5    CONTINUE
      DO 6 I1 = NAX+1, SYSMXDIM
         BBTRC(I1) = 1
         BBBLC(I1) = 1
         MINLOC(I1) = BLC(I1)
         MAXLOC(I1) = BLC(I1)
 6    CONTINUE
C
C Return on input error
C
      IF (ERROR) GO TO 999
C
      DO 70 I7 = BLC(7), TRC(7)
         DO 60 I6 = BLC(6), TRC(6)
            DO 50 I5 = BLC(5), TRC(5)
               DO 40 I4 = BLC(4), TRC(4)
                  DO 30 I3 = BLC(3), TRC(3)
                     DO 20 I2 = BLC(2), TRC(2)
                        DO 10 I1 = BLC(1), TRC(1)
                           T = A(I1,I2,I3,I4,I5,I6,I7)
                           ST = ABS(T)
                           ANPIX = ANPIX + 1
                           AVE = AVE + ST
                           RMS = RMS + ST**2
                           IF (ST.GT.DMAX) THEN
                              DMAX = ST
                              MAXLOC(1) = I1
                              MAXLOC(2) = I2
                              MAXLOC(3) = I3
                              MAXLOC(4) = I4
                              MAXLOC(5) = I5
                              MAXLOC(6) = I6
                              MAXLOC(7) = I7
                           END IF
                           IF (ST.LT.DMIN) THEN
                              DMIN = ST
                              MINLOC(1) = I1
                              MINLOC(2) = I2
                              MINLOC(3) = I3
                              MINLOC(4) = I4
                              MINLOC(5) = I5
                              MINLOC(6) = I6
                              MINLOC(7) = I7
                           END IF
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE
C
C Calculate final statistics
C
      IF (ANPIX.GT.0) THEN
         SUM = AVE
         AVE = AVE / ANPIX
         RMS = SQRT(RMS/ANPIX)
      ELSE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'All pixels were blanked')
      END IF
C
      DO 170 I7 = BLC(7), TRC(7)
         DO 160 I6 = BLC(6), TRC(6)
            DO 150 I5 = BLC(5), TRC(5)
               DO 140 I4 = BLC(4), TRC(4)
                  DO 130 I3 = BLC(3), TRC(3)
                     DO 120 I2 = BLC(2), TRC(2)
                        DO 110 I1 = BLC(1), TRC(1)
                             DISP = DISP + 
     $                        (ABS(A(I1,I2,I3,I4,I5,I6,I7))-AVE)**2
 110                    CONTINUE
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
 160     CONTINUE
 170  CONTINUE
      IF (DISP .GT. 0.0) THEN
         DISP = SQRT(DISP / ANPIX)
      ELSE
         DISP = 0.0
      ENDIF
C
C Calculate bounding box  (Do the most common cases directly)
C
      IF (NAX.EQ.1) THEN
         DO 210 I1 = BLC(1), TRC(1)
            IF (A(I1,1,1,1,1,1,1).NE.0.0) THEN
               NLOC = NLOC + 1
               BBBLC(1) = MIN(BBBLC(1),I1)
               BBTRC(1) = MAX(BBTRC(1),I1)
            END IF
 210     CONTINUE
      ELSE IF (NAX.EQ.2) THEN
         DO 320 I2 = BLC(2), TRC(2)
            DO 310 I1 = BLC(1), TRC(1)
               IF (A(I1,I2,1,1,1,1,1).NE.0.0) THEN
                  NLOC = NLOC + 1
                  BBBLC(1) = MIN(BBBLC(1),I1)
                  BBBLC(2) = MIN(BBBLC(2),I2)
                  BBTRC(1) = MAX(BBTRC(1),I1)
                  BBTRC(2) = MAX(BBTRC(2),I2)
               END IF
 310        CONTINUE
 320     CONTINUE
      ELSE IF (NAX.EQ.3) THEN
         DO 430 I3 = BLC(3), TRC(3)
            DO 420 I2 = BLC(2), TRC(2)
               DO 410 I1 = BLC(1), TRC(1)
                  IF (A(I1,I2,I3,1,1,1,1).NE.0.0) THEN
                     NLOC = NLOC + 1
                     BBBLC(1) = MIN(BBBLC(1),I1)
                     BBBLC(2) = MIN(BBBLC(2),I2)
                     BBBLC(3) = MIN(BBBLC(3),I3)
                     BBTRC(1) = MAX(BBTRC(1),I1)
                     BBTRC(2) = MAX(BBTRC(2),I2)
                     BBTRC(3) = MAX(BBTRC(3),I3)
                  END IF
 410           CONTINUE
 420        CONTINUE
 430     CONTINUE
      ELSE
         DO 570 I7 = BLC(7), TRC(7)
            DO 560 I6 = BLC(6), TRC(6)
               DO 550 I5 = BLC(5), TRC(5)
                  DO 540 I4 = BLC(4), TRC(4)
                     DO 530 I3 = BLC(3), TRC(3)
                        DO 520 I2 = BLC(2), TRC(2)
                           DO 510 I1 = BLC(1), TRC(1)
                              IF (A(I1,I2,I3,I4,I5,I6,I7).NE.0.0) THEN
                                 NLOC = NLOC + 1
                                 BBBLC(1) = MIN(BBBLC(1),I1)
                                 BBBLC(2) = MIN(BBBLC(2),I2)
                                 BBBLC(3) = MIN(BBBLC(3),I3)
                                 BBBLC(4) = MIN(BBBLC(4),I4)
                                 BBBLC(5) = MIN(BBBLC(5),I5)
                                 BBBLC(6) = MIN(BBBLC(6),I6)
                                 BBBLC(7) = MIN(BBBLC(7),I7)
                                 BBTRC(1) = MAX(BBTRC(1),I1)
                                 BBTRC(2) = MAX(BBTRC(2),I2)
                                 BBTRC(3) = MAX(BBTRC(3),I3)
                                 BBTRC(4) = MAX(BBTRC(4),I4)
                                 BBTRC(5) = MAX(BBTRC(5),I5)
                                 BBTRC(6) = MAX(BBTRC(6),I6)
                                 BBTRC(7) = MAX(BBTRC(7),I7)
                              END IF
 510                       CONTINUE
 520                    CONTINUE
 530                 CONTINUE
 540              CONTINUE
 550           CONTINUE
 560        CONTINUE
 570     CONTINUE
      ENDIF
C
 999  CONTINUE
      END
