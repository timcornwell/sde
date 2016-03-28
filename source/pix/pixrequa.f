C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrequa.f	1.3    11/7/90
C
      SUBROUTINE PIXREQUA (A, B, N, HIST, IHIST, NH, BASE, INCR,
     1   PIXR, OPC)
C
CD Equalize an array, and return inverse histogram
C
C	1   PIXR, OPC)
C
C	A	REAL	input	Real array
C	B	REAL	output	Equalized real array
C	IHIST	REAL	output	Inverse histogram
C	N	INT	input	Number of elements
C	HIST	INT	input	Real array
C	NB	INT	input	Number of boxs
C	BASE	REAL	output	BASE of array
C	INCR	REAL	output	Increment of array
C	PIXR	REAL(*)	input	Pixel range
C	OPC	CH*(*)	input	'LIN' | 'LOG' | 'SQRT'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		A(*), B(*), IHIST(*), BASE, INCR, PIXR(2)
      CHARACTER*(*)	OPC
      INTEGER		N, NH, HIST(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXREQUA')
C
      INTEGER		I, INDEX, IPEAK, ISTART, IEND
      REAL		ARRMAX, SUM, RINDEX, SCL, R
C=====================================================================
      IF (ERROR) GO TO 999
C
      ARRMAX = BASE + INCR * (NH - 1)
      IF ((PIXR(1).NE.0.0).AND.(PIXR(2).NE.0.0)) THEN
         ISTART = MAX(1, NINT((PIXR(1) - BASE)/INCR))
         IEND = MIN(NH, NINT((PIXR(2) - BASE)/INCR))
      ELSE
         IPEAK = 1
         DO 5 I = 1, NH
            IF (HIST(I).GT.HIST(IPEAK)) THEN
               IPEAK = I
            END IF
   5     CONTINUE
         ISTART = MIN (NH/2, 2*IPEAK)
         IEND = NH
      END IF
C
      SUM = 0.0
      DO 10 I = ISTART, IEND
         SUM = SUM + HIST(I)
  10  CONTINUE
      IF (OPC(1:3).EQ.'SQR') THEN
         SUM = SQRT(SUM)
      ELSE IF (OPC(1:3).EQ.'LOG') THEN
         SUM = ALOG(MAX(2.0, SUM))
      END IF
      SCL = INCR * (IEND - ISTART) / SUM
C
      DO 15 I = 1, NH
         IHIST(I) = 0.0
  15  CONTINUE
C
      SUM = 0.0
      IF (OPC(1:3).EQ.'SQR') THEN
         DO 20 I = ISTART, IEND
           SUM = SUM + HIST(I)
           IHIST(I) = BASE + INCR * (ISTART - 1) + SQRT(SUM) * SCL
 20     CONTINUE
      ELSE IF (OPC(1:3).EQ.'LOG') THEN
         DO 25 I = ISTART, IEND
            SUM = SUM + HIST(I)
            IHIST(I) = BASE + INCR * (ISTART - 1) + 
     1         ALOG(MAX(2.0, SUM)) * SCL
 25      CONTINUE
      ELSE
         DO 30 I = ISTART, IEND
           SUM = SUM + HIST(I)
           IHIST(I) = BASE + INCR * (ISTART - 1) + SUM * SCL
 30     CONTINUE
      END IF
C
      DO 35 I = IEND, NH
         IHIST(I) = IHIST(IEND)
  35  CONTINUE
C
      DO 40 I = 1, N
         RINDEX = 1.0 + (A(I) - BASE)/INCR
         B(I) = IHIST(NINT(RINDEX))
 40   CONTINUE
C
  999 CONTINUE
      END
