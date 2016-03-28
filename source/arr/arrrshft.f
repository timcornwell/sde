C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrrshft.f	1.3    11/7/90
C
      SUBROUTINE ARRRSHFT (IN, OUT, TEMP, N, DELT, SHIFT)
C
CD We simply shift an image by SHIFT pixels, blanking the 
C 'new' region of the image.
C
C
C
C	IN	REAL	in	REAL 2D Image
C	OUT	REAL	out	REAL 2D Image
C	N	INT(*)	in	Dimension of Image
C	DELT	REAL(*)	in	Pixel size
C	SHIFT	INT(*)	in	number of pixels to shift
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 7 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER	N(*), SHIFT(*)
      REAL	DELT(*)
      REAL	IN(N(1),N(2),N(3),N(4),N(5),N(6),N(7))
      REAL	OUT(N(1),N(2),N(3),N(4),N(5),N(6),N(7))
      REAL	TEMP(N(1),N(2),N(3),N(4),N(5),N(6),N(7))
C
      CHARACTER*(*)	ROUTINE
C
      PARAMETER		(ROUTINE = 'ARRRSHFT')
      INTEGER		I1,I2,I3,I4,I5,I6,I7, I
      INTEGER		S(SYSMXDIM), IMIN(SYSMXDIM),
     $   		IMAX(SYSMXDIM)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      DO 70 I7 = 1, N(7)
         DO 60 I6 = 1, N(6)
            DO 50 I5 = 1, N(5)
               DO 40 I4 = 1, N(4)
                  DO 30 I3 = 1, N(3)
                     DO 20 I2 = 1, N(2)
                        DO 10 I1 = 1, N(1)
                           TEMP(I1,I2,I3,I4,I5,I6,I7) = 0.0
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE
C
      DO 80 I = 1, 7
         IF (DELT(I) .NE. 0.) THEN
            S(I) = SHIFT(I) * (DELT(I) / ABS(DELT(I) ) )
         ELSE
            S(I) = 0.
         ENDIF
         IMIN(I) = MAX (1, 1 - S(I) )
         IMAX(I) = MIN (N(I), N(I) - S(I) )
 80   CONTINUE
C
      DO 170 I7 = IMIN(7), IMAX(7)
         DO 160 I6 = IMIN(6), IMAX(6)
            DO 150 I5 = IMIN(5), IMAX(5)
               DO 140 I4 = IMIN(4), IMAX(4)
                  DO 130 I3 = IMIN(3), IMAX(3)
                     DO 120 I2 = IMIN(2), IMAX(2)
                        DO 110 I1 = IMIN(1), IMAX(1)
                           TEMP(I1+S(1),I2+S(2),I3+S(3),I4+S(4),
     $                        I5+S(5),I6+S(6),I7+S(7) ) = 
     $                        IN(I1,I2,I3,I4,I5,I6,I7) 
 110                    CONTINUE
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
 160     CONTINUE
 170  CONTINUE
C
      DO 270 I7 = 1, N(7)
         DO 260 I6 = 1, N(6)
            DO 250 I5 = 1, N(5)
               DO 240 I4 = 1, N(4)
                  DO 230 I3 = 1, N(3)
                     DO 220 I2 = 1, N(2)
                        DO 210 I1 = 1, N(1)
                           OUT(I1,I2,I3,I4,I5,I6,I7) =
     $                        TEMP(I1,I2,I3,I4,I5,I6,I7)
 210                    CONTINUE
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
 250        CONTINUE
 260     CONTINUE
 270  CONTINUE

C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
