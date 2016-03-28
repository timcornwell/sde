C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrequal.f	1.3    11/7/90
C
      SUBROUTINE ARREQUAL (A1, A2, PIXR, OPC)
C
CD Equalize an array
C
C
C	A1	REAL	input	Name of array
C	A2	REAL	input	Name of array
C	PIXR	REAL(*)	input	Pixel range
C	OPC	CH*(*)	input	'LOG' or 'SQRT'
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	A1, A2, OPC
      REAL		PIXR(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARREQUAL')
C
      CHARACTER*1	T1, T2, TH, TIH
      INTEGER		I, N1, N2, NH, NAXIS1(SYSMXDIM),
     1			NAXIS2(SYSMXDIM), HNAXIS(SYSMXDIM)
      INTEGER		ADD1, ADD2, ADDH, ADDIH, NT, NDUMMY
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL		DATEXIST
      REAL		BASE, INCR
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL ARRHIST (A1, STRM2(A1, 'HIST'))
      CALL DATGETAR (STRM2(A1, 'HIST'), NH, HNAXIS, TH, ADDH)
      TIH = 'R'
      CALL DATMAKAR (STRM2(A1, 'IHIST'), NH, HNAXIS, TIH, ADDIH)
      CALL DATGETR (STRM2(A1, 'HIST'), 'BASE', BASE, 1, NDUMMY)
      CALL DATGETR (STRM2(A1, 'HIST'), 'INCREMENT', INCR, 1, NDUMMY)
C
      CALL DATGETAR (A1, N1, NAXIS1, T1, ADD1)
      NT = 1
      DO 10 I = 1, N1
         NAXIS1(I) = MAX(1, NAXIS1(I))
         NT = NT * NAXIS1(I)
  10  CONTINUE
C
C Make output array if it does not exist
C
      IF (.NOT.DATEXIST(A2)) THEN
         T2 = T1
	 N2 = N1
         DO 20 I = 1, N2
            NAXIS2(I) = NAXIS1(I)
  20     CONTINUE
         CALL DATMAKAR (A2, N2, NAXIS2, T2, ADD2)
      ELSE
         CALL DATGETAR (A2, N2, NAXIS2, T2, ADD2)
         IF (N1.NE.N2) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1         'Different number of axes')
            GO TO 999
         END IF
         IF (T1.NE.T2) THEN
            WRITE (MESSAGE, 1100) T1, T2
 1100       FORMAT (
     1         'Array types for image 1 and output image disagree : ',
     2         A1,1X,A1)
            CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
         END IF
         DO 30 I = 1,N2
            NAXIS2(I) = MAX(1, NAXIS2(I))
            IF (NAXIS2(I).NE.NAXIS1(I)) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     1            'Axes disagree')
               GO TO 999
            END IF
  30     CONTINUE
      END IF
C
C Call appropriate routine
C
      IF (T1.EQ.'R') THEN
         CALL PIXREQUA (MEMR(ADD1), MEMR(ADD2), NT, MEMI(ADDH),
     1      MEMR(ADDIH), HNAXIS(1), BASE, INCR, PIXR, OPC)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Array type '//T1//
     1      'Not supported')
      END IF
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
