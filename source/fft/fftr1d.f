C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftr1d.f	1.6    7/14/97
C
      SUBROUTINE FFTR1D (RARRAY, XARRAY, RWORK, XWORK, NX, DIR)
C
CD Compute One dimensional FFT of array: real to complex.
C
C	RARRAY	REAL	i/o	Real array
C	XARRAY	CMPLX	i/o	Complex array
C	RWORK	REAL	i/o	Work array
C	XWORK	CMPLX	i/o	Work array
C	NX	INT	input	Length of real array.
C	DIR	INT	input	Direction of transform
C
C Zero padding, if needed, is performed in FFTR1D2, which calls this
C routine.  The phase center in the real array is (NX/2) or (NX+1)/2
C depending as NX is even or odd.  The phase center of the complex array
C is always at the first pixel.
C
C The routine will be faster when NX is a power of two, but NX may be
C any value up to the currently allowed maximum of 8192.  In addition to
C the inherent algorithmic limitations of arbitrary N FFT routines, the
C power of 2 routines have been optimized for specific machine
C architectures.
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C       Modified to correctly pad odd sized images
C                               R.G Marson      Dec 5 1990
C	Padding broken out to FFTR1D2.  Requirement that NX be a
C	power of 2 is removed.
C				D.S.Briggs	Jan 20 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, DIR
      REAL		RARRAY(*), RWORK (*)
      COMPLEX		XARRAY(*)
      COMPLEX		XWORK(*)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTR1D')
C
      INTEGER		IX, ICEN, IOFF
C
      INTEGER		LASTNX
      LOGICAL		DOEXACT
      INTEGER		FFTPWR2
      SAVE		LASTNX, DOEXACT
      DATA		LASTNX/-1/
C
C====================================================================
      IF (ERROR) GO TO 999
C
C Find full length of x axis and center of transform
C
      ICEN = (NX+1) / 2
C
C Sort out which routine we will use
C
      IF (NX.NE.LASTNX) DOEXACT = NX.NE.FFTPWR2(NX)
      LASTNX = NX
C
C Direction of transform ?
C
      IF (DIR.GT.0) THEN
C
C Pack into work array. The center of the real array, i.e. NX/2,
C goes to pixel 1
C
         IOFF = NX-ICEN+1
         DO 8 IX = 1, ICEN-1
            RWORK (IX+IOFF) = RARRAY (IX)
 8       CONTINUE
         IOFF = -ICEN+1
         DO 10 IX = ICEN, NX
            RWORK (IX+IOFF) = RARRAY (IX)
 10      CONTINUE
         IF (DOEXACT) THEN
            CALL FFTRNP2 (RWORK, XARRAY, XWORK, NX, DIR)
         ELSE
            CALL FFTR (RWORK, XARRAY, XWORK, NX, DIR)
         END IF
      ELSE
C
C Transform assuming that first pixel is the center of the u plane
C Then fiddle to get the phase center back at ICEN
C
         IF (DOEXACT) THEN
            CALL FFTRNP2 (RWORK, XARRAY, XWORK, NX, DIR)
         ELSE
            CALL FFTR (RWORK, XARRAY, XWORK, NX, DIR)
         END IF
         IOFF = NX-ICEN+1
         DO 30 IX = 1, ICEN-1
            RARRAY(IX) = RWORK(IX+IOFF)
 30      CONTINUE
         IOFF = -ICEN+1
         DO 31 IX = ICEN, NX
            RARRAY(IX) = RWORK(IX+IOFF)
 31      CONTINUE
C
      END IF
C
  999 CONTINUE
      END
