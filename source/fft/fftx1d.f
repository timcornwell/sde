C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftx1d.f	1.5    7/14/97
C
      SUBROUTINE FFTX1D (XARRAY, XWORK, XWORK2, NX, DIR)
C
CD Compute One dimensional FFT of array: complex to complex.
C
C	XARRAY	CMPLX	input	Input complex array
C	XWORK	CMPLX	i/o	Work array
C	XWORK2	REAL	i/o	Work array
C	NX	INT	input	Length of complex array.
C	DIR	INT	input	Direction of transform
C
C Zero padding, if needed, is performed in FFTX1D2, which calls this
C routine. The phase center of the transform is at NX/2 and the center of
C the u plane is at NX/2.  When NX is odd, the center is at (NX+1)/2 in both
C cases.
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
C	Requirement that NX be a power of two is removed.
C				D.S.Briggs	Dec 27 1991
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NX, DIR
      COMPLEX		XARRAY(*)
      COMPLEX		XWORK(*), XWORK2(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFTX1D')
C
      INTEGER		IX, ICEN, IOFF
C
      INTEGER		LASTNX
      INTEGER		FFTPWR2
      LOGICAL		DOEXACT
      SAVE		LASTNX, DOEXACT
C
      DATA		LASTNX/-1/
C====================================================================
      IF (ERROR) GO TO 999
C
C Find center of transform
C
      ICEN = (NX+1) / 2
C
C Pack into work array. The center of the input
C array, i.e. NX/2, goes to pixel 1
C Then fiddle to get center of transform to NX/2
C
      IOFF = NX-ICEN+1
      DO 10 IX = 1, ICEN-1
         XWORK2 (IX+IOFF) = XARRAY (IX)
 10   CONTINUE
      IOFF = -ICEN+1
      DO 11 IX = ICEN, NX
         XWORK2 (IX+IOFF) = XARRAY (IX)
 11   CONTINUE
C
C Dispatch to the appropriate routine.
C
      IF (NX.NE.LASTNX) DOEXACT = NX.NE.FFTPWR2(NX)
      LASTNX = NX
C
      IF (DOEXACT) THEN
         CALL FFTXNP2 (XWORK2(1), XWORK(1), NX, DIR)
      ELSE
         CALL FFTX (XWORK2(1), XWORK(1), NX, DIR)
      END IF
C
C Unpack the results
C
      IOFF = NX-ICEN+1
      DO 30 IX = 1, ICEN-1
         XARRAY(IX) = XWORK2(IX+IOFF)
 30   CONTINUE
      IOFF = -ICEN+1
      DO 31 IX = ICEN, NX
         XARRAY(IX) = XWORK2(IX+IOFF)
 31   CONTINUE
C
  999 CONTINUE
      END
