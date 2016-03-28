C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftxnp2.f	1.1	 2/18/92
C
      SUBROUTINE FFTXNP2 (X, WORK, M, IDIR)
C
C Complex to complex FFT: Generic Non-Power-of-2 version.
C  Currently will handle up to 8192.
C
C	X	CMPLX	Complex array to be transformed.
C	W	CMPLX	Complex work array.  (Not used in this version.)
C	M	INT	Length of X
C	IDIR	INT	Direction of the transform - same convention as FFTX
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Dec 30 1991
C-----------------------------------------------------------------------
      COMPLEX X(*), WORK(*)
      INTEGER M, IDIR
C
      INTEGER	I
      REAL	FACT
C
      REAL	WSAVE(32783)
      INTEGER	OLDM
      DATA	OLDM /0/
      SAVE	OLDM, WSAVE
C=======================================================================
      IF (M.NE.OLDM) THEN
         OLDM = M
         CALL CFFTI (M, WSAVE)
      END IF
C
      IF (IDIR.GT.0) THEN
         CALL CFFTF (M, X, WSAVE)
      ELSE
         CALL CFFTB (M, X, WSAVE)
         FACT = 1.0 / M
         DO 10 I = 1, M
            X(I) = X(I) * FACT
 10      CONTINUE
      END IF
C
      END
