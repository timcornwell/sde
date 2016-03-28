C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftrnp2.f	1.2	 2/19/92
C
      SUBROUTINE FFTRNP2 (R, C, WORK, M, IDIR)
C
C Real to complex FFT: Generic Non-Power-of-2 version.
C
C	R	REAL	input 	Array containing real data
C	C	CMPLX	input 	Array containing complex data
C	WORK	REAL	input	Work array
C	M	INT	input	Real element count
C	IDIR	INT	input	flag, 1=>forward FFT, -1=> reverse FFT.
C				Forward = real-to-complex
C				Reverse = complex to real
C
C The length of C is actually M/2 + 1 if M is even, and (M+1)/2 if
C M is odd, since the data is stored in a non-packed format for
C later processing.
C Currently will handle up to M=8192
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Jan 18 1992
C       Fixed bug in real->complex unpacking.
C				D.S.Briggs	Jan 19 1992
C-----------------------------------------------------------------------
      REAL R(*), WORK(*)
      COMPLEX C(*)
      INTEGER M, IDIR
C
      INTEGER	I
      REAL	FACT
C
      REAL	WSAVE(16399)
      INTEGER	OLDM
      DATA	OLDM /0/
      SAVE	OLDM, WSAVE
C=======================================================================
      IF (M.NE.OLDM) THEN
         OLDM = M
         CALL RFFTI (M, WSAVE)
      END IF
C
      IF (IDIR.GT.0) THEN
         DO 10 I = 1, M
            WORK(I) = R(I)
 10      CONTINUE
         CALL RFFTF (M, WORK, WSAVE)
C
C Unpack the result
C
         C(1) = CMPLX(WORK(1),0.0)
         IF (2*(M/2).EQ.M) C(M/2+1) = CMPLX(WORK(M),0.0)
         DO 12 I = 2, (M+1)/2
            C(I) = CMPLX(WORK(2*I-2),WORK(2*I-1))
 12      CONTINUE
      ELSE
C
C Pack the data
C
         WORK(1) = REAL(C(1))
         IF (2*(M/2).EQ.M) WORK(M) = REAL(C(M/2+1))
         DO 20 I = 2, (M+1)/2
            WORK(2*I-2) = REAL(C(I))
            WORK(2*I-1) = AIMAG(C(I))
 20      CONTINUE
         CALL RFFTB (M, WORK, WSAVE)
         FACT = 1.0 / M
         DO 22 I = 1, M
            R(I) = WORK(I) * FACT
 22      CONTINUE
      END IF
C
      END
