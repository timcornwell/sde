C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fftr.f	1.4    11/8/90
C
      SUBROUTINE FFTR (R, C, WORK, N, F)
#ifdef COMP_CRAY
C
CD Does an real-to-complex forward or complex-to-real inverse.
C The length of C is actually N/2 + 1 since the data is stored in 
C a non-packed format for subsequent processing.
C Currently will handle up to M=8192
C Cray version calls RCFFT2, etc.
C
C      R	CMPLX	input 	Array containing real data
C      C	CMPLX	input 	Array containing complex data
C	WORK	CMPLX	input	Work array
C      N	INT	input	Real element count (power of 2)
C      F	INT	input	flag, 1=>forward FFT, -1=> reverse FFT.
C				Forward = real-to-complex
C				Reverse = complex to real
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
      REAL R(*)
      COMPLEX C(*)
      INTEGER N, F
      COMPLEX WORK(*)
C
      COMPLEX	LOOKUP1(12290), LOOKUP2(12290)
      INTEGER NOLD, I
      DATA NOLD /0/
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       See if table to be initilized
      IF (N.NE.NOLD) THEN
         NOLD = N
         CALL RCFFT2(1, -F, N, R, LOOKUP1, C)
         CALL CRFFT2(1, -F, N, C, LOOKUP2, R)
      ENDIF
      IF (F.GT.0) THEN
         CALL RCFFT2(0, -F, N, R, LOOKUP1, C)
         DO 10 I = 1, N/2+1
            C(I) = 0.5*C(I)
  10     CONTINUE
      ELSE
         CALL CRFFT2(0, -F, N, C, LOOKUP2, R)
         FACT = 1.0/FLOAT(N)
         DO 20 I = 1, N
            R(I) = R(I) * FACT
  20     CONTINUE
      END IF         
C
 999  CONTINUE
      END
#else
C
C Does an real-to-complex forward or complex-to-real inverse.
C The length of R is actually N + 1 since the data is stored in 
C a non-packed format for subsequent processing.
C Currently will handle up to M=8192
C Stolen from AIPS by T.J. Cornwell 1/7/86
C
C      R	CMPLX	input 	Array containing real data
C      C	CMPLX	input 	Array containing complex data
C	WORK	CMPLX	input	Work array
C      N	INT	input	Real element count (power of 2)
C      F	INT	input	flag, 1=>forward FFT, -1=> reverse FFT.
C				Forward = real-to-complex
C				Reverse = complex to real
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
      REAL R(*), C(*)
      COMPLEX WORK(*)
      INTEGER N, F, H, Q, LOOP
      REAL    TEMPR, TWOPI, PHASE, PHAS0, DPHAS, SUMR, SUMI,
     *   DIFFR, DIFFI
C                                       TABLR, TABLI for 8192 FFTs
      REAL    TABLR(2048), TABLI(2048)
      INTEGER NOLD
      DATA TWOPI, NOLD /6.2831852,0/
      SAVE NOLD
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Setup
      H = N / 2
      Q = H / 2
C                                       See if table to be initilized
      IF (N.EQ.NOLD) GO TO 50
         NOLD = N
         PHAS0 = -TWOPI / 4.0
         DPHAS = -TWOPI / N
         DO 20 LOOP = 1,Q
            PHASE = PHAS0 + LOOP * DPHAS
            TABLR(LOOP) = COS (PHASE)
            TABLI(LOOP) = SIN (PHASE)
 20         CONTINUE
C                                       Separate code for for./rev. FFT
 50   IF (F.LT.0) GO TO 200
C                                       Forward (real to complex)
         DO 10 LOOP = 1, N
            C(LOOP) = R(LOOP)
  10     CONTINUE
C                                       Complex FFT
         CALL FFTX (C, WORK, H, F)
C
C Now fix up so that the answers are in the right place
C
         DO 100 LOOP = 1,Q
            SUMR = C(1+2*LOOP) + C(1+2*(H-LOOP))
            SUMI = C(2+2*LOOP) - C(2+2*(H-LOOP))
            DIFFR = C(1+2*LOOP) - C(1+2*(H-LOOP))
            DIFFI = C(2+2*LOOP) + C(2+2*(H-LOOP))
            C(1+2*LOOP) = SUMR + (TABLR(LOOP) * DIFFR - TABLI(LOOP) *
     *         DIFFI)
            C(2+2*LOOP) = SUMI + (TABLI(LOOP) * DIFFR + TABLR(LOOP) *
     *         DIFFI)
            C(1+2*(H-LOOP)) = SUMR - (TABLR(LOOP) * DIFFR - 
     *         TABLI(LOOP) * DIFFI)
            C(2+2*(H-LOOP)) = -SUMI + (TABLI(LOOP) * DIFFR + 
     *         TABLR(LOOP) * DIFFI)
 100        CONTINUE
C                                       Pack
         TEMPR = 2.0 * (C(1) + C(2))
         C(2) = 2.0 * (C(1) - C(2))
         C(1) = TEMPR
         DO 500 LOOP = 1,N
            C(LOOP) = 0.5*C(LOOP)
 500     CONTINUE
         C(N+1) = C(2)
         C(N+2) = 0.0
         C(2) = 0.0
         GO TO 999
C                                       Reverse (complex to real)
 200     CONTINUE
         DO 400 LOOP = 1, N
            R(LOOP) = C(LOOP)
 400     CONTINUE
         R(2) = C(N+1)
         R(N+1) = 0.0
         R(N+2) = 0.0
         DO 300 LOOP = 1,Q
            SUMR = R(1+2*LOOP) + R(1+2*(H-LOOP))
            SUMI = R(2+2*LOOP) - R(2+2*(H-LOOP))
            DIFFR = R(1+2*LOOP) - R(1+2*(H-LOOP))
            DIFFI = R(2+2*LOOP) + R(2+2*(H-LOOP))
            R(1+2*LOOP) = SUMR + (TABLR(LOOP) * DIFFR + TABLI(LOOP) *
     *         DIFFI)
            R(2+2*LOOP) = SUMI - (TABLI(LOOP) * DIFFR - TABLR(LOOP) *
     *         DIFFI)
            R(1+2*(H-LOOP)) = SUMR - (TABLR(LOOP) * DIFFR + 
     *         TABLI(LOOP) * DIFFI)
            R(2+2*(H-LOOP)) = -SUMI - (TABLI(LOOP) * DIFFR - 
     *         TABLR(LOOP) * DIFFI)
 300        CONTINUE
C                                       Pack
         TEMPR = (R(1) + R(2))
         R(2) = (R(1) - R(2))
         R(1) = TEMPR
C                                       Complex FFT
         CALL FFTX (R, WORK, H, F)
C
         DO 600 LOOP = 1,N
            R(LOOP) = 0.5*R(LOOP)
 600     CONTINUE
C
 999  CONTINUE
      END
#endif
