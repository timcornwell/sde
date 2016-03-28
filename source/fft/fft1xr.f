C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft1xr.f	1.1    2/10/93
C
      SUBROUTINE FFT1XR (XARRAY, RARRAY, SIZE)
C
CD One dimensional FFT  Complex. -> Real
C
C This routine acts as a switchyard for a generic 1-D Complex -> Real FFT
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C
C	XARRAY	COMPLEX(SIZE/2+1) Input Array
C	RARRAY	REAL(SIZE + 2)	Output Array
C	SIZE	INTEGER 	size of the above Arrays
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 10 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE
      COMPLEX           XARRAY(SIZE/2+1)
      REAL              RARRAY(SIZE + 2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT1XR')

C
C Function definitions
C
      LOGICAL           FFTF2, FFTF235
C
C Local Variables
C
      COMPLEX           CVALUE
      REAL              THETA, THETA1
      INTEGER           NAXIS(SYSMXDIM), RADD, IADD, WADD, I, J, IERR
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Code goes here
C
      IF (SIZE.GE.1) THEN
#ifdef COMP_CONVEX
         IF (FFTF2(SIZE)) THEN
C
C Radix 2 FFT
C
C
C Setup a workspace array
C
            NAXIS(1) = 3 * (SIZE/2)
            CALL DATMAKAR('SCRATCH/FFT1XR', 1, NAXIS, 'R', WADD)
            IF (ERROR) GO TO 990
C
C Copy the input array to the output 
C
            J = 1
            DO I = 2, SIZE + 2, 4
               RARRAY(I-1) = REAL(XARRAY(J))
               RARRAY(I)   = AIMAG(XARRAY(J))
               J = J + 2
            END DO
            J = 2
            DO I = 4, SIZE + 2, 4
               RARRAY(I-1) = - REAL(XARRAY(J))
               RARRAY(I)   = - AIMAG(XARRAY(J))
               J = J + 2
            END DO
            CALL SRC1FT(RARRAY, SIZE, MEMR(WADD), -3, IERR)
            CALL SRC1FT(RARRAY, SIZE, MEMR(WADD), -1, IERR)
         ELSE IF (FFTF235(SIZE)) THEN
C
C Radix 2,3,5 FFT
C
            J = 1
            DO I = 2, SIZE + 2, 4
               RARRAY(I-1) = REAL(XARRAY(J))
               RARRAY(I)   = AIMAG(XARRAY(J))
               J = J + 2
            END DO
            J = 2
            DO I = 4, SIZE + 2, 4
               RARRAY(I-1) = - REAL(XARRAY(J))
               RARRAY(I)   = - AIMAG(XARRAY(J))
               J = J + 2
            END DO
            CALL SRCFTS(RARRAY, SIZE, 1, 1, SIZE, -1, IERR)
         ELSE
C
C Singletons FFT
C
            NAXIS(1) = SIZE + 2
            CALL DATMAKAR('SCRATCH/FFT1XR/I', 1, NAXIS, 'R', IADD)
            DO I = 1, SIZE/2 + 1
               RARRAY(I) = REAL(XARRAY(I))
               MEMR(IADD + I - 1) = AIMAG(XARRAY(I))
            END DO
            DO I = SIZE/2 + 2, SIZE
               RARRAY(I) = REAL(XARRAY(SIZE - I + 2))
               MEMR(IADD + I - 1) = - AIMAG(XARRAY(SIZE - I + 2))
            END DO
            IF ((SIZE/2)*2.EQ.SIZE) THEN
               DO I = 1, SIZE, 2
                  RARRAY( I + 1) = - RARRAY( I + 1)
                  MEMR(IADD + I) = - MEMR(IADD + I)
               END DO
            ELSE
               THETA1 = - (4. * ATAN(1.0) *FLOAT(SIZE - 1)/FLOAT(SIZE))
               THETA = THETA1
               DO I = 1, SIZE/2
                  CVALUE = CMPLX(RARRAY(I + 1), MEMR(IADD + I))
                  CVALUE = CVALUE * CMPLX(COS(THETA), SIN(THETA))
                  RARRAY(1 + I)         = REAL(CVALUE)
                  RARRAY(1 + SIZE - I)  = REAL(CVALUE)
                  MEMR(IADD + I)        = AIMAG(CVALUE)
                  MEMR(IADD + SIZE - I) = - AIMAG(CVALUE)
                  THETA = THETA + THETA1
               END DO
            END IF
            CALL FFT_V2(RARRAY, MEMR(IADD), 1, SIZE, 1, 1) 
         END IF
#else
C
C Basic Scalar FFT is Singletons FFT
C
         NAXIS(1) = SIZE
         CALL DATMAKAR('SCRATCH/FFT1XR/I', 1, NAXIS, 'R', IADD)
         DO I = 1, SIZE/2 + 1
            RARRAY(I) = REAL(XARRAY(I))
            MEMR(IADD + I - 1) = AIMAG(XARRAY(I))
         END DO
         DO I = SIZE/2 + 2, SIZE
            RARRAY(I) = REAL(XARRAY(SIZE - I + 2))
            MEMR(IADD + I - 1) = - AIMAG(XARRAY(SIZE - I + 2))
         END DO
         IF ((SIZE/2)*2.EQ.SIZE) THEN
            DO I = 1, SIZE, 2
               RARRAY( I + 1) = - RARRAY( I + 1)
               MEMR(IADD + I) = - MEMR(IADD + I)
            END DO
         ELSE
            THETA1 = - (4. * ATAN(1.0) * FLOAT(SIZE - 1) / FLOAT(SIZE))
            THETA = THETA1
            DO I = 1, SIZE/2
               CVALUE = CMPLX(RARRAY(I + 1), MEMR(IADD + I))
               CVALUE = CVALUE * CMPLX(COS(THETA), SIN(THETA))
               RARRAY(1 + I)         = REAL(CVALUE)
               RARRAY(1 + SIZE - I)  = REAL(CVALUE)
               MEMR(IADD + I)        = AIMAG(CVALUE)
               MEMR(IADD + SIZE - I) = - AIMAG(CVALUE)
               THETA = THETA + THETA1
            END DO
         END IF
         CALL FFT_V2(RARRAY, MEMR(IADD), 1, SIZE, 1, 1) 
#endif
      ELSE IF (SIZE.EQ.1) THEN
         XARRAY(1) = RARRAY(1)
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'FFT Size less than 1')
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
