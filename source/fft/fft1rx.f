C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft1rx.f	1.1    2/10/93
C
      SUBROUTINE FFT1RX (RARRAY, XARRAY, SIZE)
C
CD One dimensional FFT Real -> Complex. 
C
C This routine acts as a switchyard for a generic 1-D Real -> Complex FFT
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C
C	RARRAY	REAL(SIZE)	Input Array
C	XARRAY	COMPLEX(SIZE/2+1) Output Array
C	SIZE	INTEGER 	size of the above Arrays
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 9 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE
      COMPLEX           XARRAY(SIZE/2+1)
      REAL              RARRAY(SIZE)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT1RX')

C
C Function definitions
C
      LOGICAL           FFTF2, FFTF235
C
C Local Variables
C
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
C
C Radix 2 FFT
C
         IF (FFTF2(SIZE)) THEN
C
C Setup a workspace array
C
            NAXIS(1) = 3 * (SIZE/2)
            CALL DATMAKAR('SCRATCH/FFT1RX', 1, NAXIS, 'R', WADD)
            IF (ERROR) GO TO 990
C
C Copy the input array to the output 
C
            J = 0
            DO I = 1, SIZE, 2
               J = J + 1
               XARRAY(J) = CMPLX(RARRAY(I), RARRAY(I+1))
            END DO
            XARRAY(J+1) = (0.,0.)
C
C Do the FFT 
C
            CALL SRC1FT(XARRAY, SIZE, MEMR(WADD), -3, IERR)
            CALL SRC1FT(XARRAY, SIZE, MEMR(WADD), +1, IERR)
C
C Multiply the output to shift the input array to the center
C
            DO I = 2, SIZE/2, 2
               XARRAY(I) = -XARRAY(I)
            END DO
C
C Radix 2,3,5 FFT
C
         ELSE IF (FFTF235(SIZE)) THEN
C
C Copy the input array to the output 
C
            J = 0
            DO I = 1, SIZE, 2
               J = J + 1
               XARRAY(J) = CMPLX(RARRAY(I), RARRAY(I+1))
            END DO
            XARRAY(J+1) = (0.,0.)
C
C Do the FFT 
C
            CALL SRCFTS(XARRAY, SIZE, 1, 1, SIZE, +1, IERR)
C
C Multiply the output to shift the input array to the center
C
            DO I = 2, SIZE/2 + 1, 2
               XARRAY(I) = -XARRAY(I)
            END DO
         ELSE
#endif
C
C Singletons FFT
C
C
C Setup a workspace array
C
            NAXIS(1) = SIZE
            CALL DATMAKAR('SCRATCH/FFT1RX/R', 1, NAXIS, 'R', RADD)
            CALL DATMAKAR('SCRATCH/FFT1RX/I', 1, NAXIS, 'R', IADD)
C
C Copy the input array to the output 
C
            DO I = 0, SIZE - 1
               MEMR(RADD + I) = RARRAY(I + 1)
               MEMR(IADD + I) = 0.0
            END DO
C
C Do the FFT 
C
            CALL FFT_V2(MEMR(RADD), MEMR(IADD), 1, SIZE, 1, -1) 
C
C Multiply the output to shift the input array to the center
C
            IF (2*(SIZE/2).EQ.SIZE) THEN
               DO I = 0, SIZE/2, 2
                  XARRAY(I+1)   =   
     $                 CMPLX(MEMR(RADD + I), MEMR(IADD + I))
               END DO
               DO I = 1, SIZE/2, 2
                  XARRAY(I+1) = 
     $                 - CMPLX(MEMR(RADD + I), MEMR(IADD + I))
               END DO
            ELSE
               THETA = 0
               THETA1 = 4. * ATAN(1.0) * FLOAT(SIZE-1) / FLOAT(SIZE)
               DO I = 0, SIZE/2
                  XARRAY(I+1) = CMPLX(MEMR(RADD + I),MEMR(IADD + I))
     $                 * CMPLX(COS(THETA), SIN(THETA))
                  THETA = THETA + THETA1
               END DO
            END IF
C
C Delete the working array(s)
C
            CALL DATDELAR('SCRATCH/FFT1RX/R')
            CALL DATDELAR('SCRATCH/FFT1RX/I')
#ifdef COMP_CONVEX
         END IF
#endif
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
