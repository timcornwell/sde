C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft2rx.f	1.1    2/10/93
C
      SUBROUTINE FFT2RX (RARRAY, XARRAY, SIZE)
C
CD Two dimensional FFT Real -> Complex. 
C
C This routine acts as a switchyard for a generic 2-D Real -> Complex FFT
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C
C	RARRAY	REAL(SIZE(1),SIZE(2))	Input Array
C	XARRAY	COMPLEX(SIZE(1)/2+1, SIZE(2)) Output Array
C	SIZE	INTEGER(2) 	size of the above Arrays
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Feb 14 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE(2)
      COMPLEX           XARRAY(SIZE(1)/2+1,SIZE(2))
      REAL              RARRAY(SIZE(1), SIZE(2))
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT2RX')

C
C Function definitions
C
      LOGICAL           FFTF235
C
C Local Variables
C
      COMPLEX           Z
      REAL              THETA, THETA1, C, S
      INTEGER           NAXIS(SYSMXDIM), RADD, IADD, I, J, IERR
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Code goes here
C
      IF ((SIZE(1).GT.0).AND.(SIZE(2).GT.0)) THEN
#ifdef COMP_CONVEX
         IF (FFTF235(SIZE(1)).AND.FFTF235(SIZE(2)))THEN
C
C Radix 2,3,5 FFT
C
C
C Copy input data to output array (premultiplying to shift origin of output)
C
            DO I = 2, SIZE(1),2
               DO J = 1, SIZE(2),2
                  XARRAY(I/2,J) = CMPLX(RARRAY(I-1,J), RARRAY(I,J))
               END DO
               DO J = 2, SIZE(2),2
                  XARRAY(I/2,J) = CMPLX(-RARRAY(I-1,J), -RARRAY(I,J))
               END DO
            END DO
C
C Do the FFT
C
            CALL SRC2FT(XARRAY, SIZE(1), SIZE(2), SIZE(1)+2, 
     $           1, IERR)
C
C Multiply the answer to shift the origin of the input array
C
            DO I = 1, SIZE(1)/2 + 1,2
               DO J = SIZE(2)/2 + 2, SIZE(2),2
                  XARRAY(I,J) = - XARRAY(I,J)
               END DO
               DO J = SIZE(2)/2, 1, -2
                  XARRAY(I,J) = - XARRAY(I,J)
               END DO
            END DO
            DO I = 2, SIZE(1)/2 + 1,2
               DO J = SIZE(2)/2 + 1, SIZE(2),2
                  XARRAY(I,J) = - XARRAY(I,J)
               END DO
               DO J = SIZE(2)/2 - 1 , 1, -2
                  XARRAY(I,J) = - XARRAY(I,J)
               END DO
            END DO
         ELSE
#endif
C
C Basic Scalar FFT is Singletons FFT
C
C
C Create the working array
C
         NAXIS(1) = SIZE(1)
         NAXIS(2) = SIZE(2)
         CALL DATMAKAR('SCRATCH/FFT2RX/R', 2, NAXIS, 'R', RADD)
         CALL DATMAKAR('SCRATCH/FFT2RX/I', 2, NAXIS, 'R', IADD)
C
C Copy the data into the working array
C   (premultiplying to shift origin of output)
C
         IF (2*(SIZE(2)/2).EQ.SIZE(2)) THEN
            DO J = 0, SIZE(2) - 1, 2
               DO I = 0, SIZE(1) - 1
                  MEMR(RADD + J * SIZE(1) + I) = RARRAY(I+1, J+1)
                  MEMR(IADD + J * SIZE(1) + I) = 0.0
               END DO
            END DO
            DO J = 1, SIZE(2) - 1, 2
               DO I = 0, SIZE(1) - 1
                  MEMR(RADD + J * SIZE(1) + I) = - RARRAY(I+1, J+1)
                  MEMR(IADD + J * SIZE(1) + I) = 0.0
               END DO
            END DO
         ELSE
            THETA = 0.
            THETA1 = 4. * ATAN(1.) * FLOAT(SIZE(2)-1)/FLOAT(SIZE(2))
            DO J = 0, SIZE(2) - 1
               C = COS(THETA)
               S = SIN(THETA)
               DO I = 0, SIZE(1) - 1
                  MEMR(RADD + J * SIZE(1) + I) = C * RARRAY(I+1, J+1)
                  MEMR(IADD + J * SIZE(1) + I) = S * RARRAY(I+1, J+1)
               END DO
               THETA = THETA + THETA1
            END DO
         END IF
C
C Do the FFT as two calls of a 1D FFT in opposite directions
C
         CALL FFT_V2(MEMR(RADD), MEMR(IADD), 1, SIZE(2), SIZE(1), -1) 
         CALL FFT_V2(MEMR(RADD), MEMR(IADD), SIZE(2), SIZE(1), 1, -1) 
C
C Copy the data from the working array to the output array
C Multiplying the answer to shift the origin of the input array
C
         IF (2*(SIZE(2)/2).EQ.SIZE(2).AND.2*(SIZE(1)/2).EQ.SIZE(1))THEN
            DO I = 0, SIZE(1)/2, 2
               DO J = SIZE(2)/2+1, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = 
     $                 - CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = 
     $                 CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2-1, 0, -2
                  XARRAY(I+1, J+1) = 
     $                 - CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2-2, 0, -2
                  XARRAY(I+1, J+1) = 
     $                 CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
            END DO
            DO I = 1, SIZE(1)/2, 2
               DO J = SIZE(2)/2, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = 
     $                 - CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2+1, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = 
     $                 CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2-2, 0, -2
                  XARRAY(I+1, J+1) = 
     $                 - CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
               DO J = SIZE(2)/2-1, 0, -2
                  XARRAY(I+1, J+1) = 
     $                 CMPLX(MEMR(RADD + J * SIZE(1) + I), 
     $                 MEMR(IADD + J * SIZE(1) + I))
               END DO
            END DO
         ELSE IF (2*(SIZE(2)/2).EQ.SIZE(2)) THEN
            THETA = 0.
            THETA1 = 4. * ATAN(1.) * FLOAT(SIZE(1)-1)/FLOAT(SIZE(1))
            DO I = 0, SIZE(1)/2
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO J = SIZE(2)/2, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               DO J = SIZE(2)/2-2, 0, -2
                  XARRAY(I+1, J+1) = CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               DO J = SIZE(2)/2+1, SIZE(2)-1, 2
                  XARRAY(I+1, J+1) = -CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               DO J = SIZE(2)/2-1, 0, -2
                  XARRAY(I+1, J+1) = -CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               THETA = THETA + THETA1
            END DO
         ELSE IF (2*(SIZE(1)/2).EQ.SIZE(1)) THEN
            THETA = 0.
            THETA1 = 4. * ATAN(1.) * FLOAT(SIZE(2)-1)/FLOAT(SIZE(2))
            DO J = SIZE(2)/2, SIZE(2)
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO I = 0, SIZE(1)/2, 2
                  XARRAY(I+1, J+1) = CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               DO I = 1, SIZE(1)/2, 2
                  XARRAY(I+1, J+1) = - CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               THETA = THETA + THETA1
            END DO
            THETA = -THETA1
            DO J = SIZE(2)/2-1, 0, -1
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO I = 0, SIZE(1)/2, 2
                  XARRAY(I+1, J+1) = CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               DO I = 1, SIZE(1)/2, 2
                  XARRAY(I+1, J+1) = - CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               THETA = THETA - THETA1
            END DO
         ELSE
            THETA = 0.
            THETA1 = 4. * ATAN(1.) * FLOAT(SIZE(1)-1)/FLOAT(SIZE(1))
            DO I = 0, SIZE(1)/2
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO J = 0, SIZE(2)
                  XARRAY(I+1, J+1) = CMPLX(MEMR(RADD+J*SIZE(1)+I),
     $                 MEMR(IADD+J*SIZE(1)+I)) * Z
               END DO
               THETA = THETA + THETA1
            END DO
            THETA = 0
            THETA1 = 4. * ATAN(1.) * FLOAT(SIZE(2)-1)/FLOAT(SIZE(2))
            DO J = SIZE(2)/2, SIZE(2), 1
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO I = 0, SIZE(1)/2
                  XARRAY(I+1, J+1) = XARRAY(I+1, J+1) * Z
               END DO
               THETA = THETA + THETA1
            END DO
            THETA = - THETA1
            DO J = SIZE(2)/2 - 1, 0 , -1
               Z = CMPLX(COS(THETA), SIN(THETA))
               DO I = 0, SIZE(1)/2
                  XARRAY(I+1, J+1) =  XARRAY(I+1, J+1) * Z
               END DO
               THETA = THETA - THETA1
            END DO
         END IF
         CALL DATDELAR('SCRATCH/FFT2RX/R')
         CALL DATDELAR('SCRATCH/FFT2RX/I')
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
