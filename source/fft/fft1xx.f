C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft1xx.f	1.1    2/10/93
C
      SUBROUTINE FFT1XX (XARRAYIN, XARRAYOU, DIR, SIZE)
C
CD One dimensional FFT  Complex. -> Complex
C
C This routine acts as a switchyard for a generic 1-D Complex -> Complex FFT
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C DIR controls whether a forward or inverse transform is done. 
C DIR = -1 -> Forward Transform (ie -1 in exponent and no 1/N scaling)
C DIR = +1 -> Inverse Transform (ie +1 in exponent and 1/N scaling)
C
C	XARRAYIN COMPLEX(SIZE)	Input Array
C	XARRAYOU COMPLEX(SIZE)	Output Array
C	SIZE	INTEGER 	size of the above Arrays
C	DIR	INTEGER 	Forward (-1) or Inverse (+1) Transform
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 12 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE, DIR
      COMPLEX           XARRAYIN(SIZE), XARRAYOU(SIZE)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT1XX')

C
C Function definitions
C
      LOGICAL           FFTF2, FFTF235
C
C Local Variables
C
      REAL              THETA, THETA1
      INTEGER           NAXIS(SYSMXDIM), WADD, I, IERR
      INTEGER           IDIR
      LOGICAL           TXEVEN, TXP4
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
         IDIR = ISIGN(1, DIR)
C
C Determine if the transform size is a power of 2 and 4
C
            IF (((SIZE/2)*2).EQ.SIZE) THEN
               TXEVEN = .TRUE.
               IF (((SIZE/4)*4).EQ.SIZE) THEN
                  TXP4 = .TRUE.
               ELSE
                  TXP4 = .FALSE.
               END IF
            ELSE
               TXEVEN = .FALSE.
               TXP4 = .FALSE.
            END IF
#ifdef COMP_CONVEX
         IF (FFTF2(SIZE)) THEN
C
C Radix 2 FFT
C
C
C Setup a workspace array
C
            NAXIS(1) = 5 * (SIZE/2)
            CALL DATMAKAR('SCRATCH/FFT1XX', 1, NAXIS, 'R', WADD)
            IF (ERROR) GO TO 990
C
C Multiply the input array to shift the output transform to the center
C
            DO I = 1, SIZE, 2
               XARRAYOU(I)   =   XARRAYIN(I)
               XARRAYOU(I+1) = - XARRAYIN(I+1)
            END DO
C
C Do the FFT 
C
            CALL C1DFFT(XARRAYOU, SIZE, MEMR(WADD), -3, IERR)
            CALL C1DFFT(XARRAYOU, SIZE, MEMR(WADD), -IDIR, IERR)
C
C Multiply the output to shift the input array to the center
C
            DO I = 2, SIZE, 2
               XARRAYOU(I) = - XARRAYOU(I)
            END DO
         ELSE IF (FFTF235(SIZE)) THEN
C
C Radix 2,3,5 FFT
C
C
C Multiply the input array to shift the output transform to the center
C
            DO I = 1, SIZE, 2
               XARRAYOU(I)   =  XARRAYIN(I)
               XARRAYOU(I+1) = -XARRAYIN(I+1)
            END DO
C
C Do the FFT 
C
            CALL CFFTS(XARRAYOU, SIZE, 1, 1, SIZE, -IDIR, IERR)
C
C Multiply the output to shift the input array to the center
C
            IF (TXP4) THEN
               DO I = 2, SIZE, 2
                  XARRAYOU(I) = - XARRAYOU(I)
               END DO
            ELSE
               DO I = 1, SIZE, 2
                  XARRAYOU(I) = - XARRAYOU(I)
               END DO
            END IF
         ELSE
#endif
C
C Basic Scalar FFT is Singletons FFT
C
C
C Multiply the input array to shift the output transform to the center
C
         IF (TXEVEN) THEN
            DO I = 1, SIZE, 2
               XARRAYOU(I)   =   XARRAYIN(I)
               XARRAYOU(I+1) = - XARRAYIN(I+1)
            END DO
         ELSE
            THETA1 = -IDIR * 4.*ATAN(1.) * FLOAT(SIZE-1) / FLOAT(SIZE)
            THETA = 0.
            DO I = 1, SIZE
               XARRAYOU(I) = XARRAYIN(I) * CMPLX(COS(THETA), SIN(THETA))
               THETA = THETA + THETA1
            END DO
         END IF
C
C Do the FFT 
C This is a Kludge to get around the problem that you cannot
C Equivalence Arrays that are passed as parameters
C            
         CALL FFTX2RKL(XARRAYOU, 1, SIZE, 1, IDIR)
C
C Multiply the output to shift the input array to the center
C
         IF (TXP4) THEN
            DO I = 2, SIZE, 2
               XARRAYOU(I) = - XARRAYOU(I)
            END DO
         ELSE IF (TXEVEN) THEN
            DO I = 1, SIZE, 2
               XARRAYOU(I) = - XARRAYOU(I)
            END DO
         ELSE
            THETA = - SIZE/2 * THETA1
            DO I = 1, SIZE
               XARRAYOU(I) = XARRAYOU(I) * CMPLX(COS(THETA), SIN(THETA))
               THETA = THETA + THETA1
            END DO
         END IF
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
