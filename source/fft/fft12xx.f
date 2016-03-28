C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft12xx.f	1.1    2/10/93
C
      SUBROUTINE FFT12XX (XARRAYIN, XARRAYOU, DIR, SIZE1, SIZE2, AXIS)
C
CD Multiple One dimensional FFT's Complex -> Complex. On a 2D array
C
C This routine acts as a switchyard for a generic 
C 1 dimensional  Complex -> Complex FFT. The data is embedded in a 2 D array, 
C Axis specifies which axis is to be transformed. eg. Axis = 1 implies
C transform along the first axis.
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C DIR controls whether a forward or inverse transform is done. 
C DIR = -1 -> Forward Transform (ie -1 in exponent and no 1/N scaling)
C DIR = +1 -> Inverse Transform (ie +1 in exponent and 1/N scaling)
C
C	XARRAYOU COMPLEX(SIZE1, SIZE2) Input Array
C	XARRAYIN	REAL(SIZE1, SIZE2)	Output Array
C	DIR	INTEGER         Forward or Inverse Transform
C	SIZE1,2	INTEGER 	Size of the above Arrays
C       AXIS    INTEGER         which axis to transform along (1 or 2)
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Feb 9 1992
C       Modified so that SIZE is passed as two arguements not one
C                               R. G. Marson    Nov 27 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE1, SIZE2, AXIS, DIR
      COMPLEX           XARRAYIN(SIZE1, SIZE2)
      COMPLEX           XARRAYOU(SIZE1, SIZE2)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT12XX')

C
C Function definitions
C
      LOGICAL           FFTF235
C
C Local Variables
C
      COMPLEX           CFACTOR
      REAL              THETA, THETA1
      INTEGER           I, J, IERR, IDIR
      INTEGER           TXSIZE, NTX, SIZE(2)
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
      SIZE(1) = SIZE1
      SIZE(2) = SIZE2
      IF ((AXIS.GE.1).AND.(AXIS.LE.2)) THEN
         TXSIZE  = SIZE(AXIS)
         NTX     = SIZE(3-AXIS)
         IF (TXSIZE.GE.1) THEN
            IDIR = ISIGN(1,DIR)
C
C Determine if the transform size is a power of 2 and 4
C
            IF (((TXSIZE/2)*2).EQ.TXSIZE) THEN
               TXEVEN = .TRUE.
               IF (((TXSIZE/4)*4).EQ.TXSIZE) THEN
                  TXP4 = .TRUE.
               ELSE
                  TXP4 = .FALSE.
               END IF
            ELSE
               TXEVEN = .FALSE.
               TXP4 = .FALSE.
            END IF
#ifdef COMP_CONVEX
C
C Radix 2,3,5 FFT
C
            IF (FFTF235(TXSIZE)) THEN
C
C Case for Axis = 1
C
               IF (AXIS.EQ.1) THEN
C
C Multiply the input array to shift the output transform to the center
C
                  DO J = 1, SIZE(2)
                     DO I = 1, SIZE(1), 2
                        XARRAYOU(I, J)   =  XARRAYIN(I, J)
                        XARRAYOU(I+1, J) = -XARRAYIN(I+1, J)
                     END DO
                  END DO
C
C Do the FFT 
C
                  CALL CFFTS(XARRAYOU, TXSIZE, 1, 
     $                 NTX, TXSIZE, -IDIR, IERR)
C
C Multiply the output to shift the input array to the center
C
                  IF (TXP4) THEN
                     DO J = 1, SIZE(2)
                        DO I = 2, SIZE(1), 2
                           XARRAYOU(I, J) = -XARRAYOU(I, J)
                        END DO
                     END DO
                  ELSE
                     DO J = 1, SIZE(2)
                        DO I = 1, SIZE(1), 2
                           XARRAYOU(I, J) = -XARRAYOU(I, J)
                        END DO
                     END DO
                  END IF
C
C Case for Axis = 2
C
               ELSE
C
C Multiply the input array to shift the output transform to the center
C
                  DO J = 1, SIZE(2), 2
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J)   =  XARRAYIN(I, J)
                        XARRAYOU(I, J+1) = -XARRAYIN(I, J+1)
                     END DO
                  END DO
C
C Do the FFT 
C
                  CALL CFFTS(XARRAYOU, TXSIZE, NTX, 
     $                 NTX, 1, -IDIR, IERR)
C
C Multiply the output to shift the input array to the center
C
                  IF (TXP4) THEN
                     DO J = 2, SIZE(2), 2
                        DO I = 1, SIZE(1)
                           XARRAYOU(I, J) = -XARRAYOU(I, J)
                        END DO
                     END DO
                  ELSE
                     DO J = 1, SIZE(2), 2
                        DO I = 1, SIZE(1)
                           XARRAYOU(I, J) = -XARRAYOU(I, J)
                        END DO
                     END DO
                  END IF
               END IF
         ELSE
#endif
C
C Basic Scalar FFT is Singletons FFT
C
C
C Case for Axis = 1
C
            IF (AXIS.EQ.1) THEN
C
C Multiply the input array to shift the output transform to the center
C
               IF (TXEVEN) THEN
                  DO J = 1, SIZE(2)
                     DO I = 1, SIZE(1), 2
                        XARRAYOU(I, J)   =  XARRAYIN(I, J)
                        XARRAYOU(I+1, J) = -XARRAYIN(I+1, J)
                     END DO
                  END DO
               ELSE
                  THETA1 =  - IDIR * 4. * ATAN(1.) * 
     $                 FLOAT(TXSIZE-1)/FLOAT(TXSIZE)
                  THETA = 0.
                  DO I = 1, SIZE(1)
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO J = 1, SIZE(2)
                        XARRAYOU(I, J) = XARRAYIN(I, J) * CFACTOR
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF
C
C Do the FFT 
C
               CALL FFTX2RKL(XARRAYOU, SIZE(2), SIZE(1), 1, IDIR)
C
C Multiply the output to shift the input array to the center
C
               IF (TXP4) THEN
                  DO J = 1, SIZE(2)
                     DO I = 2, SIZE(1), 2
                        XARRAYOU(I, J) = - XARRAYOU(I, J)
                     END DO
                  END DO
               ELSE IF (TXEVEN) THEN
                  DO J = 1, SIZE(2)
                     DO I = 1, SIZE(1), 2
                        XARRAYOU(I, J) = - XARRAYOU(I, J)
                     END DO
                  END DO
               ELSE
                  THETA = - SIZE(1)/2 * THETA1
                  DO I = 1, SIZE(1)
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO J = 1, SIZE(2)
                        XARRAYOU(I, J) = XARRAYOU(I, J) 
     $                       * CFACTOR
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF  
C
C Case for Axis = 2
C
            ELSE
C
C Multiply the input array to shift the output transform to the center
C
               IF (TXEVEN) THEN
                  DO J = 1, SIZE(2), 2
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J) = XARRAYIN(I, J)
                        XARRAYOU(I, J+1) = -XARRAYIN(I, J+1)
                     END DO
                  END DO
               ELSE
                  THETA1 =  - IDIR * 4. * ATAN(1.) * 
     $                 FLOAT(TXSIZE-1)/FLOAT(TXSIZE)
                  THETA = 0.
                  DO J = 1, SIZE(2)
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J) = XARRAYIN(I, J) * CFACTOR
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF
C
C Do the FFT 
C
               CALL FFTX2RKL(XARRAYOU, 1, SIZE(2), SIZE(1), IDIR)
C
C Multiply the output to shift the input array to the center
C
               IF (TXP4) THEN
                  DO J = 2, SIZE(2), 2
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J) = - XARRAYOU(I, J)
                     END DO
                  END DO
               ELSE IF (TXEVEN) THEN
                  DO J = 1, SIZE(2), 2
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J) = - XARRAYOU(I, J)
                     END DO
                  END DO
               ELSE
                  THETA = - SIZE(2)/2 * THETA1
                  DO J = 1, SIZE(2)
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO I = 1, SIZE(1)
                        XARRAYOU(I, J) = XARRAYOU(I, J) 
     $                       * CFACTOR
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF  

            END IF
#ifdef COMP_CONVEX
            END IF
#endif
         ELSE
            CALL ERRREPOR(ERRBDARG, ROUTINE, 'FFT Size less than 1')
         END IF
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 'Transform Axis is Illegal')
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


