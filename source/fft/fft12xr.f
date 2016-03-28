C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft12xr.f	1.3    6/3/93
C
      SUBROUTINE FFT12XR (XARRAY, RARRAY, SIZE1, SIZE2, AXIS)
C
CD Multiple One dimensional FFT's Complex -> Real. On a 2D array
C
C This routine acts as a switchyard for a generic 
C 1 dimensional  Complex -> Real FFT. The data is embedded in a 2 D array, 
C Axis specifies which axis is to be transformed. eg. Axis = 1 implies
C transform along the first axis.
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C
C	XARRAY	COMPLEX(SIZE1/2*SIZE2 + SIZE(AXIS)) Input Array
C	RARRAY	REAL(SIZE1*SIZE2)	Output Array
C	SIZE	INTEGER(2) 	size of the above Arrays
C       AXIS    INTEGER         which axis to transform along (1 or 2)
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Feb 9 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameters 
C
      INTEGER           SIZE1, SIZE2, AXIS
      COMPLEX           XARRAY(*)
      REAL              RARRAY(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT12XR')

C
C Function definitions
C
      LOGICAL           FFTF235
C
C Local Variables
C
      COMPLEX           CFACTOR
      REAL              THETA, THETA1
      INTEGER           NAXIS(SYSMXDIM), RADD, IADD, I, J, K, IERR
      INTEGER           TXSIZE, NTX, TOTSIZE, TX2P1, SIZE(2)
      LOGICAL           NTXEVEN
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
         TOTSIZE = (TXSIZE + 2) * NTX
         IF (((NTX/2)*2).EQ.NTX) THEN
            NTXEVEN = .TRUE.
         ELSE
            NTXEVEN = .FALSE.
         END IF
         IF (TXSIZE.GT.1) THEN
#ifdef COMP_CONVEX
C
C Radix 2,3,5 FFT
C
            IF (FFTF235(TXSIZE)) THEN
               NAXIS(1) = TOTSIZE
               CALL DATMKGAR('SCRATCH/FFT12XR/R', 1, NAXIS, 'R', RADD)
               IF (AXIS.EQ.1) THEN
                  TX2P1 = TXSIZE/2 + 1
               ELSE
                  TX2P1 = TXSIZE/2
               END IF
               K = 0
               IF (AXIS.EQ.1) THEN
                  DO J = 0, (NTX - 1) * TX2P1, TX2P1
                     DO I = 2, TXSIZE + 2, 4
                        MEMR(RADD + K + I - 2) = REAL(XARRAY(J+I/2))
                        MEMR(RADD + K + I - 1) = AIMAG(XARRAY(J+I/2))
                     END DO
                     DO I = 4, TXSIZE + 2, 4
                        MEMR(RADD + K + I - 2) = - REAL(XARRAY(J+I/2))
                        MEMR(RADD + K + I - 1) = - AIMAG(XARRAY(J+I/2))
                     END DO
                     K = K + TXSIZE + 2
                  END DO
                  CALL SRCFTS(MEMR(RADD),TXSIZE,1,NTX,TXSIZE+2,-1,IERR)
                  DO I = 0, SIZE(2) - 1
                     DO J = 1, SIZE(1)
                        RARRAY(I * SIZE(1) + J) = 
     $                       MEMR(RADD + I * (SIZE(1)+2) + J - 1)
                     END DO
                  END DO
               ELSE
                  DO J = 1, NTX
                     DO I = 0, TXSIZE/2, 2
                        MEMR(RADD + 2*I*NTX + J - 1    ) = 
     $                                        REAL(XARRAY(I*NTX+J))
                        MEMR(RADD + 2*I*NTX + J - 1 + NTX) =
     $                                        AIMAG(XARRAY(I*NTX+J))
                     END DO
                     DO I = 1, TXSIZE/2, 2
                        MEMR(RADD + 2*I*NTX + J - 1      ) = 
     $                                     - REAL( XARRAY(I*NTX+J))
                        MEMR(RADD + 2*I*NTX + J - 1 + NTX) = 
     $                                     - AIMAG(XARRAY(I*NTX+J))
                     END DO
                  END DO
                  CALL SRCFTS(MEMR(RADD), TXSIZE, NTX, NTX, 1,-1, IERR)
                  DO I = 0, SIZE(2) - 1
                     DO J = 1, SIZE(1)
                        RARRAY(I * SIZE(1) + J) = 
     $                       MEMR(RADD + I * SIZE(1) + J - 1)
                     END DO
                  END DO
               END IF
            ELSE
#endif
C
C Basic Scalar FFT is Singletons FFT
C
            NAXIS(1) = SIZE(1)
            NAXIS(2) = SIZE(2)
            CALL DATMKGAR('SCRATCH/FFT12XR/I', 2, NAXIS, 'R', IADD)
            TX2P1 = TXSIZE/2 + 1
            IF (AXIS.EQ.1) THEN
               IF ((TXSIZE/2)*2 .EQ.TXSIZE) THEN
                  DO I = 0, NTX-1
                     RARRAY(I*TXSIZE + 1) = REAL(XARRAY(I*TX2P1 + 1))
                     MEMR(IADD+I*TXSIZE) = 0.0
                     DO J = 1, TX2P1 - 1, 2
                        RARRAY(TXSIZE * I + J + 1) = 
     $                       - REAL(XARRAY(TX2P1 * I + J + 1))
                        MEMR(IADD + TXSIZE * I + J) = 
     $                       -  AIMAG(XARRAY(TX2P1 * I + J + 1))
                        RARRAY(TXSIZE * I + TXSIZE - J + 1) = 
     $                       -  REAL(XARRAY(TX2P1 * I + J + 1))
                        MEMR(IADD + TXSIZE * I + TXSIZE - J) = 
     $                       + AIMAG(XARRAY(TX2P1 * I + J + 1))
                     END DO
                     DO J = 2, TX2P1 - 1, 2
                        RARRAY(TXSIZE * I + J + 1) = 
     $                       +  REAL(XARRAY(TX2P1 * I + J + 1))
                        MEMR(IADD + TXSIZE * I + J) = 
     $                       +  AIMAG(XARRAY(TX2P1 * I + J + 1))
                        RARRAY(TXSIZE * I + TXSIZE - J + 1) = 
     $                       +  REAL(XARRAY(TX2P1 * I + J + 1))
                        MEMR(IADD + TXSIZE * I + TXSIZE - J) = 
     $                       - AIMAG(XARRAY(TX2P1 * I + J + 1))
                     END DO
                  END DO
               ELSE
                  THETA1 = -4. * ATAN(1.) * 
     $                 FLOAT(TXSIZE-1)/FLOAT(TXSIZE)
                  THETA = THETA1
                  DO I = 0, NTX-1
                     RARRAY(TXSIZE * I + 1) = REAL( 
     $                    XARRAY(TX2P1 * I + 1))
                     MEMR(IADD + TXSIZE * I) = 0.
                  END DO
                  DO J = 2, TX2P1
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO I = 0, NTX-1
                        RARRAY(TXSIZE * I + J) = REAL( 
     $                       XARRAY(TX2P1 * I + J) * CFACTOR)
                        MEMR(IADD + TXSIZE * I + J - 1) = AIMAG( 
     $                       XARRAY(TX2P1 * I + J) * CFACTOR)
                        RARRAY(TXSIZE * (I+1) - J + 2) = REAL( 
     $                       XARRAY(TX2P1 * I + J) * CFACTOR)
                        MEMR(IADD + TXSIZE * (I+1) - J + 1) = - AIMAG( 
     $                       XARRAY(TX2P1 * I + J) * CFACTOR)
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF
               CALL FFT_V2(RARRAY, MEMR(IADD), NTX, TXSIZE, 1, 1) 
            ELSE
               IF ((TXSIZE/2)*2 .EQ.TXSIZE) THEN
                  DO I = 1, NTX
                     RARRAY(I) = REAL(XARRAY(I))
                     MEMR(IADD + I-1) = 0.0
                     DO J = 1, TX2P1 - 1, 2
                        RARRAY(J * NTX + I) = 
     $                       - REAL(XARRAY(J * NTX + I))
                        MEMR(IADD + J * NTX + I - 1) = 
     $                       - AIMAG(XARRAY(J * NTX + I))
                        RARRAY((TXSIZE-J) * NTX + I) = 
     $                       - REAL(XARRAY(J * NTX + I))
                        MEMR(IADD + (TXSIZE-J) * NTX + I - 1) = 
     $                       + AIMAG(XARRAY(J * NTX + I))
                     END DO
                     DO J = 2, TX2P1 - 1, 2
                        RARRAY(J * NTX + I) = 
     $                       + REAL(XARRAY(J * NTX + I))
                        RARRAY((TXSIZE-J) * NTX + I) = 
     $                       + REAL(XARRAY(J * NTX + I))
                        MEMR(IADD + J * NTX + I - 1) = 
     $                       + AIMAG(XARRAY(J * NTX + I))
                        MEMR(IADD + (TXSIZE-J) * NTX + I - 1) = 
     $                       - AIMAG(XARRAY(J * NTX + I))
                     END DO
                  END DO
               ELSE
                  THETA1 = -4. * ATAN(1.) * 
     $                 FLOAT(TXSIZE-1)/FLOAT(TXSIZE)
                  THETA = THETA1
                  DO I = 1, NTX
                     RARRAY(I) = REAL(XARRAY(I))
                     MEMR(IADD + I + 1) = 0.
                  END DO
                  DO J = 1, TX2P1 - 1
                     CFACTOR = CMPLX(COS(THETA),SIN(THETA))
                     DO I = 1, NTX
                        RARRAY(NTX * J + I) = REAL( 
     $                       XARRAY(NTX * J + I) * CFACTOR)
                        MEMR(IADD + NTX * J + I - 1) = AIMAG( 
     $                       XARRAY(NTX * J + I) * CFACTOR)
                        RARRAY(NTX * (TXSIZE - J) + I) = REAL( 
     $                       XARRAY(NTX * J + I) * CFACTOR)
                        MEMR(IADD + NTX * (TXSIZE - J) + I-1) = -AIMAG(
     $                       XARRAY(NTX * J + I) * CFACTOR)
                     END DO
                     THETA = THETA + THETA1
                  END DO
                  
               END IF
               CALL FFT_V2(RARRAY, MEMR(IADD), 1, TXSIZE, NTX, 1) 
            END IF
#ifdef COMP_CONVEX
            END IF
#endif
         ELSE IF (TXSIZE.EQ.1) THEN
            XARRAY(1) = RARRAY(1)
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


