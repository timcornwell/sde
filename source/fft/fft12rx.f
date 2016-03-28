C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft12rx.f	1.2    2/12/93
C
      SUBROUTINE FFT12RX (RARRAY, XARRAY, SIZE1, SIZE2, AXIS)
C
CD Multiple One dimensional FFT's Real -> Complex. On a 2D array
C
C This routine acts as a switchyard for a generic 
C 1 dimensional  Real -> Complex FFT. The data is embedded in a 2 D array, 
C Axis specifies which axis is to be transformed. eg. Axis = 1 implies
C transform along the first axis.
C It looks at the dimensions of the input array and calls the appropriate
C routine depending on the machine and factors of the input array.
C If necessary it will allocate work space for the FFT to do its stuff
C
C	RARRAY	REAL(SIZE1*SIZE2)	Input Array
C	XARRAY	COMPLEX(SIZE1/2*SIZE2 + SIZE(AXIS)) Output Array
C	SIZE1,2	INTEGER 	size of the above Arrays
C       AXIS    INTEGER         which axis to transform along (1 or 2)
C
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
      INTEGER           SIZE1, SIZE2, AXIS
      COMPLEX           XARRAY(*)
      REAL              RARRAY(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT12RX')

C
C Function definitions
C
      LOGICAL           FFTF235
C
C Local Variables
C
      REAL              THETA, THETA1, TX2P1, ONE
      INTEGER           NAXIS(SYSMXDIM), RADD, IADD, I, J, K, IERR
      INTEGER           TXSIZE, NTX, TOTSIZE, TADD, SIZE(2)
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
         IF (TXSIZE.GE.1) THEN
#ifdef COMP_CONVEX
C
C Radix 2,3,5 FFT
C
            IF (FFTF235(TXSIZE)) THEN
               IF (AXIS.EQ.1) THEN
                  TX2P1 = TXSIZE/2 + 1
               ELSE
                  TX2P1 = TXSIZE/2
               END IF
               K = 0
               DO J = 0, (NTX - 1) * TX2P1, TX2P1
                  DO I = 2, TXSIZE, 2
                     XARRAY(J + I/2) = CMPLX(RARRAY(K+I-1), RARRAY(K+I))
                  END DO
                  K = K + TXSIZE
               END DO
               IF (AXIS.EQ.1) THEN
                  DO I = 1, NTX
                     XARRAY(I * TX2P1) = (0., 0.)
                  END DO
                  CALL SRCFTS(XARRAY, TXSIZE, 1, NTX, TXSIZE+2, 1, IERR)
                  DO J = 0, (NTX - 1) * TX2P1, TX2P1
                     DO I = 2, TX2P1, 2
                        XARRAY(I + J) = - XARRAY(I + J)
                     END DO
                  END DO
               ELSE
                  DO I = 1, NTX
                     XARRAY(NTX*TXSIZE/2 + I) = (0., 0.)
                  END DO
                  CALL SRCFTS(XARRAY, TXSIZE, NTX, NTX, 1, 1, IERR)
                  NAXIS(1) = NTX
                  CALL DATMAKAR('SCRATCH/FFT12RX', 1, NAXIS, 'X', TADD)
                  ONE = 1.
                  DO I = 0, NTX * (TX2P1 + 1), NTX
                     IF (NTXEVEN) THEN
                        DO J = 2, NTX, 2
                           MEMX(TADD-2+J) = CMPLX(REAL(XARRAY(J/2+I)),
     $                          REAL(XARRAY(NTX/2+J/2+I)))
                           MEMX(TADD-1+J) = CMPLX(AIMAG(XARRAY(J/2+I)),
     $                          AIMAG(XARRAY(NTX/2+J/2+I)))
                        END DO
                     ELSE
                        DO J = 2, NTX, 2
                           MEMX(TADD-2+J) = CMPLX(REAL(XARRAY(J/2+I)),
     $                          AIMAG(XARRAY(NTX/2+J/2+I)))
                           MEMX(TADD-1+J) = CMPLX(AIMAG(XARRAY(J/2+I)),
     $                          REAL(XARRAY(NTX/2+J/2+I+1)))
                        END DO
                        J = NTX + 1
                        MEMX(TADD-2+J) = CMPLX(REAL(XARRAY(J/2+I)),
     $                       AIMAG(XARRAY(NTX/2+J/2+I)))
                     END IF
                     DO J = 1, NTX
                        XARRAY(J+I) = ONE * MEMX(TADD-1+J)
                     END DO
                     ONE = -1. * ONE
                  END DO
                  CALL DATDELAR('SCRATCH/FFT12RX')
               END IF
            ELSE
#endif
C
C Basic Scalar FFT is Singletons FFT
C
            NAXIS(1) = SIZE(1)
            NAXIS(2) = SIZE(2)
            CALL DATMAKAR('SCRATCH/FFT12RX/R', 2, NAXIS, 'R', RADD)
            CALL DATMAKAR('SCRATCH/FFT12RX/I', 2, NAXIS, 'R', IADD)
            DO I = 0, SIZE(1)*SIZE(2) - 1
               MEMR(RADD + I) = RARRAY(I + 1)
               MEMR(IADD + I) = 0.0
            END DO
            IF (AXIS.EQ.1) THEN
               CALL FFT_V2(MEMR(RADD), MEMR(IADD), NTX, TXSIZE, 1, -1) 
            ELSE
               CALL FFT_V2(MEMR(RADD), MEMR(IADD), 1, TXSIZE, NTX, -1) 
            END IF
            IF (AXIS.EQ.1) THEN
               IF (2*(TXSIZE/2).EQ.TXSIZE) THEN
                  K = 1
                  DO J = 0, (NTX-1)*TXSIZE, TXSIZE
                     ONE = 1
                     DO I = 0, TXSIZE/2
                        XARRAY(K) = ONE *
     $                       CMPLX(MEMR(RADD + I + J),
     $                             MEMR(IADD + I + J))
                        K = K + 1
                        ONE = -1 * ONE
                     END DO
                  END DO
               ELSE
                  K = 1
                  THETA1 = (4. * ATAN(1.0) * 
     $                      FLOAT(TXSIZE - 1) / FLOAT(TXSIZE))
                  DO J = 0, (NTX-1)*TXSIZE, TXSIZE
                     THETA = 0
                     DO I = 0, TXSIZE/2 
                        XARRAY(K) = CMPLX(MEMR(RADD + I + J),
     $                                    MEMR(IADD + I + J)) * 
     $                              CMPLX(COS(THETA), SIN(THETA))
                        THETA = THETA + THETA1
                        K = K + 1
                     END DO
                  END DO
               END IF
            ELSE
               IF (2*(TXSIZE/2).EQ.TXSIZE) THEN
                  K = 1
                  ONE = 1
                  DO J = 0, TXSIZE/2*NTX, NTX
                     DO I = 0, NTX-1
                        XARRAY(K) = ONE *
     $                       CMPLX(MEMR(RADD + I + J),
     $                             MEMR(IADD + I + J))
                        K = K + 1
                     END DO
                     ONE = -1 * ONE
                  END DO
               ELSE
                  K = 1
                  THETA1 = (4. * ATAN(1.0) * 
     $                      FLOAT(TXSIZE - 1) / FLOAT(TXSIZE))
                  THETA = 0
                  DO J = 0, TXSIZE/2*NTX, NTX
                     DO I = 0, NTX-1
                        XARRAY(K) = CMPLX(MEMR(RADD + I + J),
     $                                    MEMR(IADD + I + J)) * 
     $                              CMPLX(COS(THETA), SIN(THETA))
                        K = K + 1
                     END DO
                     THETA = THETA + THETA1
                  END DO
               END IF
               
            END IF
            CALL DATDELAR('SCRATCH/FFT12RX/R')
            CALL DATDELAR('SCRATCH/FFT12RX/I')
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


