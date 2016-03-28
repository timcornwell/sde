C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pspl.f	1.1    1/29/93
      SUBROUTINE SDEMAIN
C
CD Given a 1D Fourier Transform generate its power spectrum (photon limited)
C
C Arguments: CALL SDEMAIN
C Audit trail:
C       Cloned From fft1d
C                               R. G. Marson    Mar 22 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PSPL')
C
      INTEGER                   CRDRNAXM
C
      CHARACTER*(SYSMXNAM) 	IN, IN1, INTYPE, OUT
      CHARACTER			ATYPE, TEMPSTR, FSYS*3, INDTYPE
      INTEGER			NDUMMY, NAX, NAXIS(SYSMXDIM), AXIS
      INTEGER                   RNAX
      INTEGER                   INADD
      LOGICAL			FULL
      INTEGER                   I, J
      COMPLEX                   CDC
      INTEGER                   DC
C==================================================================
C
      CALL MSGWELCO ('I make Photon limited Power Spectra')
      CALL USRCTL
C
C Get User Parameters
C
      CALL USRGETC ('In', IN, 1, NDUMMY)
      CALL USRGETC ('In1', IN1, 1, NDUMMY)
      CALL USRGETC ('InType', INTYPE, 1, NDUMMY)
      CALL USRGETC ('Out', OUT, 1, NDUMMY)
      CALL USRGETI ('Axis', AXIS, 1, NDUMMY)
      CALL USRGETL ('Full', FULL, 1, NDUMMY)
C
C Get the input Image and determine if it is Complex
C
      CALL FILIMGGE ('Image', IN,  ' ')
      CALL FILSYSEX (IN, FSYS)
      IF (FSYS.EQ.'FTS') THEN
         CALL FILIMGGE ('Image1', IN1,  ' ')
         IF (ERROR) GO TO 999
         CALL CRDCHECK ('Image', 'Image1')
         IF (ERROR) GO TO 999
      ELSE IF (FSYS.EQ.'SDE') THEN
         CALL DATGETAR('Image', NAX, NAXIS, ATYPE, INADD)
         IF (ATYPE.EQ.'X') THEN
            INDTYPE = 'X'
         ELSE IF (ATYPE.NE.'R') THEN
            MESSAGE = 'Input Data Must be Complex'
            CALL ERRREPOR(ERRWRGTP, ROUTINE, MESSAGE)
            GOTO 999
         END IF
      END IF
C
C Transform to a complex array
C
      CALL STRUC(INTYPE(1:1), TEMPSTR)
      IF ((TEMPSTR(1:1).EQ.'A').AND.(FSYS.EQ.'FTS')) THEN
         CALL MSGPUT('Converting Amplitude/Phase to Complex', 'I')
         CALL ARRAP2X('Image', 'Image1', 'Complex')
         CALL DATDELAR('Image')
         CALL DATRENAR('Complex', 'Image')
         CALL DATDELET('Image1')
      ELSE IF (FSYS.EQ.'FTS') THEN
         CALL MSGPUT('Converting Real/Imaginary to Complex', 'I')
         CALL ARRQU2X('Image', 'Image1', 'Complex')
         CALL DATDELAR('Image')
         CALL DATRENAR('Complex', 'Image')
         CALL DATDELET('Image1')
      END IF
C         
C Check if we have a suitable image for 1-D transforms on a 2-D image
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, INADD)
      INDTYPE = ATYPE
      RNAX = CRDRNAXM(NAX, NAXIS)
C
C Find where the DC value is
C
      AXIS = MAX(MIN(AXIS,RNAX),1)
      IF (FULL) THEN
         DC = NAXIS(AXIS)/2 
      ELSE
         DC = 0
      END IF
C
C Now just do the multiplication
C
      IF (RNAX.EQ.1) THEN
         CDC = MEMX(INADD + DC)
         DO I = 0, NAXIS(1) - 1
            MEMX(INADD + I) = MEMX(INADD + I) * CONJG(MEMX(INADD + I))
     $                                    - CDC
         END DO
      ELSE IF (RNAX.EQ.2) THEN
         IF (AXIS.EQ.1) THEN
            CALL MSGPUT('------------Check Results--------------', 'W')
            DO I = 0, NAXIS(2)-1
               CDC = MEMX(INADD + DC + NAXIS(2)*I)
               PRINT *, 'DC1 = ', DC + NAXIS(2)*I, CDC
               DO J = 0, NAXIS(1)-1
                  MEMX(INADD + J + I * NAXIS(1)) = 
     $                 MEMX(INADD + J + I * NAXIS(1)) * 
     $                 CONJG(MEMX(INADD + J + I * NAXIS(1))) - CDC
               END DO
            END DO
         ELSE
            DO I = 0, NAXIS(1)-1
               CDC = MEMX(INADD + DC*NAXIS(2)+I)
               IF (REAL(CDC).NE.0.) THEN
                  IF (AIMAG(CDC)/REAL(CDC).GT.1E-6) THEN
                     MESSAGE = 'Warning DC Value is Complex'
                     CALL MSGPUT(MESSAGE, 'W')
                  END IF
               END IF
               DO J = 0, NAXIS(2)-1
                  MEMX(INADD + I + J * NAXIS(1)) = 
     $                 MEMX(INADD + I + J * NAXIS(1)) * 
     $                 CONJG(MEMX(INADD + I + J * NAXIS(1))) - CDC
               END DO
            END DO
         END IF
      ELSE
         MESSAGE = 'Input has too many axis (max 2)'
         CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
         GO TO 999
      END IF
C
C The data should be real by now so just get the real bit
C
      CALL ARRX2QU('Image', 'Power', 'Null')
      CALL DATDELAR('Image')
      CALL DATDELAR('Null')
C
C Sort out the header and Co-ords down here
C
      CALL HEDCOPY('Image', 'Power')
      CALL HISCOPY('Image', 'Power')
      CALL HISINPUT('Power')
C
C Save the Power Spectrum
C
         CALL FILIMGPU ('Power', OUT, ' ')
C
 999  CONTINUE
      END
