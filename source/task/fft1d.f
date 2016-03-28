C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)fft1d.f	1.3    6/3/93
      SUBROUTINE SDEMAIN
C
CD FFT Anything to Anything, as long as its gridded
C
C Arguments: CALL SDEMAIN
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 14 1990
C       Totally re-written to use the new FFT routines.
C         Initial rewrite only does 1-D transforms on a 2D array
C         (but there are big plans)
C                               R. G. Marson    Jan 2 1992
C       Added ability to do 1d R->X Transforms 
C       Added ability to do 1d X->X Transforms 
C       Added ability to do 1d X->R Transforms 
C       Added ability to do 1d X->R Transforms on 2D array
C                               R. G. Marson    Mar 2 1992
C       Added ability to do 2d R->X Transforms
C                              R.G. Marson      APr 7 1992
C       Added ability to do 1d X->X Transforms on a 2D Array
C                              R.G. Marson      Apr 14 1992
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'FFT1D')
C
      INTEGER                   CRDRNAXM, STRLEN
      LOGICAL                   FILEXIST
C
      CHARACTER*(SYSMXNAM) 	IN, IN1, INTYPE, OUT, OUT1, OUTTYPE
      CHARACTER			ATYPE, TEMPSTR, FSYS*3, INDTYPE
      INTEGER			NDUMMY, NAX, NAXIS(SYSMXDIM), AXIS(2)
      INTEGER                   RNAX, SIZE2(2), DIMEN, SIZE1, SIZE3(3)
      INTEGER                   INADD, OUADD, DIR, XADD
      LOGICAL			FULL
      CHARACTER*8       CTYPE(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
C==================================================================
C
      CALL MSGWELCO ('I Do FFT''s of Images')
      CALL USRCTL
C
C Get User Parameters
C
      CALL USRGETC ('In', IN, 1, NDUMMY)
      CALL USRGETC ('In1', IN1, 1, NDUMMY)
      CALL USRGETC ('InType', INTYPE, 1, NDUMMY)
      CALL USRGETC ('Out', OUT, 1, NDUMMY)
      CALL USRGETC ('Out1', OUT1, 1, NDUMMY)
      CALL USRGETC ('OutType', OUTTYPE, 1, NDUMMY)
      CALL USRGETI ('Axis', AXIS, 2, NDUMMY)
      CALL USRGETI ('Dimension', DIMEN, 1, NDUMMY)
      CALL USRGETI ('Dir', DIR, 1, NDUMMY)
      CALL USRGETL ('Full', FULL, 1, NDUMMY)
C
C Get the input Image and determine if it is Real or Complex
C
      CALL FILIMGGE ('Image', IN,  ' ')
      CALL FILSYSEX (IN, FSYS)
      IF ((FSYS.EQ.'FTS').AND.FILEXIST(IN1)) THEN
         CALL FILIMGGE ('Image1', IN1,  ' ')
         IF (ERROR) GO TO 999
         CALL CRDCHECK ('Image', 'Image1')
         IF (ERROR) THEN
            CALL ERRREASO(MESSAGE)
            IF (MESSAGE(1:STRLEN(MESSAGE)).EQ.ERRBDARG) THEN
               CALL ERRCANCE
            ELSE
               GO TO 999
            END IF
         ELSE
            INDTYPE = 'X'
         END IF
      ELSE IF (FSYS.EQ.'SDE') THEN
         CALL DATGETAR('Image', NAX, NAXIS, ATYPE, INADD)
         IF (ATYPE.EQ.'X') THEN
            INDTYPE = 'X'
         ELSE IF (ATYPE.NE.'R') THEN
            MESSAGE = 'Input Data Type not Supported'
            CALL ERRREPOR(ERRWRGTP, ROUTINE, MESSAGE)
            GOTO 999
         END IF
      END IF
C
      IF (INDTYPE.EQ.'X') THEN
         MESSAGE = 'Input Data is Complex'
      ELSE
         INDTYPE = 'R'
         MESSAGE = 'Input Data is Real'
      END IF
      CALL MSGPUT (MESSAGE, 'I')
      CALL MSGPUT ('Coordinates:', 'I')
      CALL CRDLIST ('Image')
C
C Transform to a complex array
C
      CALL STRUC(INTYPE(1:1), TEMPSTR)
      IF ((INDTYPE.EQ.'X').AND.(TEMPSTR(1:1).EQ.'A')
     $     .AND.(FSYS.EQ.'FTS')) THEN
         CALL MSGPUT('Converting Amplitude/Phase to Complex', 'I')
         CALL ARRAP2X('Image', 'Image1', 'Complex')
         CALL DATDELET('Image1')
         CALL DATDELAR('Image')
         CALL DATRENAR('Complex', 'Image')
      ELSE IF ((INDTYPE.EQ.'X').AND.(FSYS.EQ.'FTS')) THEN
         CALL MSGPUT('Converting Real/Imaginary to Complex', 'I')
         CALL ARRQU2X('Image', 'Image1', 'Complex')
         CALL DATDELET('Image1')
         CALL DATDELAR('Image')
         CALL DATRENAR('Complex', 'Image')
      END IF
C         
C Find dimensions of Image
C
      CALL DATGETAR ('Image', NAX, NAXIS, ATYPE, INADD)
      INDTYPE = ATYPE
      CALL CRDRGET
     $     ('Image', RNAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C 1-D Transforms Done in this block
C
      IF (DIMEN.EQ.1) THEN
C
C     Real Packed Transforms
C
         IF ((ATYPE.EQ.'R').AND.(.NOT.FULL)) THEN
            IF (RNAX.LE.0) NAXIS(1) = 1
            IF (RNAX.LE.1) THEN
               SIZE1 = NAXIS(1)
               NAXIS(1) = SIZE1/2+1
               CALL DATMAKAR('Transform', 1, NAXIS, 'X', OUADD)
               WRITE(MESSAGE, 500) SIZE1
 500           FORMAT('Doing a ',I8, ' point R->X transform')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT1RX(MEMR(INADD), MEMX(OUADD), SIZE1)
               RPIX(1) = 1.
               RVAL(1) = 0.
               DELT(1) = 1./(FLOAT(SIZE1)*DELT(1))
            ELSE IF (RNAX.EQ.2) THEN
               IF ((AXIS(1).GE.1).AND.(AXIS(1).LE.2)) THEN
                  SIZE2(1) = NAXIS(1)
                  SIZE2(2) = NAXIS(2)
                  NAXIS(AXIS(1)) = NAXIS(AXIS(1))/2 + 1
                  NAXIS(3-AXIS(1)) = NAXIS(3-AXIS(1))
                  CALL DATMAKAR('Transform', 2, NAXIS, 'X', OUADD)
                  WRITE(MESSAGE, 501) SIZE2(3-AXIS(1)), SIZE2(AXIS(1))
 501              FORMAT('Doing ',I8,':',I8, ' point R->X transforms')
                  CALL MSGPUT(MESSAGE,'I')
                  CALL FFT12RX
     $                 (MEMR(INADD), MEMX(OUADD), 
     $                 SIZE2(1), SIZE2(2), AXIS(1))
               ELSE
                  WRITE(MESSAGE, 220) AXIS(1)
 220              FORMAT('Cannot Transform along Axis #',I3)
                  CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
                  GOTO 999
               END IF
            ELSE IF (RNAX.EQ.3) THEN
               MESSAGE = 
     $              '1-D FFT''s on 3-D Data not yet implemented'
               CALL MSGPUT(MESSAGE, 'E')
            ELSE
               MESSAGE = 
     $              'Cannot FFT array''s with more than 3 Dimensions'
               CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
               GOTO 999
            END IF
C
C     Real UnPacked Transforms
C
         ELSE IF ((ATYPE.EQ.'R').AND.(FULL)) THEN
            IF (RNAX.LE.0) NAXIS(1) = 1
            IF (RNAX.LE.1) THEN
               SIZE1 = NAXIS(1)
               CALL DATMAKAR('Complex', 1, NAXIS, 'X', XADD)
               CALL PIXR2X(MEMR(INADD), MEMX(XADD), SIZE1)
               CALL DATDELAR('Image')
               CALL DATMAKAR('Transform', 1, NAXIS, 'X', OUADD)
               WRITE(MESSAGE, 507) SIZE1
 507           FORMAT('Doing a ',I8, ' point Full R->X transform')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT1XX(MEMX(XADD), MEMX(OUADD), -1, SIZE1)
               RPIX(1) = FLOAT(SIZE1/2+1)
               RVAL(1) = 0.
               DELT(1) = 1./(FLOAT(SIZE1)*DELT(1))
            ELSE IF (RNAX.EQ.2) THEN
               SIZE2(1) = NAXIS(1)
               SIZE2(2) = NAXIS(2)
               CALL DATMAKAR('Complex', 2, NAXIS, 'X', XADD)
               CALL PIXR2X(MEMR(INADD), MEMX(XADD), SIZE2(1)*SIZE2(2))
               CALL DATDELAR('Image')
               CALL DATMAKAR('Transform', 2, NAXIS, 'X', OUADD)
               WRITE(MESSAGE, 508) SIZE2(3-AXIS(1)), SIZE2(AXIS(1))
 508           FORMAT('Doing ',I8,':',I8, ' point Full R->X transforms')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT12XX
     $              (MEMX(XADD), MEMX(OUADD), DIR, 
     $              SIZE2(1), SIZE2(2), AXIS(1))
            ELSE IF (RNAX.EQ.3) THEN
               MESSAGE = 
     $              '1-D FFT''s on 3-D Data not yet implemented'
               CALL MSGPUT(MESSAGE, 'E')
            ELSE
               MESSAGE = 
     $              'Cannot FFT array''s with more than 3 Dimensions'
               CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
               GOTO 999
            END IF
C
C     Complex Transforms
C
         ELSE IF ((ATYPE.EQ.'X').AND.(FULL)) THEN
            IF (RNAX.LE.0) NAXIS(1) = 1
            IF (RNAX.LE.1) THEN
               SIZE1 = NAXIS(1)
               CALL DATMAKAR('Transform', 1, NAXIS, 'X', OUADD)
               IF (DIR.LT.0) THEN
                  WRITE(MESSAGE, 502) SIZE1, 'Inverse'
               ELSE
                  WRITE(MESSAGE, 502) SIZE1, 'Forward'
               END IF
 502           FORMAT('Doing a ',I8, ' point  ',A,' X->X transform')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT1XX(MEMX(INADD), MEMX(OUADD), DIR, SIZE1)
            ELSE IF (RNAX.EQ.2) THEN
               SIZE2(1) = NAXIS(1)
               SIZE2(2) = NAXIS(2)
               CALL DATMAKAR('Transform', 2, NAXIS, 'X', OUADD)
               IF (DIR.LT.0) THEN
                  WRITE(MESSAGE, 506) 
     $                 SIZE2(3-AXIS(1)), SIZE2(AXIS(1)), 'Inverse'
               ELSE
                  WRITE(MESSAGE, 506) 
     $                 SIZE2(3-AXIS(1)), SIZE2(AXIS(1)), 'Forward'
               END IF
 506           FORMAT('Doing ',I8,':',I8,' point ',A,' X->X transforms')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT12XX
     $              (MEMX(INADD), MEMX(OUADD), DIR, 
     $              SIZE2(1), SIZE2(2), AXIS(1))
            ELSE IF (RNAX.EQ.3) THEN
               MESSAGE = 
     $              '1-D FFT''s on 3-D Data not yet implemented'
               CALL MSGPUT(MESSAGE, 'E')
            ELSE
               MESSAGE = 
     $              'Cannot FFT array''s with more than 3 Dimensions'
               CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
               GOTO 999
            END IF
         ELSE IF ((ATYPE.EQ.'X').AND.(.NOT.FULL)) THEN
            MESSAGE = 'The Imaginary component of the Nyquest Frequency'
            CALL MSGPUT(MESSAGE, 'W')
            MESSAGE = 'is assumed to be zero. ie. The original Array'
            CALL MSGPUT(MESSAGE, 'W')
            MESSAGE = 'size is assumed to be an even number'
            CALL MSGPUT(MESSAGE, 'W')
            IF (RNAX.LE.0) NAXIS(1) = 1
            IF (RNAX.LE.1) THEN
               SIZE1 = 2 * (NAXIS(1) - 1)
               NAXIS(1) = SIZE1
               CALL DATMAKAR('Transform', 1, NAXIS, 'R', OUADD)
               WRITE(MESSAGE, 503) SIZE1
 503           FORMAT('Doing a ',I8, ' point X->R transform')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT1XR(MEMX(INADD), MEMR(OUADD), SIZE1)
            ELSE IF (RNAX.EQ.2) THEN
               IF ((AXIS(1).GE.1).AND.(AXIS(1).LE.2)) THEN
                  NAXIS(AXIS(1)) = 2 * (NAXIS(AXIS(1)) - 1)
                  NAXIS(3-AXIS(1)) = NAXIS(3-AXIS(1))
                  SIZE2(1) = NAXIS(1)
                  SIZE2(2) = NAXIS(2)
                  CALL DATMAKAR('Transform', 2, NAXIS, 'R', OUADD)
                  WRITE(MESSAGE, 504) SIZE2(3-AXIS(1)), SIZE2(AXIS(1))
 504              FORMAT('Doing ',I8,':',I8, ' point X->R transforms')
                  CALL MSGPUT(MESSAGE,'I')
                  CALL FFT12XR(MEMX(INADD), MEMR(OUADD), 
     $                 SIZE2(1), SIZE2(2), AXIS(1))
               ELSE
                  WRITE(MESSAGE, 220) AXIS(1)
                  CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
                  GOTO 999
               END IF
            ELSE IF (RNAX.EQ.3) THEN
               MESSAGE = 
     $              '1-D FFT''s on 3-D Data not yet implemented'
               CALL MSGPUT(MESSAGE, 'E')
            ELSE
               MESSAGE = 
     $              'Cannot FFT array''s with more than 3 Dimensions'
               CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
               GOTO 999
            END IF
         END IF
C
C 2-D Transforms Done in this block
C
      ELSE IF (DIMEN.EQ.2) THEN
         IF ((ATYPE.EQ.'R').AND.(.NOT.FULL)) THEN
            IF (RNAX.LE.0) NAXIS(1) = 1
            IF (RNAX.LE.1) NAXIS(2) = 1
            IF (RNAX.LE.2) THEN
               SIZE2(1) = NAXIS(1)
               SIZE2(2) = NAXIS(2)
               NAXIS(1) = SIZE2(1)/2+1
               CALL DATMAKAR('Transform', 2, NAXIS, 'X', OUADD)
               WRITE(MESSAGE, 505) SIZE2(1), SIZE2(2)
 505           FORMAT('Doing a ',I8, ' by ', I8' point R->X transform')
               CALL MSGPUT(MESSAGE,'I')
               CALL FFT2RX(MEMR(INADD), MEMX(OUADD), SIZE2)
            ELSE IF (RNAX.LE.3) THEN
               MESSAGE = 
     $         '2-D FFT''s on 3-D Data not yet implemented'
               CALL MSGPUT(MESSAGE, 'E')
            ELSE
               MESSAGE =
     $              'Cannot FFT array''s with more than 3 Dimensions' 
               CALL MSGPUT(MESSAGE, 'E')
            END IF
         ELSE IF ((ATYPE.EQ.'R').AND.(.NOT.FULL)) THEN
            MESSAGE = ' Currently only Packed Transforms'
            CALL MSGPUT(MESSAGE, 'E')
         ELSE IF (ATYPE.EQ.'X') THEN
            MESSAGE = 
     $           '2-D Transforms of Complex data not yet implemented'
         END IF
C
C 3-D Transforms Done in this block
C
      ELSE IF (DIMEN.EQ.3) THEN
         MESSAGE = 'Three Dimensional FFT''s Not yet implemented'
         CALL MSGPUT(MESSAGE, 'E')
      ELSE
         CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $        'Only FFT''s of dimension 1-3 allowed')
         GOTO 999
      END IF
C
C Sort out the header and Co-ords down here
C
      CALL HEDCOPY('Image', 'Transform')
      CALL CRDPUT
     $     ('Transform', RNAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL HISCOPY('Image', 'Transform')
      CALL HISINPUT('Transform')
      
C
C Check if this is being saved as a FITS file. If so it will need 
C  to be split into two files.
C      
      CALL FILSYSEX(OUT, FSYS)
      IF ((FSYS(1:3).EQ.'FTS').AND.
     $     .NOT.((INDTYPE.EQ.'X').AND.(.NOT.FULL))) THEN
         CALL STRUC(OUTTYPE(1:1), TEMPSTR)
         IF (TEMPSTR.EQ.'R') THEN
            CALL MSGPUT('Converting Complex to Real/Imaginary', 'I')
            CALL ARRX2QU('Transform', 'Real', 'Imag')
            CALL HEDCOPY('Transform', 'Real')
            CALL HISCOPY('Transform', 'Real')
            CALL FILIMGPU ('Real', OUT, ' ')
            CALL HEDCOPY('Transform', 'Imag')
            CALL HISCOPY('Transform', 'Imag')
            CALL FILIMGPU ('Imag', OUT1, ' ')
         ELSE
            CALL MSGPUT('Converting Complex to Amplitude/Phase', 'I')
            CALL ARRX2AP('Transform', 'Amplitude', 'Phase')
            CALL HEDCOPY('Transform', 'Amplitude')
            CALL HISCOPY('Transform', 'Amplitude')
            CALL FILIMGPU ('Amplitude', OUT, ' ')
            CALL HEDCOPY('Transform', 'Phase')
            CALL HISCOPY('Transform', 'Phase')
            CALL FILIMGPU ('Phase', OUT1, ' ')
         END IF
      ELSE
C
C Save the Transformed Image
C
         CALL FILIMGPU ('Transform', OUT, ' ')
      END IF
C
 999  CONTINUE
      END
