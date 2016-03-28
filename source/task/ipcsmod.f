C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsmod.f	1.4    7/9/93
C
      SUBROUTINE SDEMAIN
C     
CD   Program to Generate model MAPPIT fringe patterns
C
C    Arguments: CALL sdemain
C    Audit trail:
C             Extracted from ipcssim
C                                         R.G. Marson     June 9 1993
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSSIM')
      REAL              C
      PARAMETER         (C = 2.99792458E8)
      REAL              PI
      PARAMETER         (PI = 3.1415926535897932385)
C
C Function Declarations
C   
      REAL              IPCWAVE
      INTEGER           DATFGETI
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) ANTFILE, FRFILE, SKYFILE, AMPFILE, SPECFILE
      CHARACTER*1 ATYPE
      DOUBLE PRECISION MINX
      REAL WAVE(2), SCALE, PA, MINDLY, MAXDLY, MAXSP, DARK, BACKG
      REAL DMAX, DMIN, AVG, RMS, SUM, DISP
      INTEGER BBTRC(SYSMXDIM), BBBLC(SYSMXDIM), NLOC
      INTEGER FSIZE(2), NDUMMY, NANT
      INTEGER PLANESZ, FFTPAD, CFOSAMP, CFSUPP, CCFSIZE
      INTEGER I, J, NUMDLY, NUMPLANES, NOTIFY
      INTEGER BLC(SYSMXDIM), TRC(SYSMXDIM)
      INTEGER PADD, ANTADD, DATADD, DIAADD, SPECADD
      INTEGER DLYADD, DLSADD, FRIADD, RANTYADD, RDIAMADD
      INTEGER IDLYADD, CFADD, CCFADD, SKYADD, SKYADD1, SKYXADD1, SFADD
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C     
C=======================================================================
C     
      CALL MSGWELCO ('I generate model fringe patterns')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Let the model creation begin!
C
      CALL DATCREAT ('MODEL')
C
C Get the size of each frame
C
      CALL USRGETI ('Fsize', FSIZE, 2, NDUMMY)
      WRITE(MESSAGE, '(A, I3, A, I3, A)') 
     $     'Frame size: ', FSIZE(2), ' rows and ', FSIZE(1), ' columns'
      CALL MSGPUT( MESSAGE, 'I')
      CALL DATPUTI ('MODEL', 'FSIZE', FSIZE, 2)
C
C Get the scaling parameters 
C
      CALL USRGETR ('Scale', SCALE, 1, NDUMMY)
      CALL DATPUTR ('MODEL', 'SCALE', SCALE, 1)
      CALL USRGETR ('Wave', WAVE, 2, NDUMMY)
      WRITE(MESSAGE, '(A, G10.3, A, G10.3, A)') 
     $     'Wavelength range: ', WAVE(1)*1.E9, ' - ', 
     $     WAVE(2)*1.E9, ' nm'
      CALL MSGPUT( MESSAGE, 'I')
      CALL DATPUTR ('MODEL', 'WAVE', WAVE, 2)
C
C Get the Antenna file (Mask Pattern) and normalise it so that all
C  antenna position are greater than zero (except for one which is zero)
C  Note only antenna X positions (in local co-ords) are significant
C
      CALL USRGETC ('AntFile', ANTFILE, 1, NDUMMY)
      CALL FILGETAN('Ant', ANTFILE)
      IF (ERROR) GOTO 990
      ANTADD = DATADD('Ant/LX') 
      DIAADD = DATADD('Ant/DIAM') 
      NANT = DATFGETI('Ant', 'NANT')
      CALL DATCREAT('MODEL/ANT')
      CALL DATPUTI('MODEL/ANT', 'NANT', NANT, 1)
      MINX = MEMD(ANTADD)
      DO I = 1, NANT - 1
         MINX = MIN(MEMD(ANTADD + I), MINX)
      END DO
      NAXIS(1) = NANT
      CALL DATMAKAR('MODEL/ANT/ANTLOC', 1, NAXIS, 'R', RANTYADD)
      CALL DATMAKAR('MODEL/ANT/ANTDIAM', 1, NAXIS, 'R', RDIAMADD)
      DO I = 0, NANT - 1
         MEMR(RANTYADD + I) = REAL(MEMD(ANTADD + I) - MINX)
         MEMR(RDIAMADD + I) = REAL(MEMD(DIAADD + I))
      END DO
C
C Get the parameters describing atmospheric stats
C
      CALL USRGETI ('NumDly', NUMDLY, 1, NDUMMY)
      CALL DATPUTI('MODEL', 'NUMDLY', NUMDLY, 1)
      CALL USRGETR ('MinDly', MINDLY, 1, NDUMMY)
      CALL DATPUTR('MODEL', 'MINDLY', MINDLY, 1)
      CALL USRGETR ('MaxDly', MAXDLY, 1, NDUMMY)
      CALL DATPUTR('MODEL', 'MAXDLY', MAXDLY, 1)
      WRITE(MESSAGE, '(I3, A, I3, A)') 
     $     NANT, ' Holes with ', NUMDLY, ' Delays per hole'
      CALL MSGPUT( MESSAGE, 'I')
      WRITE(MESSAGE, '(A, F10.3, A, F10.3, A)')
     $     'Delay Range: ', MINDLY * 1E15, ' - ', 
     $     MAXDLY * 1E15, ' fs'
      CALL MSGPUT( MESSAGE, 'I')
      DO I = 0, NANT - 1
         WRITE(MESSAGE, '(A, I3, A, 3PF10.2, A, 3PF10.2, A)')
     $        'Hole # ', I+1, ' Position: ', MEMR(RANTYADD + I),
     $        ' mm Diameter: ', MEMR(RDIAMADD + I), ' mm'
         CALL MSGPUT( MESSAGE, 'I')
      END DO
C
C Make arrays to hold our atmospheric fringe pattern parameters
C
      NAXIS(1) = NANT
      CALL DATMAKAR('MODEL/DELAYS', 1, NAXIS, 'R', DLYADD)
      CALL DATMAKAR('MODEL/IDELAYS', 1, NAXIS, 'I', IDLYADD)
      CALL DATMAKAR('MODEL/DLYSLP', 1, NAXIS, 'R', DLSADD)
C
C Define the array containing our ideal fringe pattern
C
      NUMPLANES = NUMDLY**(NANT)
      NAXIS(1) = FSIZE(1)
      NAXIS(2) = FSIZE(2)
      NAXIS(3) = NUMPLANES
      PLANESZ = FSIZE(1) * FSIZE(2)
      CALL DATMAKAR('MODEL', 3, NAXIS, 'R', FRIADD)
      IF (ERROR) GOTO 990
      CALL USRGETI ('FFTpad', FFTPAD, 1, NDUMMY)
      WRITE (MESSAGE, '(A, I3)') 'FFT Padding Factor is ', FFTPAD
      CALL MSGPUT( MESSAGE, 'I')
C
C Get the user specified spectrum of our source (or flat if none specified)
C
      CALL USRGETC('Spectrum', SPECFILE, 1, NDUMMY)
      IF (SPECFILE.NE.' ') THEN
         CALL FILIMGGE('MODEL/SPECTRUM', SPECFILE, ' ')
         CALL DATGETAR('MODEL/SPECTRUM', ANAX, NAXIS, ATYPE, SPECADD)
         IF (NAXIS(1).NE.FSIZE(1)) THEN
            CALL ERRREPOR(ERRBDARG, ROUTINE,
     $           'Spectrum size is not the same as frame X size')
            GOTO 990
         END IF
         MAXSP = MEMR(SPECADD)
         DO I = 1, FSIZE(1) - 1
            MAXSP = MAX(MAXSP, MEMR(SPECADD+I))
         END DO
         DO I = 0, FSIZE(1) - 1
            MEMR(SPECADD+I) = MEMR(SPECADD+I)/MAXSP
         END DO
         
      ELSE
         NAXIS(1) = FSIZE(1)
         CALL DATMAKAR('MODEL/SPECTRUM', 1, NAXIS, 'R', SPECADD)
         CALL PIXRSETC(MEMR(SPECADD), 1.0, NAXIS(1))
      END IF
      IF (SYSDEBUG) THEN
         DO I = 0, FSIZE(1) - 1
            PRINT *, I, MEMR(SPECADD+I)
         END DO
      END IF
C
C Go and find the source model, Collapse it to a 1-D image and Fourier
C  Transform it. For the moment we assume that the source has no
C  wavelength dependent structure and that it is the same brightness at
C  all wavelengths
C
      CALL USRGETC('SkyModel', SKYFILE, 1, NDUMMY)
      CALL USRGETR('PA', PA, 1, NDUMMY)
      CALL DATCREAT('MODEL/SKY')
      CALL FILGETMO('MODEL/SKY/RAWMODEL', SKYFILE)
      CALL MODROTAT('MODEL/SKY/RAWMODEL', PA, 'MODEL/SKY/MODEL')
      CALL DATDELET('MODEL/SKY/RAWMODEL')
      NAXIS(1) = FSIZE(2)
      CALL DATMAKAR('MODEL/SKY/IMAGE1D', 1, NAXIS, 'R', SKYADD1)
      CTYPE(1) = 'DEC-'
      RPIX(1) = FSIZE(2)/2 + 1
      RVAL(1) = 0.
      DELT(1) = SCALE*180./PI
      ROTA(1) = PA
      CALL CRDPUT('MODEL/SKY/IMAGE1D', 1, 
     $     CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL ARRSETCO('MODEL/SKY/IMAGE1D', 0., 0.)
      CALL MODIMGP('MODEL/SKY/MODEL', 'MODEL/SKY/IMAGE1D')
      CALL DATDELET('MODEL/SKY/MODEL')
      IF (SYSDEBUG) THEN
         CALL FILIMGPU('MODEL/SKY/IMAGE1D', 'T0/model.fit', ' ')
      END IF
      NAXIS(1) = FSIZE(2)/2 + 1
      CALL DATMAKAR('MODEL/SKY/IMAGE1DX', 1, NAXIS, 'X', SKYXADD1)
      CALL FFT1RX(MEMR(SKYADD1), MEMX(SKYXADD1), FSIZE(2))
      CALL DATDELAR('MODEL/SKY/IMAGE1D')
      NAXIS(1) = FSIZE(1)
      NAXIS(2) = FSIZE(2)/2 + 1
      CALL DATMAKAR('MODEL/SKY/IMAGE', 2, NAXIS, 'X', SKYADD)
      CALL DATMAKAR('MODEL/SFREQ', 2, NAXIS, 'X', SFADD)
      IF (ERROR) GOTO 990
      DO J = 0, FSIZE(2)/2
         DO I = 0, FSIZE(1) - 1
C This expression has been re-arranged to avoid as SUN compiler bug at -O3
            MEMX(SKYADD + J*FSIZE(1) + I) = 
     $           MEMR(SPECADD+I)/REAL(MEMX(SKYXADD1)) * MEMX(SKYXADD1+J)
         END DO
      END DO
      CALL DATDELAR('MODEL/SKY/IMAGE1DX')
C
C Setup stuff for the fringe scaling
C
      DO I = 1, SYSMXDIM
         BLC(I) = 1
         TRC(I) = 1
      END DO
      TRC(1) = FSIZE(1)
      TRC(2) = FSIZE(2)
C 
C For various efficiency reasons in griding data the pupil is transposed
C 
      NAXIS(1) = FSIZE(2) * FFTPAD
      NAXIS(2) = FSIZE(1)
      CALL DATMAKAR('MODEL/PUPIL', 2, NAXIS, 'X', PADD)
C
C Generate the spheriodal convolution function. This is needed by IPCMKFR
C  but there is no point in putting its calculation inside the loop
C
      NAXIS(1) = ((1 + 3) * 128) * 2 + 1
      CALL DATMAKAR('MODEL/CFFN', 1, NAXIS, 'R', CFADD)
      CALL GRDCF('SF', MEMR(CFADD), CFSUPP, CFOSAMP)
      CCFSIZE = FFTPAD*FSIZE(2)/ 2 + 1
      NAXIS(1) = CCFSIZE
      CALL DATMAKAR('MODEL/CCFFN', 1, NAXIS, 'R', CCFADD)
      CALL GRDCCF('BOX', MEMR(CCFADD), CCFSIZE)
C 
C Find out what the expected dark current is
C 
      CALL USRGETR('Dark', DARK, 1, NDUMMY)
      BACKG = MAX(MIN(DARK, 1.0), 0.0)
C
C Now generate the ideal fringe patterns for each delay 
C
      CALL USRGETI ('Notify', NOTIFY, 1, NDUMMY)
      DO I = 0, NUMPLANES - 1
         IF (MOD(I, NOTIFY).EQ.(NOTIFY-1)) THEN
            WRITE(MESSAGE, '(A, I5, A, I5)') 'Generated ', I+1,
     $           ' Planes out of ', NUMPLANES
            CALL MSGPUT( MESSAGE, 'I')
         END IF
         CALL IPCNXTDL(I, MEMR(DLYADD), MEMI(IDLYADD), 
     $        NANT, NUMDLY, MINDLY, MAXDLY)
         CALL IPCMKFR(MEMR(FRIADD + I * PLANESZ), 
     $        FSIZE(1), FSIZE(2), FFTPAD*FSIZE(2), 
     $        WAVE, NANT, MEMR(RANTYADD), 
     $        MEMR(RDIAMADD), MEMR(DLYADD), MEMR(DLSADD), SCALE, 
     $        MEMX(PADD), MEMR(CFADD), CFSUPP, CFOSAMP, 
     $        MEMR(CCFADD), CCFSIZE)
C
C Add in the source model
C
         CALL FFT12RX(MEMR(FRIADD + I * PLANESZ), MEMX(SFADD), 
     $        FSIZE(1), FSIZE(2), 2)
         CALL PIXXMULT(MEMX(SFADD), MEMX(SKYADD), MEMX(SFADD), 
     $        FSIZE(1)*(FSIZE(2)/2 + 1))
         CALL FFT12XR(MEMX(SFADD), MEMR(FRIADD + I * PLANESZ),
     $        FSIZE(1), FSIZE(2), 2)
C
C Normalise so that sum over entire fringe pattern is one
C
         CALL PIXRSTAT(MEMR(FRIADD + I * PLANESZ), FSIZE(1), FSIZE(2), 
     $        1, 1, 1, 1, 1, BLC, TRC, 
     $        DMAX, DMIN, AVG, RMS, SUM, DISP, NLOC, BBBLC, BBTRC)
         CALL PIXRSCAL(MEMR(FRIADD + I * PLANESZ), (1.-BACKG)/SUM, 0., 
     $        MEMR(FRIADD + I * PLANESZ), PLANESZ) 
C
C Add in a uniform background haze corresponding to scattered light or 
C  Detector dark current
C
         IF (BACKG.GT.1E-30) THEN
            DO J = 0, PLANESZ - 1
               MEMR(FRIADD + I * PLANESZ + J ) = BACKG/PLANESZ + 
     $              MEMR(FRIADD + I * PLANESZ + J )
            END DO
         END IF
      END DO
      CALL DATDELAR('MODEL/PUPIL')
C
C Save the fringe file.
C Lets get some co-ords into this fringe pattern. The horizontal 
C  direction (fastest moving axis) is temporal frequency or wavelength.
C  In either case its not linear, so I'll make an approximate linearisation
C  when computing DELT(1). The vertical axis is the fourier transform of
C  spatial frequency ie. angle. I'll assign this to declination.
C
      CALL CRDGET('MODEL', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CTYPE(1) = 'WAVE'
      CTYPE(2) = 'DEC-'
      CTYPE(3) = 'INDX'
      RPIX(1) = FSIZE(1)/2 + 1
      RPIX(2) = FSIZE(2)/2 + 1
      RPIX(3) = 1.
      RVAL(1) = IPCWAVE(NINT(RPIX(1)), FSIZE(1), WAVE)
      RVAL(2) = 0.
      RVAL(3) = 1.
      DELT(1) = (WAVE(2) - WAVE(1))/ FSIZE(1)
      DELT(2) = SCALE*180./PI
      DELT(3) = 1.
      ROTA(1) = 0.
      ROTA(2) = PA
      ROTA(3) = 0.
      CALL CRDPUT('MODEL', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CALL HISOPEN('MODEL')
      CALL HISPUT('MODEL', 'This is the Fringe MODEL file')
      CALL HISINPUT('MODEL')
      CALL DATSETTP('MODEL', 'FRINGEMODEL')
C
C Get Name of output file(s)
C
      CALL USRGETC ('FrFile', FRFILE, 1, NDUMMY)
      CALL FILIMGPU('MODEL', FRFILE, ' ')
C
C Save the amplitude squared model too (for later reference)
C
      CALL USRGETC('AmpModel', AMPFILE, 1, NDUMMY)
      IF (AMPFILE.NE.' ') THEN
         CALL ARRX2AP('MODEL/SKY/IMAGE', 'AMP', 'PHASE')
         CALL DATDELAR('PHASE')
         CALL ARRMULT('AMP', 'AMP', 'AMP2')
         CALL DATDELAR('AMP')
         CALL HISOPEN('AMP2')
         CALL HISPUT('AMP2', 
     $        'This is the Sky Model amplitudes (squared)')
         CALL HISINPUT('AMP2')
         CALL CRDGET('AMP2', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CTYPE(1) = 'WAVE'
         RPIX(1) = FSIZE(1)/2 + 1
         RVAL(1) = IPCWAVE(NINT(RPIX(1)), FSIZE(1), WAVE)
         DELT(1) = (WAVE(2) - WAVE(1))/ FSIZE(1)
         ROTA(1) = 0.
         CTYPE(2) = 'VV--'
         RPIX(2) = 1
         RVAL(2) = 0.
         DELT(2) = 1./SCALE
         ROTA(2) = PA
         CALL CRDPUT('AMP2', ANAX, CTYPE, NAXIS, RVAL, 
     $        RPIX, DELT, ROTA)
         CALL FILIMGPU('AMP2', AMPFILE, ' ')
         CALL DATDELAR('AMP2')
      END IF
C
C Clean up and finish
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
