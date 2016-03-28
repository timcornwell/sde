C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcssim.f	1.6    7/27/93
C
      SUBROUTINE SDEMAIN
C     
CD   Program to Generate simulated IPCS data
C
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcsdp
C                                         R.G. Marson     Aug 1 1991
C             Model Creation stuff split off into ipcsmod
C                                         R. G Marson     Jun 9 1993
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
      REAL              DATFGETR, IPCWAVE
      INTEGER           DATFGETI, DLY2INDX
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) IPCSFILE, FRFILE, TRFILE
      CHARACTER*1 ATYPE
      REAL AVPHOTNS, EXPFACT, LOC, WAVE(2)
      REAL MINDLY, MAXDLY, TZERO, RZERO, CLAMBDA, FTIME, DARK
      INTEGER NFRAMES, FSIZE(2), SEED, IPCSSIZE, NDUMMY, NANT
      INTEGER FRINDX, PHOTONS, PHINDX, OUTINDX, XLOC, YLOC
      INTEGER PLANESZ, INDEX, NUMDLY2
      INTEGER I, NUMDLY, NUMPLANES, NOTIFY
      INTEGER FRADD, DADD, XADD, YADD, DATADD
      INTEGER FRIADD, RANTYADD, RDIAMADD, TRADD, IDLYADD
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C     
C=======================================================================
C     
      CALL MSGWELCO ('I generate simulated IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get Name of output file(s)
C
      CALL USRGETC ('IPCSfile', IPCSFILE, 1, NDUMMY)
      CALL USRGETC ('Frfile', FRFILE, 1, NDUMMY)
      CALL USRGETC ('Trackfile', TRFILE, 1, NDUMMY)
C
C Get the Model (MUST be an SDE file)
C
      CALL DATCREAT ('MODEL')
      CALL DATREAD ('MODEL', FRFILE)
      CALL DATCHKTP('MODEL', 'FRINGEMODEL')
      NANT = DATFGETI('MODEL/ANT', 'NANT')
      NUMDLY = DATFGETI('MODEL', 'NUMDLY')
      IF (ERROR) GOTO 990
      WRITE(MESSAGE, '(I3, A, I3, A)') 
     $     NANT, ' Holes with ', NUMDLY, ' Delays per hole'
      CALL MSGPUT( MESSAGE, 'I')
      MINDLY = DATFGETR('MODEL', 'MINDLY')
      MAXDLY = DATFGETR('MODEL', 'MAXDLY')
      IF (ERROR) GOTO 990
      WRITE(MESSAGE, '(A, F10.3, A, F10.3, A)')
     $     'Delay Range: ', MINDLY * 1E9, ' - ', 
     $     MAXDLY * 1E15, ' fs'
      CALL MSGPUT( MESSAGE, 'I')
      RANTYADD = DATADD('MODEL/ANT/ANTLOC')
      RDIAMADD = DATADD('MODEL/ANT/ANTDIAM')
      DO I = 0, NANT - 1
         WRITE(MESSAGE, '(A, I3, A, 3PF10.2, A, 3PF10.2, A)')
     $        'Hole # ', I+1, ' Position: ', MEMR(RANTYADD + I),
     $        ' mm Diameter: ', MEMR(RDIAMADD + I), ' mm'
         CALL MSGPUT( MESSAGE, 'I')
      END DO
C
C Set up a few variables for later on
C
      NUMPLANES = NUMDLY**(NANT)
      CALL DATGETAR('MODEL', ANAX, NAXIS, ATYPE, FRIADD)
      FSIZE(1) = NAXIS(1)
      FSIZE(2) = NAXIS(2)
      PLANESZ = FSIZE(1) * FSIZE(2)
      IDLYADD = DATADD('MODEL/IDELAYS')
      CALL DATGETR('MODEL', 'WAVE', WAVE, 2, NDUMMY)
C
C Now turn the ideal fringe patterns into cumulative distributions
C
      DO I = 0, NUMPLANES - 1
         CALL IPCMKCM(MEMR(FRIADD + I * PLANESZ), 
     $        FSIZE(1), FSIZE(2))
      END DO
C
C Determine index of center plane and set the initial delays to that value
C
      NUMDLY2 = (NUMDLY-1)/2 + 1
      DO I = 0, NANT - 1
         MEMI(IDLYADD + I) = NUMDLY2
      END DO
      INDEX = DLY2INDX(MEMI(IDLYADD), NANT, NUMDLY)
C     
C     Estimate Size of output IPCS file 
C     
      CALL USRGETR ('Photons', AVPHOTNS, 1, NDUMMY)
      IF (PHOTONS.LT.0) PHOTONS = -1E-7
      CALL USRGETR ('Dark', DARK, 1, NDUMMY)
      IF (DARK.LT.0) DARK = -1E-7
      CALL USRGETI ('Frames', NFRAMES, 1, NDUMMY)
      CALL USRGETR ('Expansion', EXPFACT, 1, NDUMMY)
      IPCSSIZE = NINT((AVPHOTNS+DARK)*NFRAMES*EXPFACT)
      WRITE(MESSAGE, '(A, I6, A, F7.1, A)')
     $     'Generating ', NFRAMES, ' Frames with an average of ', 
     $     AVPHOTNS+DARK, ' Photons/Frame'
      CALL MSGPUT( MESSAGE, 'I')
C
C Create the Output IPCS Data Arrays
C
      NAXIS(2) = IPCSSIZE
      NAXIS(1) = 1
      CALL DATMAKAR('IPCS', 2, NAXIS, 'R', DADD)
      NAXIS(1) = IPCSSIZE
      CALL DATMAKAR('IPCS/XPIXEL', 1, NAXIS, 'R', XADD)
      CALL DATMAKAR('IPCS/YPIXEL', 1, NAXIS, 'R', YADD)
      CALL DATMAKAR('IPCS/FRAME', 1, NAXIS, 'R', FRADD)
C
C Get the Track file organised
C
      NAXIS(1) = NANT
      NAXIS(2) = NFRAMES
      CALL DATMAKAR('TRACK', 2, NAXIS, 'R', TRADD)
C
C Get the atmospheric parameters and normalize them
C
      CALL USRGETR ('Tzero', TZERO, 1, NDUMMY)
      CALL USRGETR ('Rzero', RZERO, 1, NDUMMY)
      CALL USRGETR ('Ftime', FTIME, 1, NDUMMY)
      WRITE(MESSAGE, '(A, 3PF6.2, A, 3PF6.2, A)')
     $     'Each Frame is ', FTIME, ' ms and Tzero is ', 
     $     TZERO, ' ms'
      CALL MSGPUT( MESSAGE, 'I')
C
C Calculate the varience on one frame time (in radians) given that
C the variance after Tzero seconds is Pi radians.
C Note:    D(T)=A.(t/T)^(5/3)=Varience=sigma^2
C
      TZERO = (FTIME/TZERO)**(5./3.) * PI
C
C Now convert this to a time delay at the center frequency
C
      CLAMBDA = IPCWAVE(FSIZE(1)/2, FSIZE(1), WAVE)
      TZERO = TZERO * CLAMBDA/C / (2 * PI)
C
C Now normalise this time delay by the delay increment
C
      TZERO = TZERO*FLOAT(NUMDLY)/(MAXDLY-MINDLY)
      WRITE(MESSAGE,'(A, F8.2)')  
     $     'Frame to frame varience (in delay steps) ', TZERO
      CALL MSGPUT(MESSAGE, 'I')
C
C Convert the Varience (= sigma^2) to sigma
C
      TZERO = SQRT(TZERO)
C
C Main loop (each iteration corresponds to one frame)
C
      CALL USRGETI ('Seed', SEED, 1, NDUMMY)
      SEED = SEED*4+1
      OUTINDX = 0
      CALL USRGETI ('Notify', NOTIFY, 1, NDUMMY)
      DO FRINDX = 0, NFRAMES - 1
         IF (MOD(FRINDX, NOTIFY).EQ.(NOTIFY-1)) THEN
            WRITE(MESSAGE, '(A, I6, A, I6)') 'Generated ', FRINDX+1,
     $           ' Frames out of ', NFRAMES
            CALL MSGPUT( MESSAGE, 'I')
         END IF
C
C Save current Delays in the track file
C
         DO I = 0, NANT - 1
            MEMR(TRADD + FRINDX*NANT + I) = FLOAT(MEMI(IDLYADD + I))
         END DO
C
C Find out how many real photons in this frame
C
         IF (AVPHOTNS.GE.0.) THEN
            CALL UTLPOISS(AVPHOTNS, PHOTONS, SEED)
         ELSE
            PHOTONS = 0
         END IF
C
C Now find where these photons are located 
C
         DO PHINDX = OUTINDX, OUTINDX + PHOTONS - 1
C
C For the moment we assume that there is no wavelength dependent
C  structure so that any wavelngth column is equally likely
C
            CALL UTLRAND(LOC, SEED)
            XLOC = INT(LOC * FSIZE(1)) + 1
C
C Determine which vertical column the data photon arrived in
C
            CALL IPCLOC(MEMR(FRIADD+INDEX*PLANESZ),
     $           FSIZE(1), FSIZE(2), XLOC, YLOC, SEED)
C
C Now save all this info in the array
C
            MEMR(DADD+PHINDX) = 1.0
            MEMR(XADD+PHINDX) = XLOC - 1
            MEMR(YADD+PHINDX) = YLOC - 1
            MEMR(FRADD+PHINDX) = FRINDX
         END DO
         OUTINDX = OUTINDX + PHOTONS
C
C Determine the fringe pattern to be used for the next frame
C
         CALL IPCNXTFR(MEMI(IDLYADD), NANT, NUMDLY, INDEX, 
     $        TZERO, RZERO, SEED)
C
C Find out how many random photons in this frame
C
         IF (DARK.GE.0.) THEN
            CALL UTLPOISS(DARK, PHOTONS, SEED)
         ELSE
            PHOTONS = 0
         END IF
C
C Now find where these photons are located 
C
         DO PHINDX = OUTINDX, OUTINDX + PHOTONS - 1
C
C Dark current is a random detector noise which should be uniform
C  in both axis on the detector
C
            CALL UTLRAND(LOC, SEED)
            XLOC = INT(LOC * FSIZE(1))
            CALL UTLRAND(LOC, SEED)
            YLOC = INT(LOC * FSIZE(2))
C
C Now save all this info in the array
C
            MEMR(DADD+PHINDX) = 1.0
            MEMR(XADD+PHINDX) = XLOC
            MEMR(YADD+PHINDX) = YLOC
            MEMR(FRADD+PHINDX) = FRINDX
         END DO
         OUTINDX = OUTINDX + PHOTONS
      END DO
C
C We now know how big the final data so adjust the final array sizes
C
      CALL DATGETAR('IPCS', NDUMMY, NAXIS, ATYPE, DADD)
      NAXIS(1) = 1
      NAXIS(2) = OUTINDX
      CALL DATPUTI('IPCS/ARRAY', 'NAXIS', NAXIS, 2)
      NAXIS(1) = OUTINDX
      CALL DATPUTI('IPCS/XPIXEL/ARRAY', 'NAXIS', NAXIS, 1)
      CALL DATPUTI('IPCS/YPIXEL/ARRAY', 'NAXIS', NAXIS, 1)
      CALL DATPUTI('IPCS/FRAME/ARRAY', 'NAXIS', NAXIS, 1)
C
C Now fix up the header of the IPCS file
C
      CALL DATPUTI('IPCS', 'GCOUNT', OUTINDX, 1)
      CALL DATPUTI('IPCS', 'WINDOW_W', FSIZE(1), 1)
      CALL DATPUTI('IPCS', 'WINDOW_H', FSIZE(2), 1)
      CALL DATPUTC('IPCS', 'SORT', 'IF', 1)
      CALL DATPUTI('IPCS', 'FR_START', 0, 1)
      CALL DATPUTI('IPCS', 'FR_END', NFRAMES-1, 1)
      CALL DATPUTR('IPCS', 'FTIME', FTIME, 1)
      CALL DATSETTP('IPCS', 'IPCS')
      CALL HISOPEN('IPCS')
      CALL HISCOPY('MODEL', 'IPCS')
      CALL HISPUT('IPCS', 'This is the IPCS (simulated data) file')
      CALL HISINPUT('IPCS')
C
C And write out the file
C
      CALL IPCSPUT ('IPCS', IPCSFILE)
C
C Do the same for the Track file
C
      IF (TRFILE.NE.' ') THEN
         CALL HEDCOPY('MODEL', 'TRACK')
         CALL DATRENAM('MODEL/ANT', 'TRACK/ANT')
         CALL CRDGET
     $        ('TRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CTYPE(1) = 'ANTENNA'
         CTYPE(2) = 'FRAME'
         RVAL(1) = 1.
         RVAL(2) = 0.
         RPIX(1) = 1.
         RPIX(2) = 1.
         DELT(1) = 1.
         DELT(2) = 1.
         ROTA(1) = 0.
         ROTA(2) = 0.
         CALL CRDPUT
     $        ('TRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL HISOPEN('TRACK')
         CALL HISPUT('TRACK', 'This is the Real Delay TRACK file')
         CALL HISINPUT('TRACK')
         CALL DATSETTP('TRACK', 'TRACK')
         CALL FILIMGPU('TRACK', TRFILE, ' ')
         CALL DATRENAM('TRACK/ANT', 'MODEL/ANT')
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
