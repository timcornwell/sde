C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsratm.f	1.2    7/27/93
C
      SUBROUTINE SDEMAIN
C     
CD    Program to remove the atmospheric effects from IPCS data
C     
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcsfft
C                                         R.G. Marson     Dec 4 1992
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSRATM')
C
C Function Declarations
C   
      REAL              DATFGETR, IPCWAVE
      INTEGER           DATFGETI, STRLEN, DATADD
C     
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, WINDOW, TRACKFILE
      REAL PAD, WAVE(2), SCALE, MAXDLY, MINDLY, DLYSTP
      INTEGER NDUMMY, FR, IND, YSHIFT, NANT, I, J
      INTEGER HEIGHT, WIDTH, PHEIGHT, FRSTART, FREND
      INTEGER NPHOTONS, NUMPH, NPIX, NOTIFY
      INTEGER NUMDLY
      INTEGER OADD, TADD, TXADD, XADD, YADD, FRADD, DADD, TRADD
      INTEGER ANTLADD, ANTDADD, VLAADD, OXADD, WADD, WINADD
      LOGICAL DONORM
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C     
C==================================================================
C     
      CALL MSGWELCO ('I remove the atmosphere from IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 990
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
C
C Check that the data is sorted the way we want it
C
      CALL IPCSSORT('IPCS', 'IF')
C
C Get Sizes of arrays
C
      WIDTH = DATFGETI('IPCS', 'WINDOW_W')
      HEIGHT = DATFGETI('IPCS', 'WINDOW_H')
      FRSTART = DATFGETI('IPCS', 'FR_START')
      FREND = DATFGETI('IPCS', 'FR_END')
C
C Get array addresses
C
      DADD = DATADD('IPCS')
      XADD = DATADD('IPCS/XPIXEL')
      YADD = DATADD('IPCS/YPIXEL')
      FRADD = DATADD('IPCS/FRAME')
C
C Get the Track File (must be an SDE file created by IPCSHMM)
C
      CALL USRGETC ('Trackfile', TRACKFILE, 1, NDUMMY)
      CALL DATCREAT('TRACK')
      CALL DATREAD('TRACK', TRACKFILE)
      IF (ERROR) GOTO 990
      CALL DATCHKTP('TRACK' , 'TRACK')
      TRADD = DATADD('TRACK')
C
C Get Antenna Info
C
      NANT = DATFGETI('TRACK/ANT', 'NANT')
      ANTLADD = DATADD('TRACK/ANT/ANTLOC')
      ANTDADD = DATADD('TRACK/ANT/ANTDIAM')
C
C Get Model Info
C
      NUMDLY = DATFGETI('TRACK', 'NUMDLY')
      MINDLY = DATFGETR('TRACK', 'MINDLY')
      MAXDLY = DATFGETR('TRACK', 'MAXDLY')
      DLYSTP = (MAXDLY - MINDLY) / FLOAT(NUMDLY)
      CALL DATGETR('TRACK', 'WAVE', WAVE, 2, NDUMMY)
      SCALE = DATFGETR('TRACK', 'SCALE')
C
C Allocate the wavelength cache
C
      NAXIS(1) = WIDTH
      CALL DATMAKAR('WAVE', 1, NAXIS, 'R', WADD)
C
C Get Padding Factors
C
      CALL USRGETR('Pad', PAD, 1, NDUMMY)
      PHEIGHT = NINT(PAD*HEIGHT)
      WRITE (MESSAGE, '(A, I4, A, I4, A)') 'Data window size is ',
     $     WIDTH, ' by ', HEIGHT, ' pixels'
      CALL MSGPUT(MESSAGE, 'I')
      IF (PHEIGHT.NE.HEIGHT) THEN
         WRITE (MESSAGE, '(A, I4, A, I4, A)') 'Padded window size is ',
     $        WIDTH, ' by ', PHEIGHT, ' pixels'
         CALL MSGPUT(MESSAGE, 'I')
      END IF
C
C Get window to use and apply it
C
      CALL USRGETC('Window', WINDOW, 1, NDUMMY)
      CALL STRUC(WINDOW, MESSAGE)
      WINDOW = MESSAGE
      WRITE(MESSAGE, '(A, A, A)') 'Using a ', 
     $     WINDOW(1:STRLEN(WINDOW)), ' window'
      CALL MSGPUT(MESSAGE, 'I')
      IF (WINDOW(1:3).NE.'UNI') THEN
         NAXIS(1) = HEIGHT
         CALL DATMAKAR('WIN', 1, NAXIS, 'R', WINADD)
         CALL IMGMKWI1(MEMR(WINADD), HEIGHT, WINDOW, 1.0)
C#         DO I = 0, hEIGHT -1
C#            PRINT *, I, MEMR(WINADD + I)
C#         END DO
         DO I = 0, NPHOTONS - 1
C#            PRINT *, I, MEMR(DADD + I), MEMR(XADD+I), 
C#     $           MEMR(DADD + I) * MEMR(WINADD + MEMR(XADD+I))
            MEMR(DADD + I) = MEMR(DADD + I) 
     $           * MEMR(WINADD + MEMR(YADD+I))
         END DO
      END IF
C
C Do the padding by scaling the photon locations
C
      YSHIFT = (PHEIGHT-HEIGHT) / 2 + 1
      CALL PIXRSCAL(MEMR(YADD), 1.0, FLOAT(YSHIFT),MEMR(YADD),NPHOTONS)
      CALL PIXRSCAL(MEMR(XADD), 1.0, 1.0, MEMR(XADD), NPHOTONS)
C
C Allocate the output array
C
      NAXIS(1) = WIDTH
      NAXIS(2) = PHEIGHT/2 + 1
      CALL DATMAKAR('OUTPUT', 2, NAXIS, 'R', OADD)
      CALL DATMAKAR('OUTX', 2, NAXIS, 'X', OXADD)
      CALL PIXXSETC(MEMX(OXADD), 0.0, (PHEIGHT/2+1)*WIDTH)
C
C Allocate tempory arrays to hold the data pre and post FFT'ing
C
      NAXIS(1) = WIDTH
      NAXIS(2) = PHEIGHT
      CALL DATMAKAR('TEMP', 2, NAXIS, 'R', TADD)
      NAXIS(2) = PHEIGHT/2 + 1
      CALL DATMAKAR('XTEMP', 2, NAXIS, 'X', TXADD)
      NPIX = WIDTH * (PHEIGHT/2 + 1)
C
C Allocate and initialise the visibility lookup array.
C
      NAXIS(1) = NANT
      NAXIS(2) = NANT
      NAXIS(3) = WIDTH
      CALL DATMAKAR('VLA', 3, NAXIS, 'R', VLAADD)
      CALL IPCVLA(MEMR(VLAADD), NANT, WIDTH, 
     $     MEMR(ANTLADD),MEMR(ANTDADD), WAVE, PHEIGHT, 
     $     SCALE, MEMR(WADD))
C
C Main Loop
C
      DO FR = FRSTART, FREND
         IF (MOD(FR-FRSTART+1,NOTIFY).EQ.0) THEN
            WRITE(MESSAGE, '(A, I6, A, I6)') 
     $           'Doing Frame ', FR-FRSTART+1, 
     $           ' out of ', FREND-FRSTART + 1
            CALL MSGPUT(MESSAGE, 'I')
         END IF
C
C Search for the start and end points of the frame
C
         CALL IPCWIN(MEMR(FRADD), NPHOTONS, FR, FR+1, IND, NUMPH)
         IND = IND - 1
C
C Put this data into a 2D array
C
         CALL PIXRSETC(MEMR(TADD), 0.0, PHEIGHT*WIDTH)
         CALL PIXRSCAT(MEMR(XADD+IND), MEMR(YADD+IND), MEMR(DADD+IND), 
     $        NUMPH, MEMR(TADD), WIDTH, PHEIGHT)
C
C FFT the Temp, ARRAY, Putting results into output array
C
         CALL FFT12RX(MEMR(TADD), MEMX(TXADD), WIDTH, PHEIGHT, 2) 
C
C Apply the correction
C
         CALL IPCRATM(MEMX(TXADD), WIDTH, PHEIGHT/2 + 1, MEMR(VLAADD), 
     $        MEMR(TRADD+(FR-FRSTART)*NANT), 
     $        NANT, DLYSTP, PAD, MEMR(WADD))
C
C Accumulate the results
C
         CALL PIXXADD(MEMX(TXADD), MEMX(OXADD), MEMX(OXADD), NPIX)
C#         IF (FR.EQ.FRSTART) CALL DATWRITE('XTEMP', 'T0/xtemp.SDE')
      END DO
C
C Generate the amplitude squared
C
      CALL PIXXAMP2(NPIX, MEMX(OXADD), MEMR(OADD))
C
C Do normalisation if req'd
C
      CALL USRGETL('Normalise', DONORM, 1, NDUMMY)
      IF (DONORM) THEN
         DO I = 0, WIDTH - 1
            DO J = PHEIGHT/2 + 1, 0, -1
               MEMR(OADD + I + J*WIDTH) = MEMR(OADD + I + J*WIDTH) /
     $              MEMR(OADD + I) * NANT * NANT
            END DO
         END DO
      END IF
C
C Write sort out the header and history and write the image out
C 
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
      CALL HISCOPY('TRACK', 'OUTPUT')
      CALL HISPUT('OUTPUT',
     $     'This is the corrected visibility (squared)')
      CALL HISINPUT('OUTPUT')
      CALL CRDGET ('OUTPUT', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CTYPE(1) = 'WAVE'
      RPIX(1) = WIDTH/2 + 1
      RVAL(1) = IPCWAVE(NINT(RPIX(1)), WIDTH, WAVE)
      DELT(1) = (WAVE(2) - WAVE(1))/ WIDTH
      ROTA(1) = 0.
      CTYPE(2) = 'VV--'
      RPIX(2) = 1
      RVAL(2) = 0.
      DELT(2) = 1./SCALE
      ROTA(2) = 0.
      CALL CRDPUT ('OUTPUT', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)

      CALL FILIMGPU('OUTPUT', OUTFILE, ' ')
C
C Clean up and finish
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
