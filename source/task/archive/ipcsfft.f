C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsfft.f	1.3    1/7/94
C
      SUBROUTINE SDEMAIN
C     
CD    Program to fft IPCS data 
C     
C    Arguments: CALL sdemain
C    Audit trail:
C             Cloned from ipcssum
C                                         R.G. Marson     June 17 1991
C    
C-----------------------------------------------------------------------
#include	"stdinc.h"
C     
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSFFT')
C
C Function Declarations
C   
      INTEGER           DATFGETI, STRLEN, DATADD
C     
C Local Variables
C
      CHARACTER*(SYSMXNAM) INFILE, OUTFILE, WINDOW
      REAL PAD, TOTPH
      INTEGER NAXIS(SYSMXDIM)
      INTEGER NDUMMY, DIMEN, FR, IND, XSHIFT, YSHIFT, I, J
      INTEGER HEIGHT, WIDTH, PWIDTH, PHEIGHT, FRSTART, FREND
      INTEGER NPHOTONS, NUMPH, NPIX, NOTIFY, XLOC, NORMALISE, ROW
      INTEGER OADD, TADD, TXADD, XADD, YADD, FRADD, DADD, TOTADD
      INTEGER WINADD
      LOGICAL BIAS
C     
C==================================================================
C     
      CALL MSGWELCO ('I fft IPCS data')
      CALL USRCTL
C     
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
      CALL USRGETL ('Bias', BIAS, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
C     
C     Get IPCS file
C     
      CALL USRGETC ('Infile', INFILE, 1, NDUMMY)
      CALL USRGETC ('Outfile', OUTFILE, 1, NDUMMY)
C
      CALL IPCSGET('IPCS', INFILE, '*', ' ')
      IF (ERROR) GOTO 990
      NPHOTONS = DATFGETI('IPCS', 'GCOUNT')
C
      CALL USRGETI ('Dimen', DIMEN, 1, NDUMMY)
      IF (DIMEN.EQ.1) THEN
         MESSAGE = 'Doing a 1-D FFT along the Columns'
         CALL MSGPUT(MESSAGE, 'I')
      ELSE
         DIMEN = 2
         MESSAGE = 'Doing a 2-D FFT on each Frame'
         CALL MSGPUT(MESSAGE, 'I')
      END IF
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
C Get Padding Factors
C
      CALL USRGETR('Pad', PAD, 1, NDUMMY)
      PHEIGHT = NINT(PAD*HEIGHT)
      IF (DIMEN.EQ.2) THEN
         PWIDTH = NINT(PAD*WIDTH)
      ELSE
         PWIDTH = WIDTH
      END IF
      WRITE (MESSAGE, '(A, I4, A, I4, A)') 'Data window size is ',
     $     WIDTH, ' by ', HEIGHT, ' pixels'
      CALL MSGPUT(MESSAGE, 'I')
      IF (PHEIGHT.NE.HEIGHT) THEN
         WRITE (MESSAGE, '(A, I4, A, I4, A)') 'Padded window size is ',
     $        PWIDTH, ' by ', PHEIGHT, ' pixels'
         CALL MSGPUT(MESSAGE, 'I')
      END IF
C
C A few other houskeeping things
C
      DADD = DATADD('IPCS')
      XADD = DATADD('IPCS/XPIXEL')
      YADD = DATADD('IPCS/YPIXEL')
      FRADD = DATADD('IPCS/FRAME')
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
         DO I = 0, NPHOTONS - 1
            MEMR(DADD + I) = MEMR(DADD + I) 
     $           * MEMR(WINADD + MEMR(YADD+I))
         END DO
      END IF
C
C Do the padding by scaling the photon locations
C
      XSHIFT = (PWIDTH-WIDTH) / 2 + 1
      YSHIFT = (PHEIGHT-HEIGHT) / 2 + 1
      CALL PIXRSCAL(MEMR(XADD), 1.0, FLOAT(XSHIFT),MEMR(XADD),NPHOTONS)
      CALL PIXRSCAL(MEMR(YADD), 1.0, FLOAT(YSHIFT),MEMR(YADD),NPHOTONS)
C
C Create the output array
C
      NAXIS(1) = PWIDTH
      NAXIS(2) = PHEIGHT/2 + 1
      CALL DATMAKAR('OUTPUT', 2, NAXIS, 'R', OADD)
      CALL PIXRSETC(MEMR(OADD), 0., NAXIS(1)*NAXIS(2))
C
C Make a tempory arrays to hold the data pre and post FFT'ing
C
      NAXIS(1) = PWIDTH
      NAXIS(2) = PHEIGHT
      CALL DATMAKAR('TEMP', 2, NAXIS, 'R', TADD)
      NAXIS(2) = PHEIGHT/2 + 1
      CALL DATMAKAR('XTEMP', 2, NAXIS, 'X', TXADD)
      NPIX = PWIDTH * (PHEIGHT/2 + 1)
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
         CALL PIXRSETC(MEMR(TADD), 0.0, PHEIGHT*PWIDTH)
         CALL PIXRSCAT(MEMR(XADD+IND), MEMR(YADD+IND), MEMR(DADD+IND), 
     $        NUMPH, MEMR(TADD), PWIDTH, PHEIGHT)
C
C FFT the Temp, ARRAY, Putting results into output array
C
         IF (DIMEN.EQ.1) THEN
            CALL FFT12RX(MEMR(TADD), MEMX(TXADD), PWIDTH, PHEIGHT, 2) 
         ELSE
            CALL FFT2RX(MEMR(TADD), MEMX(TXADD), PWIDTH, PHEIGHT) 
         END IF
C
C Generate the amplitude squared
C
         CALL PIXXAMP2(NPIX, MEMX(TXADD), MEMR(TADD))
C
C Accumulate the results
C
         CALL PIXRADD(MEMR(TADD), MEMR(OADD), MEMR(OADD), NPIX)
      END DO
C
C Calculate the photon bias
C  For 1-D Transforms each column is seperatly scaled
C
      IF (BIAS) THEN
         CALL MSGPUT('Correcting for photon bias', 'I')
         IF (DIMEN.EQ.1) THEN
            NAXIS(1) = PWIDTH
            CALL DATMAKAR('TOTPH', 1, NAXIS, 'R', TOTADD)
            DO I = 0, PWIDTH - 1
               MEMR(TOTADD + I) = 0.
            END DO
            DO I = 0, NPHOTONS - 1
               XLOC = NINT(MEMR(XADD + I)) - 1
               MEMR(TOTADD + XLOC) = MEMR(TOTADD + XLOC) - MEMR(DADD+I)
            END DO
            DO I = 0, PHEIGHT/2
               CALL PIXRADD(MEMR(OADD+I*PWIDTH), MEMR(TOTADD), 
     $              MEMR(OADD+I*PWIDTH), PWIDTH)
            END DO
         ELSE
            TOTPH = 0.
            DO I = 0, NPHOTONS - 1
               TOTPH = TOTPH - MEMR(DADD+I)
            END DO
            CALL PIXRSCAL(MEMR(OADD), 1.0, TOTPH, MEMR(OADD), NPIX)
         END IF
      END IF
C
C Normalise if Necessary
C
      CALL USRGETI ('Normalise', NORMALISE, 1, NDUMMY)
      IF (NORMALISE.GT.0) THEN
         CALL MSGPUT('Normalising the Data', 'I')
         IF (DIMEN.EQ.1) THEN
            DO J = 0, PWIDTH - 1
               MEMR(OADD + J) = MEMR(OADD + J) /
     $              (REAL(NORMALISE**2))
               IF (ABS(MEMR(OADD + J)).LT.1.E-30) 
     $              MEMR(OADD + J) = 1.E-30
            END DO
            DO I = PHEIGHT/2, 1, -1
               ROW = PWIDTH * I
               DO J = 0, PWIDTH - 1
                  MEMR(OADD + ROW + J) = MEMR(OADD + ROW + J)/ 
     $                 MEMR(OADD + J) 
               END DO
            END DO
            DO J = 0, PWIDTH - 1
               MEMR(OADD + J) = REAL(NORMALISE**2)
            END DO
         ELSE
            CALL MSGPUT
     $         ('I Do not know how to Normalise after a 2D Transform',
     $           'W')
         END IF
      END IF
C
C Write the image out
C 
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
