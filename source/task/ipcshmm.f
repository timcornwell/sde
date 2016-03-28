C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcshmm.f	1.3    6/23/93
C
      SUBROUTINE SDEMAIN
C
CD Recovers fringes in MAPPIT data using the HIdden Markov Model
C
C More extensive description with details etc.
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 7 1992
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSHMM')
      REAL              C
      PARAMETER         (C = 2.99792458E8)
      REAL              PI
      PARAMETER         (PI = 3.1415926535897932385)
C
C Function definitions
C
      REAL                 DATFGETR, IPCWAVE
      INTEGER              DATFGETI
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) PDFFILE, TRFILE, OTRFILE
      CHARACTER*1          ATYPE
      REAL                 MAXPROB
      REAL                 TZERO, RZERO, MINDLY, MAXDLY, FTIME, WAVE(2)
      REAL                 CLAMBDA, CPROB
      INTEGER              NDUMMY, NOTIFY, FSIZE(2)
      INTEGER              FR, NFRINGES, NFRAMES
      INTEGER              I, FRSTART, FREND, NUMDLY
      INTEGER              NANT, MAXI
      INTEGER              PRADD
      INTEGER              TRADD, IDLYADD, ATADD
      INTEGER              TKADD, TCADD, WADD, OTRADD
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      CALL MSGWELCO('I recover the Track on MAPPIT data')
C
C Get Input Parameters
C
      CALL USRCTL
      CALL USRGETC('Prob', PDFFILE, 1, NDUMMY)
      CALL USRGETC ('Trackfile', TRFILE, 1, NDUMMY)
      CALL USRGETC ('OldTrackfile', OTRFILE, 1, NDUMMY)
      CALL USRGETR('Tzero', TZERO, 1, NDUMMY)
      CALL USRGETR('Rzero', RZERO, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
C
C Get PDF Data (must be an SDE file created with ipcsprob)
C
      CALL DATCREAT('PROB')
      CALL DATREAD ('PROB', PDFFILE)
      CALL DATCHKTP('PROB', 'PROBABILITIES')
      CALL DATGETAR('PROB', ANAX, NAXIS, ATYPE, PRADD) 
      NFRINGES = NAXIS(1)
      NFRAMES = NAXIS(2)
      FRSTART = 0
      FREND = NFRAMES - 1
      FTIME = DATFGETR('PROB', 'FTIME')
      CALL DATGETR('PROB', 'WAVE', WAVE, 2, NDUMMY)
      CALL DATGETI('PROB', 'FSIZE', FSIZE, 2, NDUMMY)
      NANT = DATFGETI('PROB/ANT', 'NANT')
      NUMDLY = DATFGETI('PROB', 'NUMDLY')
      MINDLY = DATFGETR('PROB', 'MINDLY')
      MAXDLY = DATFGETR('PROB', 'MAXDLY')
      NAXIS(1) = NANT
      CALL DATMAKAR('Delay', 1, NAXIS, 'I', IDLYADD)
      IF (ERROR) GOTO 990
C
C Print out info on the Data
C
      WRITE(MESSAGE, '(A, I6, A)') 'Data has ', NFRAMES, 
     $     ' Frames'
      CALL MSGPUT(MESSAGE, 'I')
C
C Print out info on the Model
C
      WRITE(MESSAGE, '(A, I3, A, I3, A)') 'Model has ', 
     $     NANT, ' Holes with ', NUMDLY, ' Delays per hole'
      CALL MSGPUT( MESSAGE, 'I')
      WRITE(MESSAGE, '(A, F10.3, A, F10.3, A)')
     $     'Delay Range: ', MINDLY * 1E15, ' - ', 
     $     MAXDLY * 1E15, ' fs'
      CALL MSGPUT( MESSAGE, 'I')
C
C Do the non HMM file?
C
      IF (OTRFILE.NE.' ') THEN
C
C Get the non HMM track file allocated
C
         NAXIS(1) = NANT
         NAXIS(2) = NFRAMES
         CALL DATMAKAR('OTRACK', 2, NAXIS, 'R', OTRADD)
C
C Do for each Frame
C
         MESSAGE = 'Starting the Non-HMM search'
         CALL MSGPUT(MESSAGE, 'I')
         DO FR = FRSTART, FREND
            IF (MOD(FR-FRSTART+1,NOTIFY).EQ.0) THEN
               WRITE(MESSAGE, '(A, I6, A, I6)') 
     $              'Doing Frame ', FR-FRSTART+1, 
     $              ' out of ', FREND-FRSTART + 1
               CALL MSGPUT(MESSAGE, 'I')
            END IF
C
C Sort out the pseudo Track file (The Psuedo Track file
C  is the track file generated by just taking the Max Prob at each time step
C  regardless of the time or spatial dynamics).
C
            MAXPROB = MEMR(PRADD+NFRINGES*(FR-FRSTART))
            DO I = NFRINGES - 1, 1, -1
               CPROB = MEMR(PRADD+NFRINGES*(FR-FRSTART)+I)
               IF (CPROB.GT.MAXPROB) THEN
                  MAXPROB = CPROB
                  MAXI = I
               END IF
            END DO
            CALL INDX2DLY(MAXI, MEMI(IDLYADD), NANT, NUMDLY)
            DO I = 0, NANT - 1
               MEMR(OTRADD + (FR-FRSTART)*NANT + I) = 
     $              FLOAT(MEMI(IDLYADD + I))
            END DO
C
C Next Frame
C
         END DO
C
C Get the header done and save the file to disk
C
         CALL DATPUTI('OTRACK', 'NUMDLY', NUMDLY, 1)
         CALL DATPUTR('OTRACK', 'MINDLY', MINDLY, 1)
         CALL DATPUTR('OTRACK', 'MAXDLY', MAXDLY, 1)
         CALL CRDGET
     $        ('OTRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
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
         CALL HISCOPY('PROB', 'OTRACK')
         CALL HISPUT('OTRACK', 'This is the Non-HMM Delay TRACK file')
         CALL HISINPUT('OTRACK')
         CALL DATRENAM('PROB/ANT', 'OTRACK/ANT')
         CALL HEDCOPY('PROB', 'OTRACK')
         CALL CRDPUT
     $        ('OTRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL DATSETTP('OTRACK', 'TRACK')
         CALL FILIMGPU('OTRACK', OTRFILE, ' ')
         CALL DATRENAM('OTRACK/ANT', 'PROB/ANT')
      END IF
C
C Do the HMM file?
C
      IF (TRFILE.NE.' ') THEN
C
C Get the Track file and the non HMM track file organised
C
         NAXIS(1) = NANT
         NAXIS(2) = NFRAMES
         CALL DATMAKAR('TRACK', 2, NAXIS, 'R', TRADD)
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
         WRITE(MESSAGE,'(A, F6.2)')  
     $        'Frame to frame varience (in delay steps) ', TZERO
         CALL MSGPUT(MESSAGE, 'I')
C
C Initialise the atmospheric transition Probabilities
C
         NAXIS(1) = NUMDLY**NANT
         NAXIS(2) = NAXIS(1)
         CALL DATMAKAR('ATMT', 2, NAXIS, 'R', ATADD)
         CALL IPCATMT(MEMR(ATADD), NUMDLY, NANT, TZERO, RZERO)
C
C Initialise the Track Probabilities
C
         NAXIS(1) = NFRINGES
         CALL DATMAKAR('TPROB', 1, NAXIS, 'R', TKADD)
         DO I = 0, NFRINGES - 1
            MEMR(TKADD + I) = - LOG(FLOAT(NFRINGES))
         END DO
C
C Allocate the TraceBack and working Arrays
C
         NAXIS(1) = NFRINGES
         NAXIS(2) = NFRAMES
         CALL DATMAKAR('TRACE', 2, NAXIS, 'I', TCADD)
         CALL DATMAKAR('WORKING', 1, NAXIS, 'R', WADD)
         IF (ERROR) GOTO 990
C
C Do for each Frame
C
         MESSAGE = 'Starting the HMM search'
         CALL MSGPUT(MESSAGE, 'I')
         DO FR = FRSTART, FREND
            IF (MOD(FR-FRSTART+1,NOTIFY).EQ.0) THEN
               WRITE(MESSAGE, '(A, I6, A, I6)') 
     $              'Doing Frame ', FR-FRSTART+1, 
     $              ' out of ', FREND-FRSTART + 1
               CALL MSGPUT(MESSAGE, 'I')
            END IF
C
C Do one step of the Vitterbi Algorithm
C
            CALL IPCVIT(MEMR(PRADD+NFRINGES*(FR-FRSTART)), 
     $           MEMR(TKADD), 
     $           MEMI(TCADD+(FR-FRSTART)*NFRINGES), 
     $           NFRINGES, MEMR(WADD), MEMR(ATADD))
C
C Next Frame
C
         END DO
C
C Recover most likely state track
C
         CALL IPCTBCK(MEMR(TKADD), MEMI(TCADD), MEMR(TRADD), 
     $        NFRINGES, NFRAMES, NANT, NUMDLY)
C
C Get the header done and save the file to disk
C
         CALL DATPUTI('TRACK', 'NUMDLY', NUMDLY, 1)
         CALL DATPUTR('TRACK', 'MINDLY', MINDLY, 1)
         CALL DATPUTR('TRACK', 'MAXDLY', MAXDLY, 1)
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
         CALL HISCOPY('PROB', 'TRACK')
         CALL HISPUT('TRACK', 'This is the HMM Delay TRACK file')
         CALL HISINPUT('TRACK')
C
C Display the Log(probability) of the 'best' track
C
         MAXPROB = MEMR(TKADD)
         DO I = 1, NFRINGES-1
            MAXPROB = MAX(MAXPROB, MEMR(TKADD+I))
         END DO
         WRITE(MESSAGE, '(A, E15.8)') 
     $        'Log(Prob) of the Best Track is', MAXPROB
         CALL MSGPUT(MESSAGE, 'I')
         CALL HISPUT('TRACK', MESSAGE)

C
C Finish doing the header
C
         CALL DATRENAM('PROB/ANT', 'TRACK/ANT')
         CALL HEDCOPY('PROB', 'TRACK')
         CALL CRDPUT
     $        ('TRACK', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL DATSETTP('TRACK', 'TRACK')
         CALL FILIMGPU('TRACK', TRFILE, ' ')
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
