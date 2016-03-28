C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsprob.f	1.4    7/7/93
C
      SUBROUTINE SDEMAIN
C
CD Determines the probability of a set for fringe patterns from IPCS data
C
C More extensive description with details etc.
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Oct 7 1992
C       Converted to real PDF's
C                               R.G. Marson     Dec 29 1992
C       Added calculation of the Prob[Measurement]
C                               R.G. Marson     June 18 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
C Parameter Definitions
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSPROB')

C
C Function definitions
C
      REAL                 DATFGETR, PIXLFACT
      INTEGER              DATADD, DATFGETI
      LOGICAL              DATEXIST
C
C Local Variables
C
      CHARACTER*(SYSMXNAM) IPCSFILE, MODELFILE, PDFFILE, MEASFILE
      CHARACTER*1          ATYPE
      REAL                 MAXPROB, MINPROB, SUMPROB, THISPROB, DIFF
      REAL                 MINDLY, MAXDLY, FTIME, EXPPROB, LNFRINGES
      INTEGER              NDUMMY, TOTPHOTONS
      INTEGER              FRSIZE(2), FR, NFRINGES, NFRAMES, FRINDEX
      INTEGER              NPHOTONS, I, FRSTART, FREND, NUMDLY
      INTEGER              NANT, NOTIFY
      INTEGER              PHADD, FRADD, XADD, YADD, PRADD, MADD
      INTEGER              ANTYADD, DIAMADD, IDLYADD, PMADD
      CHARACTER*8      CTYPE(SYSMXDIM)
      DOUBLE PRECISION RVAL(SYSMXDIM)
      REAL             RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      INTEGER          ANAX, NAXIS(SYSMXDIM)
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
      CALL MSGWELCO('I find the probability of MAPPIT data')
C
C Get Input Parameters
C
      CALL USRCTL
      CALL USRGETC('IPCS', IPCSFILE, 1, NDUMMY)
      CALL USRGETC('Fringe', MODELFILE, 1, NDUMMY)
      CALL USRGETC('PDF', PDFFILE, 1, NDUMMY)
      CALL USRGETI('Notify', NOTIFY, 1, NDUMMY)
      CALL USRGETL('Debug', SYSDEBUG, 1, NDUMMY)
C
C Get IPCS data
C
      CALL IPCSGET('IPCS', IPCSFILE, '*', ' ')
      IF (ERROR) GOTO 990
      CALL IPCSSORT('IPCS', 'IF')
      CALL DATGETAR('IPCS', ANAX, NAXIS, ATYPE, PHADD) 
      TOTPHOTONS = DATFGETI('IPCS', 'GCOUNT')
      FRSTART = DATFGETI('IPCS', 'FR_START')
      FREND = DATFGETI('IPCS', 'FR_END')
      NFRAMES =  FREND - FRSTART + 1
      FRADD = DATADD('IPCS/FRAME')
      XADD = DATADD('IPCS/XPIXEL')
      YADD = DATADD('IPCS/YPIXEL')
      IF (DATEXIST('IPCS/FTIME')) THEN
         FTIME = DATFGETR('IPCS', 'FTIME')
      ELSE
         FTIME = 6.E-3
         CALL MSGPUT('Assuming the Frame Period is 6 ms', 'W')
      END IF
      IF (ERROR) GOTO 990
C
C Print out info on the Data
C
      WRITE(MESSAGE, '(A, I6, A, F7.2, A)') 'Data has ', NFRAMES, 
     $     ' Frames with', FLOAT(TOTPHOTONS)/FLOAT(NFRAMES), 
     $     ' Photons/Frame'
      CALL MSGPUT(MESSAGE, 'I')
C
C Get Fringe Data (must be an SDE file created with ipcssim)
C
      CALL DATCREAT('MODEL')
      CALL DATREAD ('MODEL', MODELFILE)
      CALL DATCHKTP('MODEL', 'FRINGEMODEL')
      CALL DATGETAR('MODEL', ANAX, NAXIS, ATYPE, MADD) 
      IDLYADD = DATADD('MODEL/IDELAYS')
      FRSIZE(1) = NAXIS(1)
      FRSIZE(2) = NAXIS(2)
      NFRINGES = NAXIS(3)
      NANT = DATFGETI('MODEL/ANT', 'NANT')
      NUMDLY = DATFGETI('MODEL', 'NUMDLY')
      MINDLY = DATFGETR('MODEL', 'MINDLY')
      MAXDLY = DATFGETR('MODEL', 'MAXDLY')
      IF (ERROR) GOTO 990
C
C Convert fringes to LOG(Fringes)
C
      CALL ARRLOG('MODEL', -100., 'MODEL')
C
C Print out info on the Model
C
      WRITE(MESSAGE, '(A, I3, A, I3, A)') 'Model has ', 
     $     NANT, ' Holes with ', NUMDLY, ' Delays per hole'
      CALL MSGPUT( MESSAGE, 'I')
      WRITE(MESSAGE, '(A, F10.3, A, F10.3, A)')
     $     'Delay Range: ', MINDLY * 1E9, ' - ', 
     $     MAXDLY * 1E15, ' fs'
      CALL MSGPUT( MESSAGE, 'I')
      ANTYADD = DATADD('MODEL/ANT/ANTLOC')
      DIAMADD = DATADD('MODEL/ANT/ANTDIAM')
      DO I = 0, NANT - 1
         WRITE(MESSAGE, '(A, I3, A, 3PF10.2, A, 3PF10.2, A)')
     $        'Hole # ', I+1, ' Position: ', MEMR(ANTYADD + I),
     $        ' mm Diameter: ', MEMR(DIAMADD + I), ' mm'
         CALL MSGPUT( MESSAGE, 'I')
      END DO
C
C Clip the X and Y locations of photons so we do not have
C  to do this check in the inner loop
C
      CALL PIXRCLIP(MEMR(XADD), 0., FLOAT(FRSIZE(1)-1), 
     $     MEMR(XADD), NPHOTONS)
      CALL PIXRCLIP(MEMR(YADD), 0., FLOAT(FRSIZE(2)-1), 
     $     MEMR(YADD), NPHOTONS)
C
C Allocate space for the probabilities
C
      NAXIS(1) = NFRINGES
      NAXIS(2) = NFRAMES
      CALL DATMAKAR('PROB',2, NAXIS, 'R', PRADD)
C
C Do for each Frame
C
      DO FR = FRSTART, FREND
         IF (MOD(FR-FRSTART+1,NOTIFY).EQ.0) THEN
            WRITE(MESSAGE, '(A, I6, A, I6)') 
     $           'Doing Frame ', FR-FRSTART+1, 
     $           ' out of ', FREND-FRSTART + 1
            CALL MSGPUT(MESSAGE, 'I')
         END IF
C
C Extract current frame
C
         CALL IPCWIN(MEMR(FRADD), TOTPHOTONS, FR, FR+1, FRINDEX, 
     $        NPHOTONS)
C
C Find its probability
C
         FRINDEX = FRINDEX - 1
         CALL IPCPROB(MEMR(MADD), FRSIZE(1), FRSIZE(2), NFRINGES, 
     $        MEMR(PHADD+FRINDEX), MEMR(XADD+FRINDEX), 
     $        MEMR(YADD+FRINDEX),
     $        NPHOTONS, MEMR(PRADD+NFRINGES*(FR-FRSTART)))
C
C Next Frame
C
      END DO
C
C Calculate the prob measurement also to properly normalise these probabilities
C
C
C Get the output file allocated
C
      NAXIS(1) = NFRAMES
      CALL DATMAKAR('PMEAS', 1, NAXIS, 'R', PMADD)
C
C Do for each Frame
C
      LNFRINGES = LOG(REAL(NFRINGES))
      DO FR = FRSTART, FREND
C
C Just chunk thru on a frame by frame basis calculating the sum of all the
C  probabilities. The Probability of a measurement is:
C      P[M] = Sum{P[M|theta].P[theta]} Where we sum over theta, the phase
C                                      on each of the holes
C We Assume P[theta] is uniformly distributed and 
C P[M|theta] is given by N!.Prod{P[x|theta]}
C The number stored in the prob file is just Log{Prod{P[x|theta]}}
C As the actual P[M] would overflow the exponent on single precision numbers
C I calculate Log{P[M]} instead. To avoid overflow problems this is 
C calculated by realising that Log(a + b) = Log(a) + Log(1+b/a)
C                                         = Log(a) + b/a        iff b/a << 1
C                                         = A      + Exp(B-A)
C                                           where A = Log(a); B = Log(b)
C                                             and      a > b            
C Note: The numbers in this section assume IEEE single precision floating point
            
         SUMPROB = MEMR(PRADD+NFRINGES*(FR-FRSTART))
         IF ((SYSDEBUG).AND.(FR.EQ.FREND))
     $        PRINT *, SUMPROB
         DO I = 2, NFRINGES
            THISPROB = MEMR(PRADD+NFRINGES*(FR-FRSTART)+I-1)
            MAXPROB = MAX(SUMPROB, THISPROB)
            MINPROB = MIN(SUMPROB, THISPROB)
            DIFF = MINPROB - MAXPROB
            IF (DIFF.GT.-103.27) THEN
               EXPPROB = EXP(DIFF)
               IF (EXPPROB.LT.1.E-7) THEN
                  SUMPROB = MAXPROB + EXPPROB
               ELSE
                  SUMPROB = MAXPROB + LOG(1 + EXPPROB)
               END IF
            ELSE
               SUMPROB = MAXPROB
            END IF
            IF ((FR.EQ.FREND).AND.(I.GT.(NFRINGES-100))
     $           .AND.(SYSDEBUG))
     $           PRINT *, FR, I, SUMPROB, THISPROB, DIFF, EXPPROB
         END DO
         CALL IPCWIN(MEMR(FRADD), TOTPHOTONS, FR, FR+1, FRINDEX, 
     $        NPHOTONS)
         MEMR(PMADD+(FR-FRSTART)) = SUMPROB+LNFRINGES+
     $        PIXLFACT(NPHOTONS)
         IF (SYSDEBUG) PRINT *, FR, NPHOTONS, 
     $        MEMR(PMADD+(FR-FRSTART)), LNFRINGES, PIXLFACT(NPHOTONS)
C
C Normalise the Probabilities
C
         DO I = 0, NFRINGES - 1
            MEMR(PRADD + (FR-FRSTART)*NFRINGES + I) = 
     $           MEMR(PRADD + (FR-FRSTART)*NFRINGES + I) - 
     $           MEMR(PMADD + (FR-FRSTART)) 
     $           + PIXLFACT(NPHOTONS) + LNFRINGES
         END DO
C
C Next Frame
C
      END DO
C
C Check if we want to save the Prob[Meas] in a file
C
      CALL USRGETC('Pmeas', MEASFILE, 1, NDUMMY)
      IF (MEASFILE.NE.' ') THEN
C
C Get the header done and save the file to disk
C
         CALL CRDGET
     $        ('PMEAS', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CTYPE(1) = 'FRAME'
         RVAL(1) = 0.
         RPIX(1) = 1.
         DELT(1) = 1.
         ROTA(1) = 0.
         CALL HISCOPY('IPCS', 'PMEAS')
         CALL HISPUT('PMEAS','This is the Measured Probabilities file')
         CALL HISINPUT('PMEAS')
         CALL HEDCOPY('MODEL', 'PMEAS')
         CALL CRDPUT
     $        ('PMEAS', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
         CALL DATSETTP('PMEAS', 'IMAGE')
         CALL FILIMGPU('PMEAS', MEASFILE, ' ')
      END IF
C
C Save the Probabilities
C
C
C Fix up the history
C
      CALL HISCOPY('IPCS', 'PROB')
      CALL HISPUT('PROB', 'This is the Raw Probabilities file')
      CALL HISINPUT('PROB')
C
C Fix up the header
C
      CALL DATRENAM('MODEL/ANT', 'PROB/ANT')
      CALL HEDCOPY('MODEL', 'PROB')
      CALL DATPUTR('PROB', 'FTIME', FTIME, 1)
      CALL DATSETTP('PROB', 'PROBABILITIES')
C
C Fix up the Co-ordinates
C
      CALL CRDGET('PROB', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      CTYPE(1) = 'INDX'
      RVAL(1) = 1
      RPIX(1) = 1
      DELT(1) = 1
      ROTA(1) = 0
      CTYPE(2) = 'FRAME'
      RVAL(2) = 0
      RPIX(2) = 1
      DELT(2) = 1
      ROTA(2) = 0
      CALL CRDPUT('PROB', ANAX, CTYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
C
C Save the thing
C      
      CALL FILIMGPU('PROB', PDFFILE, ' ')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
