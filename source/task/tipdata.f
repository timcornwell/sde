C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)tipdata.f	1.1	 3/26/93
C
      SUBROUTINE SDEMAIN
C
CD Program for viewing and reducing tipper stability data
C
C Audit trail:
C	Original version
C					M.A.Holdaway  22 Dec 1992
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPDATA')
C
C Go commands
C
      INTEGER		NPOSS
      PARAMETER		(NPOSS=22)
      CHARACTER*(SYSMXNAM)	GC
      CHARACTER*8	MNMAT, POSSGC(NPOSS)
C
C Variables
C
      REAL		THRESH, T, SKYWT, CALWT, GRAPHLIM(4)
      INTEGER		NDUMMY, STIME(2), CTIME
      CHARACTER*(SYSMXNAM)	SFILE, LFILE, DATAIN, DATAOUT, NEWTIP,
     $   			PLOTWHAT, DEV, COMMENT, COMSUB, DBNAME
      INTEGER		 	CALIBRTR, BINSUB
      REAL			BINLIST(10)
C
      LOGICAL			DATEXIST
      CHARACTER*(SYSMXNAM)	STRM2
C
C Possible go commands
C
      DATA	POSSGC	/'get', 'save', 'newtip', 'delete', 'status', 
     $  		'edit', '1plt', 'timplt', 'cumplt', '1list',
     $   		'timlist', 'cumlist', 'av', 'sf', 'calsf',
     $   		'calav', 'calac', 'help', 'delsub', 'comment', 
     $   		'bin', 'quit'/

C
C==================================================================
C
C Start of loop over go commands
C
      CALL MSGWELCO ('I wash the tipper stability data''s floor')
 1    CONTINUE
C
C Get input parameters
C
      CALL USRCTL
      CALL USRGETC('OldDataBase', DATAIN, 1, NDUMMY)
      CALL USRGETC('NewDataBase', DATAOUT, 1, NDUMMY)
      CALL USRGETC('DBname', DBNAME, 1, NDUMMY)
      CALL USRGETC('NewTip', NEWTIP, 1, NDUMMY)
      CALL USRGETC('Dev', DEV, 1, NDUMMY)
      CALL USRGETC('ListFile', LFILE, 1, NDUMMY)
      CALL USRGETC('StatFile', SFILE, 1, NDUMMY)
      CALL USRGETC('PlotWhat', PLOTWHAT, 1, NDUMMY)
      CALL USRGETR('TimeScale', T, 1, NDUMMY)
      CALL USRGETI('Calibrator', CALIBRTR, 1, NDUMMY)  ! 0, 1, or 2
      CALL USRGETI('Select', STIME, 2, NDUMMY)
      CALL USRGETI('CalSelect', CTIME, 1, NDUMMY)
      CALL USRGETR('SkyWt', SKYWT, 1, NDUMMY)
      CALL USRGETR('CalWt', CALWT, 1, NDUMMY)
      CALL USRGETR('Threshold', THRESH, 1, NDUMMY)
      CALL USRGETC('Comment', COMMENT, 1, NDUMMY)
      CALL USRGETC('Comsub', COMSUB, 1, NDUMMY)
      CALL USRGETR('GraphLim', GRAPHLIM, 4, NDUMMY)
      CALL USRGETI('BinSub', BINSUB, 1, NDUMMY)
      CALL USRGETR('BinList', BINLIST, 10, NDUMMY)
C
      IF (DBNAME .EQ. ' ') DBNAME = 'DataBase'
      IF(ERROR) GO TO 990
C
C Minmatch go command
C
      CALL USRGETGO (GC)
      CALL STRMNMAT (GC, POSSGC, NPOSS, MNMAT)
      IF(ERROR) GO TO 990
      IF (MNMAT.EQ.' ') THEN
         GC='help'
      ELSE
         GC = MNMAT
      END IF
C
C Go through all the things we can do
C
      IF (GC.EQ.'help') THEN
         CALL TIPHELP
C
C Get Tipper DataBase file
C
      ELSEIF (GC.EQ.'get') THEN
         IF (DATAIN.NE.' ') THEN
            IF(DATEXIST(DBNAME)) THEN
               CALL MSGPUT ('A DataBase already exists in memory', 'W')
               CALL MSGPUT ('Use "go delete" before you read another',
     $            'W')
               GOTO 1
            END IF
            CALL MSGPUT ('Getting database from '//DATAIN, 'I')
            CALL DATCREAT (DBNAME)
            CALL DATREAD (DBNAME, DATAIN)
         ELSE
            CALL MSGPUT ('Need to specify OldDataBase', 'E')
            GO TO 1
         END IF
C
C Save Tipper DataBase
C
      ELSEIF (GC.EQ.'save') THEN
         IF (DATAOUT.NE. ' ' ) THEN
            CALL DATWRITE (DBNAME, DATAOUT)
         ELSE
            CALL MSGPUT ('Need to specify NewDataBase', 'E')            
         ENDIF
C
C Add a new tipper subdirectory to DataBase
C
      ELSEIF (GC.EQ.'newtip') THEN
         IF (NEWTIP.NE.' ') THEN
            CALL TIPADD (DBNAME, NEWTIP)
         ELSE
            CALL MSGPUT ('Need to specify NewTip', 'E')            
         ENDIF
C
C Delete some tipper subdirectories in DataBase
C
      ELSE IF (GC .EQ. 'delsub') THEN
         CALL TIPDEL (DBNAME, STIME)
C
C Delete Current Tipper database
C
      ELSEIF (GC.EQ.'delete') THEN
         CALL DATDELET (DBNAME)
C
C Database Status
C
      ELSEIF (GC.EQ.'status') THEN
         CALL TIPSTAT (DBNAME, SFILE)
C
C Add a comment to DataBase
C
         ELSEIF (GC.EQ.'comment') THEN
            IF (COMSUB .EQ. ' ') THEN
               CALL TIPCOMME (DBNAME, COMMENT)
            ELSE
               CALL TIPCOMME (STRM2(DBNAME, COMSUB), COMMENT)
            ENDIF
C
C Edit individual tipper files for bad points
C
      ELSEIF (GC .EQ. 'edit') THEN
         CALL TIPEDIT (DBNAME, THRESH, STIME)
C
C Make 1 plot of RAW, AV, ASD, SF, CAV, CASD, or CSF
C
      ELSEIF (GC.EQ.'1plt') THEN
         CALL TIP1PLT (DBNAME, PLOTWHAT, STIME, CALIBRTR,
     $      GRAPHLIM, DEV, .FALSE.)
C
C Make 1 LIST of RAW, AV, ASD, SF, CAV, CASD, or CSF
C
      ELSEIF (GC.EQ.'1list') THEN
         CALL TIP1PLT (DBNAME, PLOTWHAT, STIME, CALIBRTR,
     $      GRAPHLIM, LFILE, .TRUE.)
C
C Plot something over a wide range of time 
C     (ie, ASD(56 s): (RAW, AV, ASD, SF, CAV, CASD, or CSF)
C
      ELSEIF (GC.EQ.'timplt') THEN
         CALL TIPTPLT (DBNAME, PLOTWHAT, T, STIME, CALIBRTR,
     $      GRAPHLIM, DEV, .FALSE.)
C
C LIST something over a wide range of time 
C     (ie, ASD(56 s): (RAW, AV, ASD, SF, CAV, CASD, or CSF)
C
      ELSEIF (GC.EQ.'timlist') THEN
         CALL TIPTPLT (DBNAME, PLOTWHAT, T, STIME, CALIBRTR,
     $      GRAPHLIM, LFILE, .TRUE.)
C
C Plot how frequently things are better than something
C  (RAW, AV, ASD, SF, CAV, CASD, or CSF)
C
      ELSEIF (GC.EQ.'cumplt') THEN
         CALL TIPCUMPL (DBNAME, PLOTWHAT, T, STIME, CALIBRTR,
     $      GRAPHLIM, DEV, .FALSE.)
C
C List how frequently things are better than something
C  (RAW, AV, ASD, SF, CAV, CASD, or CSF)
C
      ELSEIF (GC.EQ.'cumlist') THEN
         CALL TIPCUMPL (DBNAME, PLOTWHAT, T, STIME, CALIBRTR,
     $      GRAPHLIM, LFILE, .TRUE.)
C
C Calculate Structure Function and Allan Variance
C
      ELSEIF (GC.EQ.'sf' .OR. GC .EQ. 'av') THEN
         CALL TIPSFAV (DBNAME, STIME)
C
C Calibrate SF, SSF
C
      ELSEIF (GC.EQ.'calsf') THEN
         CALL TIPCALSF (DBNAME, STIME, CTIME, SKYWT, CALWT)
C
C Calibrate AV, ASD
C
      ELSEIF (GC.EQ.'calav') THEN
         CALL TIPCALAV (DBNAME, STIME, CTIME, SKYWT, CALWT)
C
C Accumulate Calibrator Scans
C
      ELSEIF (GC.EQ.'calac') THEN
         CALL TIPCALAC (DBNAME, BINSUB, STIME, BINLIST, 10,
     $      PLOTWHAT, T)
C
C BIN CASD or CSSF
C
      ELSEIF (GC.EQ.'bin') THEN
         CALL TIPBIN (DBNAME, BINSUB, STIME, CALIBRTR,
     $      BINLIST, 10, PLOTWHAT, T)
C
C Quit
C
      ELSEIF (GC.EQ.'quit') THEN
         GO TO 999
      ELSE
C
C Unknown!
C
         CALL MSGPUT ('Unknown command', 'W')
         CALL MSGPUT ('type "go help" for assistance', 'W')
      END IF
C
C Live dangerously
C
 980  CONTINUE
      IF(ERROR) THEN
         CALL MSGPUT ('Error found: continuing anyway', 'W')
         CALL ERRFLUSH
         CALL ERRCANCE
      END IF
      GO TO 1
C
 990  CONTINUE
      IF (ERROR) CALL ERRTRACE(ROUTINE)
C
 999  CONTINUE
      END
      SUBROUTINE TIPHELP
C
C Print help info for ant task
C
C
C
C Audit trail:
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'TIPHELP')
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL MSGPUT ('Possible commands:', 'H')
      CALL MSGPUT ('   get    * get existing DataBase (DB)', 'H')
      CALL MSGPUT ('            - OldDataBase', 'H')
      CALL MSGPUT ('   save   * save current DB', 'H') 
      CALL MSGPUT ('          * NewDataBase', 'H')
      CALL MSGPUT ('   newtip * add a TIPPER file to DB', 'H')
      CALL MSGPUT ('            - NewTip', 'H')
      CALL MSGPUT ('   delete * delete current DB', 'H')
      CALL MSGPUT ('   delsub * delete range of DB/SUB', 'H')
      CALL MSGPUT ('            - Select(1), Select(2) as TimeRange',
     $   'H')
      CALL MSGPUT ('   status * print what has been done to DB', 'H')
      CALL MSGPUT ('   commen * add comment to database', 'H')
      CALL MSGPUT ('   comsub   - subdirectory to add comment to', 'H')
      CALL MSGPUT ('   edit   * edit bad points from TIPPER files', 'H')
      CALL MSGPUT ('            - Threshold', 'H')
      CALL MSGPUT ('   av/sf  * calculate Allan Var, ASD', 'H')
      CALL MSGPUT ('            - Select(1), Select(2) as TimeRange',
     $   'H')
      CALL MSGPUT ('   calac  * accumulate and normalize calibrators',
     $   'H')
      CALL MSGPUT ('            - use Select, T, PlotWhat, BinList(1)',
     $   'H')
      CALL MSGPUT ('              to select cal scans, BinSub is','H')
      CALL MSGPUT ('              output subdirectory', 'H')
      CALL MSGPUT ('   calsf  * calibrate SF, SSF', 'H')
      CALL MSGPUT ('   calav  * calibrate AV, ASD', 'H')
      CALL MSGPUT ('            - Select(1), Select(2) as TimeRange',
     $   'H')
      CALL MSGPUT ('            - CalSelect to select calibration run',
     $   'H')
      CALL MSGPUT ('            - SkyWt and CalWt', 'H')
      CALL MSGPUT ('            - Select(1) = Date+Time ','H')
      CALL MSGPUT ('   1plt   * plot something ', 'H')
      CALL MSGPUT ('   1list  * list something ', 'H')
      CALL MSGPUT ('            - PlotWhat=(RAW,AV,ASD,SF,CAV,CASD,CSF)'
     $   , 'H')
      CALL MSGPUT ('            - Select(1), Select(2) as Timerange ',
     $   'H')
      CALL MSGPUT ('            - Calibrator: (0=sky, 1=cal, 2=both)',
     $   'H')
      CALL MSGPUT ('   timplt * plot some quntity over a range of time'
     $   , 'H')
      CALL MSGPUT ('   timlis * list some quntity over a range of time'
     $   , 'H')
      CALL MSGPUT ('            - PlotWhat=(AV,ASD,SF,CAV,CASD,CSF)'
     $   , 'H')
      CALL MSGPUT ('            - TimeScale=3 to 1800', 'H')
      CALL MSGPUT ('            - Select= Date+Time from start to end',
     $   'H')
      CALL MSGPUT ('            - Calibrator: (0=sky, 1=cal, 2=both)',
     $   'H')
      CALL MSGPUT ('   cumplt * make a cumulative plot', 'H')
      CALL MSGPUT ('   cumlis * make a cumulative list', 'H')
      CALL MSGPUT ('            - PlotWhat=(AV,ASD,SF,CAV,CASD,CSF)'
     $   , 'H')
      CALL MSGPUT ('            - TimeScale=3 to 1800', 'H')
      CALL MSGPUT ('            - Select= Date+Time from start to end',
     $   'H')
      CALL MSGPUT ('            - Calibrator: (0=sky, 1=cal, 2=both)',
     $   'H')
      CALL MSGPUT ('    bin   * bin the ASD and SSF into bins', 'H')
      CALL MSGPUT ('            - PlotWhat=(CASD, CSSF)', 'H')
      CALL MSGPUT ('            - TimeScale', 'H')
      CALL MSGPUT ('            - BinList', 'H')
      CALL MSGPUT ('   quit  * quit program', 'H')
C
  999 CONTINUE
      END
