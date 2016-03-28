C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)gfit.f	1.1    7/13/94
C
      SUBROUTINE SDEMAIN
C
CD General purpose interface to the external program GaussFit
C
CModes:  Use as "GO <mode>  It is recorded in the curfile as 'GOCMD'
C
CFitting modes:
C GO LFIT	(Linear fit)
C GO RFIT	(Radius & linear fit)
C GO GFIT	(Global fit for all parameters)
C GO FFIT	(LFIT, RFIT, FIT in succession)
C
CTranslation modes:
C GO >SAOMODEL  (from SDE MODEL)
C GO >PARAMS    (from SDE MODEL)
C GO SAOMODEL>  (to SDE MODEL)
C GO PARAMS>    (to SDE MODEL)
C GO >EXCEL     (from SDE IMAGE)
C GO EXCEL>     (to SDE IMAGE)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Apr 12 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GFIT')
C
      CHARACTER*(SYSMXNAM)	GOCMD, IMAGE, GMODEL, EXLFILE, GAUSSFIT,
     $   		ENVFILE, BOXFILE, RESULTS, MODEL, PARAMS,
     $   		SMODEL, FIT, RESIDUAL
      CHARACTER		COMMAND*80, CURFIT*2
      LOGICAL		MINSUM, IRLS, ORM, DOUBLE, TRIANG,
     $   		PRMAT, PRVAR
      INTEGER		NDUMMY, L, ITERS, ITER(3), ISTAT
      REAL		FAIR, HUBER, TUKEY, LAMBDA, FACTOR, TOL
C
#define TMPEXLDA 'TempExcelData'
#define TMPRESLT 'TempResult'
#define TMPENV 'TempEnvironment'
#define TMPMODEL 'TempModel'
#define TMPPARAM 'TempParam'
C
      INTEGER		SYSSYSTM, STRLEN
      LOGICAL		FILEXIST, DATEXIST
      CHARACTER		STRINT*5
C=======================================================================
      CALL MSGWELCO ('I fit gaussians to an image')
 1    CALL USRCTL
C
C Get input parameters
C
      CALL USRGETC ('Image', IMAGE, 1, NDUMMY)
      CALL USRGETC ('Fit', FIT, 1, NDUMMY)
      CALL USRGETC ('Residual', RESIDUAL, 1, NDUMMY)
      CALL USRGETC ('Box', BOXFILE, 1, NDUMMY)
      CALL USRGETC ('Model', MODEL, 1, NDUMMY)
      CALL USRGETC ('Params', PARAMS, 1, NDUMMY)
      CALL USRGETC ('SModel', SMODEL, 1, NDUMMY)
      CALL USRGETC ('GModel', GMODEL, 1, NDUMMY)
      CALL USRGETC ('Excel', EXLFILE, 1, NDUMMY)
      CALL USRGETC ('Env', ENVFILE, 1, NDUMMY)
      CALL USRGETC ('Results', RESULTS, 1, NDUMMY)
      CALL USRGETR ('Fair', FAIR, 1, NDUMMY)
      CALL USRGETR ('Huber', HUBER, 1, NDUMMY)
      CALL USRGETR ('Tukey', TUKEY, 1, NDUMMY)
      CALL USRGETL ('Minsum', MINSUM, 1, NDUMMY)
      CALL USRGETL ('IRLS', IRLS, 1, NDUMMY)
      CALL USRGETL ('ORM', ORM, 1, NDUMMY)
      CALL USRGETL ('Double', DOUBLE, 1, NDUMMY)
      CALL USRGETL ('Triang', TRIANG, 1, NDUMMY)
      CALL USRGETR ('Lambda', LAMBDA, 1, NDUMMY)
      CALL USRGETR ('Factor', FACTOR, 1, NDUMMY)
      CALL USRGETI ('Iters', ITER, 3, NDUMMY)
      CALL USRGETR ('Tol', TOL, 1, NDUMMY)
      CALL USRGETL ('Prmat', PRMAT, 1, NDUMMY)
      CALL USRGETL ('Prvar', PRVAR, 1, NDUMMY)
      CALL USRGETGO (STRBUF)
      CALL STRUC (STRBUF(1:LEN(GOCMD)), GOCMD)
      IF (ERROR) GO TO 999
C
      IF (GOCMD.EQ.' ') THEN
         CALL MSGPUT ('You must provide a GO command!','E')
         CALL MSGPUT ('Type ''help'' for more details.','E')
         GO TO 1
      END IF
C
      IF (MODEL.EQ.'*') MODEL = TMPMODEL
      IF (PARAMS.EQ.'*') PARAMS = TMPPARAM
C
      L = STRLEN(GOCMD)
      IF ((GOCMD(1:1).EQ.'>').OR.(GOCMD(L:L).EQ.'>')) THEN
         CALL TRANSL8 (GOCMD, MODEL, PARAMS, SMODEL, IMAGE, BOXFILE,
     $   EXLFILE)
         GO TO 1
      END IF
C
C Fitting functions begin here  (do we recognize it?)
C
      IF ((GOCMD(1:2).NE.'LF').AND.(GOCMD(1:2).NE.'RF').AND.
     $    (GOCMD(1:2).NE.'GF').AND.(GOCMD(1:2).NE.'FF')) THEN
         CALL MSGPUT ('Error: Unrecognized GO command', 'E')
         GO TO 1
      END IF
C
C Translate to PARMS file if needed
C
      IF (PARAMS.EQ.TMPPARAM) CALL TRANSL8
     $   ('>PARAMS', MODEL, PARAMS, SMODEL, IMAGE, BOXFILE, EXLFILE)
C
C First default the input parameters
C
      ITERS = ITER(1)
      IF (EXLFILE.EQ.' ') EXLFILE = TMPEXLDA
      IF (RESULTS.EQ.' ') RESULTS = TMPRESLT
      IF (ENVFILE.EQ.' ') ENVFILE = TMPENV
C
C Create the environment
C
      CALL DATCREAT ('Env')
      CALL DATPUTC ('Env', 'RESULTS', RESULTS, 1)
      CALL DATPUTC ('Env', 'PARAMS', PARAMS, 1)
      CALL DATPUTC ('Env', 'DATA1', EXLFILE, 1)
      STRBUF = EXLFILE
      CALL STRAPPEN (STRBUF,'.CRD')
      CALL DATPUTC ('Env', 'PARAM1', STRBUF(1:SYSMXNAM), 1)
      CALL DATPUTR ('Env', 'FAIR', FAIR, 1)
      CALL DATPUTR ('Env', 'HUBER', HUBER, 1)
      CALL DATPUTR ('Env', 'TUKEY', TUKEY, 1)
      CALL DATPUTL ('Env', 'MINSUM', MINSUM, 1)
      CALL DATPUTL ('Env', 'IRLS', IRLS, 1)
      CALL DATPUTL ('Env', 'ORM', ORM, 1)
      CALL DATPUTL ('Env', 'DOUBLE', DOUBLE, 1)
      CALL DATPUTL ('Env', 'TRIANG', TRIANG, 1)
      CALL DATPUTR ('Env', 'LAMBDA', LAMBDA, 1)
      CALL DATPUTR ('Env', 'FACTOR', FACTOR, 1)
      CALL DATPUTI ('Env', 'ITERS', ITERS, 1)
      CALL DATPUTR ('Env', 'TOL', TOL, 1)
      CALL DATPUTL ('Env', 'PRMAT', PRMAT, 1)
      CALL DATPUTL ('Env', 'PRVAR', PRVAR, 1)
C
C Find the program 'GaussFit'
C
      STRBUF = 'gaussfit'
      IF (FILEXIST(STRBUF)) THEN
         GAUSSFIT = STRBUF
      ELSE
         CALL SYSTRANS('SDEBIN',STRBUF)
         CALL STRAPPEN(STRBUF,'/gaussfit')
         IF (FILEXIST(STRBUF)) THEN
            GAUSSFIT = STRBUF
         ELSE
            CALL ERRREPOR (ERRFATAL, ROUTINE,
     $         'Cannot find gaussfit executable')
            GO TO 999
         END IF
      END IF
C
C Loopback for multiple fits
C
      CURFIT = GOCMD
      IF (GOCMD(1:2).EQ.'FF') CURFIT = 'LF'
 100  CONTINUE
C
C Check a few things
C
      IF (PARAMS.EQ.' ') THEN
         CALL MSGPUT ('Error: PARAMS cannot be null.','E')
         CALL MSGPUT (
     $      'Use GO >PARAMS and/or GO SAOMODEL> for initial model','I')
         GO TO 1
      END IF
C
C Write out environment file
C
      CALL DATPUTI ('Env', 'ITERS', ITERS, 1)
      CALL FILENVPU ('Env', ENVFILE)
      IF (ERROR) GO TO 999
C
C Find the appropriate model file
C
      IF (GMODEL.EQ.' ') THEN
         IF (CURFIT.EQ.'LF') GMODEL = 'GModel1.gf'
         IF (CURFIT.EQ.'RF') GMODEL = 'GModel2.gf'
         IF (CURFIT.EQ.'GF') GMODEL = 'GModel0.gf'
         IF (GMODEL.EQ.' ') THEN
            CALL MSGPUT ('Error: Can''t select appropriate model','E')
            GO TO 1
         END IF
      END IF
      IF (.NOT.FILEXIST(GMODEL)) THEN
         CALL SYSTRANS('SDEROOT',STRBUF)
         CALL STRAPPEN(STRBUF,'/source/mod/')
         CALL STRAPPEN(STRBUF,GMODEL)
         IF (FILEXIST(STRBUF)) THEN
            GMODEL = STRBUF
         ELSE
            STRBUF = './'
            CALL STRAPPEN (STRBUF, GMODEL)
            IF (FILEXIST(STRBUF)) THEN
               GMODEL = STRBUF
            ELSE
               MESSAGE = 'Error: Cannot find generic model file '
     $            // GMODEL(1:STRLEN(GMODEL))
               CALL MSGPUT (MESSAGE,'E')
               GO TO 1
            END IF
         END IF
      END IF
C
C Write out the Excel data file, if needed
C
      IF (IMAGE.NE.' ') THEN
         CALL FILIMGGE ('Image', IMAGE, ' ')
         CALL IMG2JY ('Image', 'Image')
         CALL FILMASGE ('Box', BOXFILE, 'Image')
         CALL FILEXLPU ('Image', EXLFILE, 'Box')
         CALL DATDELET ('Image')
         CALL DATDELET ('Box')
      ELSE
         IF (.NOT.FILEXIST(EXLFILE)) THEN
            MESSAGE = 'Error: Both Image and Excel cannot be blank'
            CALL MSGPUT (MESSAGE,'E')
            GO TO 1
         END IF
      END IF
C
C Construct command line
C
      COMMAND = GAUSSFIT(1:STRLEN(GAUSSFIT)+1) //
     $   	GMODEL(1:STRLEN(GMODEL)+1) //
     $   	ENVFILE(1:STRLEN(ENVFILE))
C
C Handle log file, if needed.  Use either sh or ksh here.
C
      CALL SYSGTENV('SDELOG', STRBUF)
      IF (STRBUF.NE.' ') THEN
         MESSAGE = COMMAND
         COMMAND = MESSAGE(1:STRLEN(MESSAGE)) // ' 2>&1 | tee -a ' //
     $      STRBUF(1:STRLEN(STRBUF))
      END IF
C
C Do the shell out
C
c      PRINT *, 'SIMULATING SHELL COMMAND'
c      PRINT *, COMMAND
c      READ (*,'(a)') message
      ISTAT = SYSSYSTM (COMMAND)
      MESSAGE = 'Status returned is ' // STRINT(ISTAT)
      CALL MSGPUT (MESSAGE, 'I')
C
C Construct fitted image and residual if needed
C
      IF ((FIT.NE.' ').OR.(RESIDUAL.NE.' ')) THEN
         CALL MSGPUT ('Calculating fitted model image','I')
         IF (IMAGE.NE.' ') THEN
            CALL FILIMGGE ('Fit', IMAGE, ' ')
            CALL IMG2JY ('Image', 'Image')
         ELSE
            CALL FILEXLGE ('Fit', EXLFILE, .FALSE.)
         END IF
         CALL ARRSETCO ('Fit', 0.0, 0.0)
         CALL FILGFMGE ('Model', PARAMS)
         CALL MODIMG ('Model','Fit')
         IF (FIT.NE.' ') THEN
            CALL FILDEL (FIT)
            CALL FILIMGPU ('Fit', FIT, ' ')
         END IF
      END IF
C
      IF (RESIDUAL.NE.' ') THEN
         IF (IMAGE.EQ.' ') THEN
            MESSAGE = 'Must have initial image to calculate residual'
            CALL MSGPUT (MESSAGE,'E')
         ELSE
            CALL FILIMGGE ('Image', IMAGE, ' ')
            CALL IMG2JY ('Image', 'Image')
            CALL IMGCLONE ('Image', 'Resid')
            CALL ARRSUBTR ('Image', 'Fit', 'Resid')
            CALL FILDEL (RESIDUAL)
            CALL FILIMGPU ('Resid', RESIDUAL, ' ')
         END IF
      END IF
C
C Translate back to model file, if desired
C
      IF (PARAMS.EQ.TMPPARAM) CALL TRANSL8
     $   ('PARAMS>', MODEL, PARAMS, SMODEL, IMAGE, BOXFILE, EXLFILE)
C
C Clean up temporary files and arrays
C
      IF (DATEXIST('Image')) CALL DATDELET('Image')
      IF (DATEXIST('Fit')) CALL DATDELET('Fit')
      IF (DATEXIST('Resid')) CALL DATDELET('Resid')
      IF (DATEXIST('Model')) CALL DATDELET('Model')
      IF (EXLFILE.EQ.TMPEXLDA) CALL FILDEL(EXLFILE)
      IF (RESULTS.EQ.TMPRESLT) CALL FILDEL(RESULTS)
      IF (ENVFILE.EQ.TMPENV) CALL FILDEL(ENVFILE)
      IF (PARAMS.EQ.TMPPARAM) CALL FILDEL(PARAMS)
C
C Loopback, to input menu or next fitting stage as needed
C
      IF ((GOCMD(1:2).EQ.'LF').OR.(GOCMD.EQ.'RF')) GO TO 1
      IF (GOCMD(1:2).EQ.'FF') THEN
         IF (CURFIT.EQ.'LF') THEN
            CURFIT = 'RF'
            ITERS = ITER(2)
            GO TO 100
         ELSE IF (CURFIT.EQ.'RF') THEN
            CURFIT = 'GF'
            ITERS = ITER(3)
            GO TO 100
         END IF
      END IF
C
C Can jump to here if an error found
C
  999 CONTINUE
      END
C
      SUBROUTINE TRANSL8 (TRANS, MODEL, PARAMS, SMODEL, IMAGE, BOXFILE,
     $   EXLFILE)
C
CD Deal with all the translation subfunctions
C
CTranslation modes:
C GO >SAOMODEL  (from SDE MODEL)
C GO >PARAMS    (from SDE MODEL)
C GO SAOMODEL>  (to SDE MODEL)
C GO PARAMS>    (to SDE MODEL)
C GO >EXCEL     (from SDE IMAGE)
C GO EXCEL>     (to SDE IMAGE)
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				D.S.Briggs	Apr 12 1992
C-----------------------------------------------------------------------
#include		"stdinc.h"
C
      CHARACTER*(SYSMXNAM)	TRANS, IMAGE, EXLFILE, BOXFILE, MODEL,
     $  			SMODEL, PARAMS
C
      INTEGER			L
      LOGICAL			DOFORWRD
      CHARACTER*(SYSMXNAM)	GOCMD
C
      INTEGER			STRLEN
C=======================================================================
      GOCMD = TRANS
      IF (GOCMD(1:1).EQ.'>') THEN
         DOFORWRD = .TRUE.
         STRBUF = GOCMD
         GOCMD = STRBUF(2:)
      END IF
      L = STRLEN(GOCMD)
      IF (GOCMD(L:L).EQ.'>') THEN
         DOFORWRD = .FALSE.
         STRBUF = GOCMD
         GOCMD = STRBUF(1:L-1)
      END IF
C
      IF (GOCMD(1:1).EQ.'S') THEN
         IF (DOFORWRD) THEN
            CALL MSGPUT (
     $         'Translating from SDE model to SAOImage model','I')
            CALL FILGETMO ('Model', MODEL)
            CALL MODLIST ('Model')
            CALL FILSMOPU ('Model', SMODEL, IMAGE)
            CALL DATDELET ('Model')
         ELSE
            CALL MSGPUT (
     $         'Translating from SAOImage model to SDE model','I')
            CALL FILSMOGE ('Model', SMODEL, IMAGE)
            CALL MODLIST ('Model')
            CALL FILDEL (MODEL)
            CALL FILPUTMO ('Model', MODEL)
            CALL DATDELET ('Model')
         END IF
      ELSE IF (GOCMD(1:1).EQ.'P') THEN
         IF (DOFORWRD) THEN
            CALL MSGPUT (
     $         'Translating from SDE model to Gaussfit params','I')
            CALL FILGETMO ('Model', MODEL)
            CALL MODLIST ('Model')
            CALL FILGFMPU ('Model', PARAMS)
            CALL DATDELET ('Model')
            IF (MODEL.EQ.TMPMODEL) CALL FILDEL (MODEL)
         ELSE
            CALL MSGPUT (
     $         'Translating from Gaussfit params to SDE model','I')
            CALL FILGFMGE ('Model', PARAMS)
            CALL MODLIST ('Model')
            CALL FILDEL (MODEL)
            CALL FILPUTMO ('Model', MODEL)
            CALL DATDELET ('Model')
         END IF
      ELSE IF (GOCMD(1:1).EQ.'E') THEN
         IF (DOFORWRD) THEN
            CALL MSGPUT (
     $         'Translating from SDE image to Excel file','I')
            CALL FILIMGGE ('Image', IMAGE, ' ')
            CALL IMG2JY ('Image', 'Image')
            CALL FILMASGE ('Box', BOXFILE, 'Image')
            CALL FILEXLPU ('Image', EXLFILE, 'Box')
            CALL DATDELET ('Image')
            CALL DATDELET ('Box')
         ELSE
            CALL MSGPUT (
     $         'Translating from Excel file to SDE image','I')
            IF ((PARAMS.NE.' ').AND.(MODEL.NE.' ')) THEN
               CALL MSGPUT (
     $            'Cannot get pixels from both PARAMS and MODEL','E')
               GO TO 999
            END IF
            IF ((PARAMS.EQ.' ').AND.(MODEL.EQ.' ')) THEN
               CALL MSGPUT ('Getting pixels from Excel file','I')
               CALL FILEXLGE ('Image', EXLFILE, .FALSE.)
            ELSE
               CALL FILEXLGE ('Image', EXLFILE, .TRUE.)
               IF (MODEL.NE.' ') THEN
                  CALL MSGPUT ('Getting pixels from model file','I')
                  CALL FILGETMO ('Model', MODEL)
               ELSE
                  CALL MSGPUT ('Getting pixels from params file','I')
                  CALL FILGFMGE ('Model', PARAMS)
               END IF
               CALL MODIMG ('Model', 'Image')
               CALL DATDELET ('Model')
            END IF
            CALL FILDEL (IMAGE)
            CALL FILIMGPU ('Image', IMAGE, ' ')
            CALL DATDELET ('Image')
         END IF
      ELSE
         CALL MSGPUT ('Unrecognized translation function','E')
         CALL MSGPUT ('Type ''help'' for list of commands','E')
      END IF
C
 999  CONTINUE
      RETURN
      END
