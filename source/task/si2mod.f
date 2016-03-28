C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)si2mod.f	1.4    12/4/90
C
      SUBROUTINE SDEMAIN
C
CD Program to convert a spectral index model to a normal SDE one
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.G. Marson	Nov 5 1990
C       Modified to produce an alpha model an I-alpha model as well
C       as an I model
C                               R.G. Marson     Dec 5 1990
C
C-----------------------------------------------------------------------
C
C The standard include must be present
C
#include	"stdinc.h"
C
C Declare name of routine
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SI2MOD')
C
C Function Declerations
C
      INTEGER STRLEN
C
C Local variables
C
      CHARACTER*(SYSMXNAM) 	INPMODEL, OUTMODEL(SYSMXIMG), IALPHA
      REAL              RFREQ, FREQ(SYSMXIMG)
      INTEGER		NDUMMY, I, NMOD
C=======================================================================
C
C Write welcome message
C
      CALL MSGWELCO ('I convert SI models to SDE ones')
C
C Call user interface routine, and get all parameters
C
      CALL USRCTL
      CALL USRGETC ('SImodel', INPMODEL, 1, NDUMMY)
      CALL USRGETC ('Model', OUTMODEL, SYSMXIMG, NDUMMY)
      CALL USRGETR ('RFreq', RFREQ, 1, NDUMMY)
      CALL USRGETR ('Freq', FREQ, SYSMXIMG, NDUMMY)
      CALL USRGETC ('MType', IALPHA, 1, NDUMMY)
      CALL USRGETL ('Debug', SYSDEBUG, 1, NDUMMY)
C
C Now get the input model
C
      CALL FILGETMS ('INMODEL', INPMODEL)
C
C Check that we have a valid type
C
      CALL STRUC(IALPHA, IALPHA)
      IF (IALPHA(1:2).EQ.'IA') THEN
         IALPHA = 'IA'
         MESSAGE = 'Creating a Intensity*Spectral Index model'
      ELSE IF (IALPHA(1:1).EQ.'I') THEN
         IALPHA = 'I '
         MESSAGE = 'Creating a Intensity model'
      ELSE IF (IALPHA(1:1).EQ.'A') THEN
         IALPHA = 'A '
         MESSAGE = 'Creating a Spectral Index model'
      ELSE
         MESSAGE = 'Model type '//IALPHA(1:STRLEN(IALPHA))//
     $        ' Not allowed'
         CALL ERRREPOR(ERRBDARG, ROUTINE, MESSAGE)
         GOTO 999
      END IF
      CALL MSGPUT(MESSAGE, 'I')
C
C Find out how many frequencies
C
      NMOD = SYSMXIMG
      DO I = 1, SYSMXIMG
         IF ((OUTMODEL(I).EQ.' ').OR.(FREQ(I).EQ.0.0)) THEN
            NMOD = MIN (I - 1, NMOD)
         END IF
      END DO
      IF (NMOD.EQ.0) THEN
         CALL ERRREPOR(ERRBDARG, ROUTINE, 
     $        'No output model name/freqency specified')
         GOTO 999
      END IF
C
C Loop over each frequency
C
      DO I = 1, NMOD
C
C Convert to the required frequency and format
C
         CALL MODSI2ST('INMODEL', 'OUTMODEL', RFREQ, FREQ(I), IALPHA)
C
C Output the model
C
         CALL FILPUTMO('OUTMODEL', OUTMODEL(I))
         CALL DATDELET('OUTMODEL')
      END DO
C
 999  CONTINUE
      END
