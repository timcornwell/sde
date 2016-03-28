C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)decalc.f	1.1 1/16/95
C
      SUBROUTINE SDEMAIN
C
CD Program to calculate delay errors
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 16 1995
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DECALC')
C
      INTEGER		NDUMMY, NCHAN, IANT, JANT, NSPEC
      REAL       	DERR(10), BW, SCANWT, ORESID, NRESID,
     $   		CORWT(10,10), ARG, BSERR, SGERR
      COMPLEX		CORRGAIN(10,10), ANTGAIN(10)
C==================================================================
C
      CALL MSGWELCO ('I calculate delay errors')
      CALL USRCTL
C
C Get Inputs
C
      CALL USRGETR ('Delays', DERR, 10, NDUMMY)
      CALL USRGETR ('Bandwidth', BW, 1, NDUMMY)
      CALL USRGETI ('Channels', NCHAN, 1, NDUMMY)
      CALL USRGETI ('SpecAvg', NSPEC, 1, NDUMMY)
C
      DO 10 IANT = 1, 10
         DO 20 JANT = 1, 10
            ARG = 2.0*3.14159*ABS(DERR(IANT) - DERR(JANT))*1E-9*
     $         BW/FLOAT(NCHAN)
            IF((NSPEC.NE.0).AND.(ARG.NE.0.0)) THEN
               BSERR = SIN(ARG)/ (NSPEC * SIN(ARG/NSPEC))
            ELSE
               BSERR = 1.0
            END IF
            SGERR = 1.0-ABS(DERR(IANT) - DERR(JANT))*1E-9*
     $         (2.0*BW)/FLOAT(NCHAN)
            CORRGAIN(IANT, JANT) = CMPLX(SGERR * BSERR, 0.0)
            CORWT(IANT, JANT) = 1.0
 20      CONTINUE
         CORWT(IANT,IANT)=0.0
         ANTGAIN(IANT) = CMPLX(1.0,0.0)
 10   CONTINUE
C
      CALL CALANTSO (CORRGAIN, CORWT, 10, ANTGAIN, 'AMPPHI', 
     1   ORESID, NRESID, SCANWT)
C
      DO 40 IANT = 1, 10
         DO 50 JANT = IANT+1, 10
            WRITE (MESSAGE, 1200) IANT, JANT,
     $         REAL(CORRGAIN(IANT,JANT)-CMPLX(1.0,0.0)),
     $         REAL(CORRGAIN(IANT, JANT)
     $         / (ABS(ANTGAIN(IANT))*ABS(ANTGAIN(JANT)))-
     $         CMPLX(1.0,0.0))
 1200       FORMAT ('Ants ',I2,',',I2,', Gain = ',F7.4,1X,F7.4)
            CALL MSGPUT (MESSAGE, 'I')
 50      CONTINUE
 40   CONTINUE
C
      WRITE (MESSAGE, 1000) ORESID, NRESID
 1000 FORMAT ('Residual before = ',1PE12.3,', after = ',
     $   1PE12.3)
      CALL MSGPUT (MESSAGE, 'I')
      DO 30 IANT = 1, 10
         WRITE (MESSAGE, 1100) IANT,  DERR(IANT),
     $      ABS(ANTGAIN(IANT))-1.0
 1100    FORMAT ('Antenna ',I2,', Delay error = ',F7.1,' ns,',
     $      ' Gain error = ',F7.4)
         CALL MSGPUT (MESSAGE, 'I')
 30   CONTINUE
C
 999  CONTINUE
      END
