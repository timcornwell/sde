C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)decal.f	1.1 22 Feb 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to estimate the effect of delay errors on visibility data 
C
C Audit trail:
C	New task
C				T.J. Cornwell     Jan 17 1995
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'DEFIX')
C
      CHARACTER*(SYSMXNAM)	VISFILE, MODFILE, NVISFILE
      REAL		DERR(10), BW, A2, CHISQ, SUMWT, SUMWTS, VBEFORE,
     $   		VAFTER, AVGDE
      COMPLEX		CORRGAIN(10,10)
      INTEGER		NDUMMY, NCHAN, IANT, JANT
      INTEGER           NAX, NAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL		RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      CHARACTER*8       TYPE(SYSMXDIM)
C==================================================================
      CALL MSGWELCO ('I fit empirical decorrelation to VLBA data')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETR ('Delays', DERR, 10, NDUMMY)
      CALL USRGETR ('Bandwidth', BW, 1, NDUMMY)
      CALL USRGETI ('Channels', NCHAN, 1, NDUMMY)
C
C Scale delays so that the coefficient for Hanning weighting 
C should be 6.39 (for 10th order polynomial fits or 7.07 (for first 
C term of the expansion of a fitted Gaussian)
C
      AVGDE = 0.0
      DO 10 IANT = 1, 10
         DERR(IANT) = DERR(IANT)*1E-9*(2.0*BW)/FLOAT(NCHAN)
         AVGDE = AVGDE + DERR(IANT)/10.0
 10   CONTINUE
      DO 11 IANT = 1, 10
         DERR(IANT) = DERR(IANT) - AVGDE 
 11   CONTINUE
C
C Read visibility data
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'IV', '*', ' ')
      IF (ERROR) GO TO 999
C
C Calculate model
C
      CALL DATCREAT ('Vis/MOD')
      CALL DATCREAT ('Vis/MOD/I')
      CALL CRDGET ('Vis/OBS/I', NAX, TYPE, NAXIS, RVAL,
     $   RPIX, DELT, ROTA)
      CALL CRDPUT   ('Vis/MOD/I', NAX, TYPE, NAXIS, RVAL, 
     1   RPIX, DELT, ROTA)
      CALL ARRCOPY ('Vis/OBS/I/VIS', 'Vis/MOD/I/VIS')
      CALL ARRCOPY ('Vis/OBS/I/WT',  'Vis/MOD/I/WT')
      CALL USRGETC ('Model', MODFILE, 1, NDUMMY)
      CALL FILIMGGE ('Model', MODFILE, ' ')
      CALL IMGTOVIS ('Vis', 'MOD/I', 'Model', 'ModVis', 'DModel')
C
C Calculate delay error coefficient
C
      CALL USRGETR ('A2', A2, 1, NDUMMY)
      IF(A2.EQ.0.0) THEN
         CALL VISDECAL ('Vis', 'OBS/I', 'MOD/I', 10, DERR, A2)
         IF (ERROR) GO TO 999
         WRITE (MESSAGE, 1000) A2
 1000    FORMAT ('Estimated A2 = ',1PE12.3)
          CALL MSGPUT (MESSAGE, 'I')
      ELSE
         WRITE (MESSAGE, 1050) A2
 1050    FORMAT ('Input A2 = ',1PE12.3)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
C Calculate closure errors
C
      DO 110 IANT = 1, 10
         DO 120 JANT = 1, 10
            CORRGAIN(IANT, JANT) =
     $         CMPLX(1.0+2.0*A2*DERR(IANT)*DERR(JANT), 0.0)
 120     CONTINUE
 110  CONTINUE
C
C Apply corrections
C
      CALL VISCHISQ ('Vis', 'OBS/I', 'MOD/I', CHISQ, SUMWT, SUMWTS,
     $   NDUMMY)
      IF (ERROR) GO TO 999
      VBEFORE=SQRT(CHISQ/(2.0*SUMWT))
      WRITE (MESSAGE, 1100) VBEFORE
 1100 FORMAT ('Before Correction, visibility fit is ',1PE12.3,
     $   ' Jy for unit weight')
      CALL MSGPUT (MESSAGE, 'I')
      CALL VISBLCOR ('Vis', 'OBS/I', 'OBS/I', 10, CORRGAIN)
      CALL VISCHISQ ('Vis', 'OBS/I', 'MOD/I', CHISQ, SUMWT, SUMWTS,
     $   NDUMMY)
      IF (ERROR) GO TO 999
      VAFTER=SQRT(CHISQ/(2.0*SUMWT))
      WRITE (MESSAGE, 1200) VAFTER
 1200 FORMAT ('After Correction, visibility fit is  ',1PE12.3,
     $   ' Jy for unit weight')
      CALL MSGPUT (MESSAGE, 'I')
      CALL VISBLCOR ('Vis', 'OBS/V', 'OBS/V', 10, CORRGAIN)
C
C Write visibility data
C
      CALL USRGETC ('NewVis', NVISFILE, 1, NDUMMY)
      IF (NVISFILE.NE.' ') THEN
         CALL HISINPUT ('Vis')
         CALL VISPUT ('Vis', NVISFILE, 'OBS', 'IV', '*', ' ')
      END IF
C
C
 999  CONTINUE
      END
