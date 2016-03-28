C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)defix.f	1.2 24 Jan 1995
C
      SUBROUTINE SDEMAIN
C
CD Program to correct visibility data for delay errors
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
      CHARACTER*(SYSMXNAM)	VISFILE, NVISFILE, MTYPE
      REAL		DERR(10), BW, PAR(3), SGERR, X
      INTEGER		NDUMMY, NCHAN, IANT, JANT, STRLEN
      COMPLEX		CORRGAIN(10,10)
C==================================================================
      CALL MSGWELCO ('I correct VLBA data for closure errors')
      CALL USRCTL
C
C Get input parameters
C
 1    CALL USRGETR ('Delays', DERR, 10, NDUMMY)
      CALL USRGETR ('Bandwidth', BW, 1, NDUMMY)
      CALL USRGETI ('Channels', NCHAN, 1, NDUMMY)
      CALL USRGETR ('Parameters', PAR, 3, NDUMMY)
      CALL USRGETC ('Type', MTYPE, 1, NDUMMY)
C
C Calculate Baseline errors
C
      DO 10 IANT = 1, 10
         DO 20 JANT = 1, 10
            X = ABS(DERR(IANT) - DERR(JANT))*1E-9*(2.0*BW)/
     $         FLOAT(NCHAN)
            IF(MTYPE.EQ.'GAUSS') THEN
               SGERR = EXP(-PAR(2)*X**2)
            ELSE
               SGERR = 1.0-PAR(1)*X-PAR(2)*X**2-PAR(3)*X**3
            END IF
            CORRGAIN(IANT, JANT) = CMPLX(SGERR, 0.0)
 20      CONTINUE
 10   CONTINUE
C
C Output values
C
      MESSAGE = 'Antenna   Delay'
      DO 35 JANT = 1, 10
         WRITE (MESSAGE(STRLEN(MESSAGE)+3:), '(3X,I2)') JANT
 35   CONTINUE
      CALL MSGPUT (MESSAGE, 'I')
      DO 40 IANT = 1, 10
         WRITE (MESSAGE,'(3X,I2,4X,F6.1)') IANT, DERR(IANT)
         DO 50 JANT = 1, 10
            WRITE (MESSAGE(STRLEN(MESSAGE)+1:), '(1X,F6.4)')
     $         REAL(CORRGAIN(IANT,JANT)-CMPLX(1.0,0.0))
 50      CONTINUE
         CALL MSGPUT (MESSAGE, 'I')
 40   CONTINUE
C
C Read visibility data
C
      CALL USRGETC ('Vis', VISFILE, 1, NDUMMY)
      CALL VISGET ('Vis', VISFILE, 'IV', '*', ' ')
      IF (ERROR) GO TO 999
C
C Correct visibility data for errors
C
      CALL VISBLCOR ('Vis', 'OBS/I', 'OBS/I', 10, CORRGAIN)
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
 999  CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)defix.f	1.2 24 Jan 1995
C
       SUBROUTINE VISBLCOR (NAME, SUB, CAL, NANT, CORGAIN)
C
CD Calibrate data
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	SUB	CH*(*)	input	Name of sub-class e.g. OBS/I
C	FLUX	REAL	input	Estimated flux
C	CAL	CH*(*)	input	Name of calibrated data
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
       CHARACTER*(*)	NAME, SUB, CAL
       INTEGER		NANT
       COMPLEX		CORGAIN(NANT, *)
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISBLCOR')
C
      INTEGER		NAX, NAXIS(SYSMXDIM), DATADD,
     1			BADD, WTADD, VSADD, VSNADD, WTNADD, NFLAG
      CHARACTER*1	ATYPE
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
C=======================================================================
      IF (ERROR) GO TO 999
C
C Get addresses of all the relevant parts
C
      CALL DATGETAR (STRM3(NAME, SUB, 'VIS'), NAX, NAXIS, ATYPE, VSADD)
      VSNADD = DATADD (STRM3(NAME, CAL, 'VIS'))
      WTADD = DATADD (STRM3(NAME, SUB, 'WT'))
      WTNADD = DATADD (STRM3(NAME, CAL, 'WT'))
      BADD = DATADD (STRM2(NAME, 'BASELINE'))
C
      CALL VISBLCOP (MEMX(VSADD), MEMR(BADD), MEMR(WTADD), NAXIS(1), 
     1   NANT, CORGAIN, MEMX(VSNADD), MEMR(WTNADD), NFLAG)
      IF (ERROR) GO TO 990
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)defix.f	1.2 24 Jan 1995
C
      SUBROUTINE VISBLCOP (VIS, BASE, WT, NVIS, NANT, CORGAIN,
     1   NEWVIS, NEWWT, NFLAG)
C
CD Calibrate data
C
C
C	VIS	CMPLX	input	Input visibilities
C	BASE	REAL(*)	input	Baselines
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	NANT	INT	input	Number of antennas
C	FLUX	REAL	input	Estimated flux
C	NEWVIS	CMPLX	output	Output visibilities
C	NEWWT	REAL	output	Output weights
C	NFLAG	INT	output	Number of vis. flagged
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NVIS, NANT, NFLAG
      COMPLEX		VIS(NVIS), NEWVIS(NVIS), CORGAIN(NANT, *)
      REAL		BASE(NVIS), WT(NVIS)
      REAL		NEWWT(NVIS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISBLCOP')
C
      INTEGER		IVIS, IA1, IA2
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NFLAG = 0
      DO 300 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = WT(IVIS)
            NFLAG = NFLAG + 1
            GO TO 300
         END IF
         IA1 = NINT(BASE(IVIS)/256.0)
         IA2 = NINT(BASE(IVIS)-FLOAT(256*IA1))
         IF (ABS(CORGAIN(IA1,IA2)).NE.0.0) THEN
            NEWVIS(IVIS) = VIS(IVIS) / CORGAIN (IA1,IA2)
         ELSE
            NEWVIS(IVIS) = VIS(IVIS)
            NEWWT(IVIS) = SIGN (WT(IVIS), -1.0)
            NFLAG = NFLAG + 1
         END IF
 300  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
