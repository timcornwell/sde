C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)simpe.f	1.3    7/18/97
C
      SUBROUTINE SIMPE (ANT, HAMIN, HAMAX, HAN, DECL, INTT, VIS,
     $   GLPNTAZ, GLPNTEL, INPNTAZ, INPNTEL, DRFTAZ, DRFTEL, 
     $   RANAZ, RANEL)
C
CD Simulate antenna pointing errors
C
C      GLPNTAZ, GLPNTEL, INPNTAZ, INPNTEL, DRFTAZ, DRFTEL, 
C      RANAZ, RANEL)
C
C	ANT	CH*(*)	input	Name of antenna file
C      HAMIN   DBLE    input   Minimum hour angle 
C      HAMAX   DBLE    input   Maximum hour angle
C	HAN	DBLE	input	Hour Angle Now 
C      DECL    DBLE    input   Declination of observation
C      INTT    DBLE    input   Integration time
C      VIS     CH*(*)  input   Name of directory for visibility file
C		ALL pointing errors are in DEGREES:
C	GLPNTAZ	REAL	input	Global pointing error, AZ
C	GLPNTEL	REAL	input	Global pointing error, EL
C
C	INPNTAZ	REAL	input	Initial Pointing error, AZ
C	INPNTEL	REAL	input	Initial Pointing error, EL
C				(random among antennae, constant with time)
C	DRFTAZ	REAL	input	Drift from initial pointing, AZ
C	DRFTEL	REAL	input	Drift from initial pointing, EL
C				(can vary any way; idea was linear drift)
C	RANAZ	REAL	input	Amplitude of random component of pointing error
C	RANEL	REAL	input	Amplitude of random component of pointing error
C
C Audit trail:
C	Generate random antenna offsets in RA and DEC
C				M.A.Holdaway	Sep 17 1989
C	Created a much more general scheme of pointing errors
C				M.A.Holdaway	Dec 7 1989
C	We are given errors in AZ, EL and convert to errors in
C	RA, DEC.
C
C	Stop printing out pointing errors
C				M.A.Holdaway	March 28 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ANT, VIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIMPE')
C
      INTEGER		NANT, NDUMMY, SEED
      DOUBLE PRECISION  PI, TWOPI, DECL, INTT
      DOUBLE PRECISION  INTTIME, HAMIN, HAMAX
      DOUBLE PRECISION  HMIN, HMAX, TINTD, HAN
      REAL		GLPNTAZ, GLPNTEL, INITAZ, INITEL
      REAL		INPNTAZ, INPNTEL, DRFTAZ, DRFTEL, 
     $   		RANAZ,RANEL
C
      REAL		AZOFF, ELOFF, RAND
      LOGICAL		RANDINPNT
      INTEGER		NAX, NAXIS(SYSMXDIM)
      INTEGER		NUMINT, IA1, INTNDX, ILEN, STRLEN
      INTEGER		ROADD, DOADD, IAZADD, IELADD
      CHARACTER*(SYSMXNAM)	STRM2, SYSTIME
      CHARACTER*(2)		STRTIME
      DOUBLE PRECISION		SLAT
      REAL			DECERR, RAERR, AZERR, ELERR
C
      LOGICAL		DATEXIST
      DATA		NAXIS	/SYSMXDIM*1/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get antennas data
C
      CALL DATGETI (ANT, 'NANT', NANT, 1, NDUMMY)
      CALL DATGETD (ANT, 'SITELAT', SLAT, 1, NDUMMY)
C
C Trick to get a SEED which changes from time to time
C
      CALL SYSDATET (SYSTIME)
      ILEN = STRLEN (SYSTIME)
      STRTIME = SYSTIME(ILEN-5:ILEN-5)
      CALL STRAPPEN (STRTIME, SYSTIME(ILEN-6:ILEN-6))
      READ(STRTIME, 30) SEED
 30   FORMAT(I2)
      SEED = 4 * (SEED+100) + 1
      IF (ERROR) GOTO 990
C
C Set up some parameters
C
      RANDINPNT = .TRUE.
      PI = 4D0*ATAN(1D0)
      TWOPI = 8D0*ATAN(1D0)
      HMIN = HAMIN * TWOPI/24D0
      HMAX = HAMAX * TWOPI/24D0
      TINTD = INTT / (60D0*60D0*24D0)
      INTTIME = INTT * TWOPI /86400D0
      NUMINT = MAX0(NINT((HMAX-HMIN)/INTTIME),1)
C
C  make initial pointing offset arrays if needed
C
      IF (.NOT. DATEXIST(STRM2('Scratch', 'INPNTAZ'))) THEN
         NAX = 1
         NAXIS(1) = NANT
         NAXIS(2) = 1
         CALL DATMAKAR (STRM2('Scratch', 'INPNTAZ'), NAX, 
     $      NAXIS, 'R', IAZADD)
         CALL DATMAKAR (STRM2('Scratch', 'INPNTEL'), NAX, 
     $      NAXIS, 'R', IELADD)
         IF (ERROR) GO TO 990
C
C	 fill the initial pointing offset arrays
C
         DO 90 IA1 = 0, NANT-1
            CALL UTLGRAND (RAND, SEED)
            AZOFF = INPNTAZ  * RAND
            CALL UTLGRAND (RAND, SEED)
            ELOFF = INPNTEL * RAND
            MEMR (IAZADD + IA1 ) = AZOFF + GLPNTAZ
            MEMR (IELADD + IA1 ) = ELOFF + GLPNTEL
 90      CONTINUE
      ENDIF
      IF (ERROR) GO TO 990
C
C  make the pointing offset arrays, fill with pointing errors after
C  converting from (AZ, EL) errors to (RA, DEC) errors
C
      NAX = 2
      NAXIS(1) = NANT
      NAXIS(2) = NUMINT
      CALL DATMAKAR (STRM2(VIS, 'RAOFF'), NAX, NAXIS, 'R', ROADD)
      CALL DATMAKAR (STRM2(VIS, 'DECOFF'), NAX, NAXIS, 'R', DOADD)
      IF (ERROR) GO TO 990
C
C     make pointing errors; drift plus random part, add to initial
C
      DO 120 INTNDX = 0, NUMINT*NANT-1, NANT
         DO 110 IA1 = 0, NANT-1
            INITAZ = MEMR (IAZADD + IA1)
            INITEL = MEMR (IELADD + IA1)
            CALL UTLGRAND (RAND, SEED)
            AZERR = RANAZ * RAND  +  DRFTAZ + INITAZ
            CALL UTLGRAND (RAND, SEED)
            ELERR = RANEL * RAND  +  DRFTEL + INITEL
            CALL SIMAERD (DECL, HAN, SLAT, AZERR, ELERR,
     $         RAERR, DECERR)
            MEMR (ROADD + INTNDX + IA1 ) = RAERR
            MEMR (DOADD + INTNDX + IA1 ) = DECERR
 110     CONTINUE
 120  CONTINUE
C
C View the pointing errors
C
C      CALL SEEPOINT (MEMR(ROADD), MEMR(DOADD), NAX, NAXIS, DECL )
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C
      SUBROUTINE SEEPOINT (RAERR, DECERR, NAX, NAXIS, DECL)
C
C Look at the statistics of the pointing errors and print out
C
C
C	NAME	CH*(*)	input	Name of directory entry
C	RAERR	REAL	input	Error in RA
C	DECERR	REAL	input	Error in DEC
C	NAX	INT	input	Number of Axes
C	NAXIS	INT*(*)	input	Dimensions of Axes
C	DECL	REAL	input	Declination
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	7 DEC 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      REAL		RAERR(NAXIS(1), *), DECERR(NAXIS(1), *)
      DOUBLE PRECISION		DECL
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SEEPOINT')
C
      INTEGER		IANT, INTNDX
      DOUBLE PRECISION	PI
      REAL		DAVE, RAVE, DD, RR
      CHARACTER*(SYSMXNAM)	STRREAL
C
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C We want to calculate the mean and RMS of RA, DEC errors
C
      PI = 4D0*ATAN(1D0)
      DO 120 INTNDX = 1, NAXIS(2)
         DAVE = 0.
         RAVE = 0.
         DD = 0.
         RR = 0.
         DO 100 IANT = 1, NAXIS(1)
            DAVE = DAVE + DECERR( IANT , INTNDX )
            RAVE = RAVE +  RAERR( IANT , INTNDX )
 100     CONTINUE
         DAVE = DAVE/ FLOAT(NAXIS(1))
         RAVE = RAVE/ FLOAT(NAXIS(1))  
         DO 110 IANT = 1, NAXIS(1)
            DD = DD + (DECERR( IANT, INTNDX ) - DAVE) **2
            RR = RR + (RAERR ( IANT, INTNDX ) - RAVE) **2
 110     CONTINUE
         DD = SQRT( DD / FLOAT (NAXIS(1)) )* 3600.
         RR = SQRT( RR / FLOAT (NAXIS(1)) ) * 3600. *COS(DECL*PI/180.D0)
         RAVE = RAVE * 3600. * COS(DECL*PI/180.D0)
         DAVE = DAVE * 3600.
C         
         MESSAGE = ' RA ERROR AVE, arcseconds '
         CALL STRAPNB2 (MESSAGE, ' = ', STRREAL(RAVE, 3))
         CALL MSGPUT( MESSAGE, 'I')
         MESSAGE = 'DEC ERROR AVE, arcseconds '
         CALL STRAPNB2 (MESSAGE, ' = ', STRREAL(DAVE, 3))
         CALL MSGPUT( MESSAGE, 'I')
         MESSAGE = ' RA ERROR SPREAD, arcseconds '
         CALL STRAPNB2 (MESSAGE, ' = ', STRREAL(RR, 3))
         CALL MSGPUT( MESSAGE, 'I')
         MESSAGE = 'DEC ERROR SPREAD, arcseconds '
         CALL STRAPNB2 (MESSAGE, ' = ', STRREAL(DD, 3))
         CALL MSGPUT( MESSAGE, 'I')
         MESSAGE = '   .   '
         CALL MSGPUT( MESSAGE, 'I')
 120  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



