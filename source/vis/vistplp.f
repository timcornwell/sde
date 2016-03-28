C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vistplp.f	1.4	 12/22/92
C
      SUBROUTINE VISTPLP (VIS, DEVPH, DEVAM, BASL, TIME, WT, 
     $   DTIME, DAMP, DPHASE, ANT1, ANT2, NVIS, STOKES, STYLE)
C
CD Plot visibility data in baseline order
C
C
C	VIS	CMPLX	input	Input visibilities
C	DEV	CH*(*)	input	Graphics Device
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	DTIME	REAL(*)	inp	Dummy Time
C	DAMP	REAL(*)	inp	Dummy Amp
C	DPHASE	REAL(*)	inp	Dummy Phase
C	NVIS	INT	input	Number of visibilities
C	STOKES	CH*(*)	inp	Stokes label for plotting
C	STYLE	INT	input	Style for plot characters
C Audit trail:
C	New routine
C				M.A.Holdaway	19 Sep 1991
C	Track change to PIXPGGRF
C				D.S.Briggs	15 July 1992
C	Added STOKES to plot label
C				M.A.Holdaway	22 DEC 1992
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	DEVPH, DEVAM, STOKES
      INTEGER		NVIS, STYLE
      REAL		BASL(*), TIME(*), WT(*), DTIME(*), DAMP(*), 
     $   		DPHASE(*)
      COMPLEX		VIS(*)
      INTEGER		ANT1, ANT2
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTPLP')
C
      INTEGER		IVIS, JA1, JA2, ORDER, NP
      REAL		AMP, PHASE, PI, TMIN, TMAX, PMIN, PMAX, AMIN, 
     $   		AMAX
      CHARACTER*4	STRINT
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      PI = 4.0*ATAN(1.0)
C
C Loop over data
C
      NP = 0
      TMIN = 999999.
      TMAX = -999999.
      PMIN = 999999.
      PMAX = -999999.
      AMIN = 999999.
      AMAX = -999999.
C
      DO 100 IVIS = 1, NVIS
         JA1 = NINT(BASL(IVIS)/256.0)
         JA2 = NINT(BASL(IVIS)-FLOAT(256*JA1))
         IF (JA1 .EQ. ANT1 .AND. JA2 .EQ. ANT2) THEN
            ORDER = 1
         ELSE IF (JA1 .EQ. ANT2 .AND. JA2 .EQ. ANT1) THEN
            ORDER = -1
         ELSE
            ORDER = 0
         ENDIF
C                  
         IF (ORDER .NE. 0 .AND. WT(IVIS) .GT. 0.0) THEN
            AMP = ABS(VIS(IVIS))
            IF (AMP.GT.0.0) THEN
               PHASE = ORDER * 180.0 * ATAN2 (AIMAG(VIS(IVIS)), 
     $            REAL(VIS(IVIS))) / PI
            ELSE
               PHASE = 0.0
            END IF
            NP = NP + 1
            DTIME(NP) = TIME(IVIS) * 86400.0
            DAMP(NP)  = AMP
            DPHASE(NP)= PHASE
            TMIN = MIN (TMIN, DTIME(NP))
            TMAX = MAX (TMAX, DTIME(NP))
            PMIN = MIN (PMIN, DPHASE(NP))
            PMAX = MAX (PMAX, DPHASE(NP))
            AMIN = MIN (AMIN, DAMP(NP))
            AMAX = MAX (AMAX, DAMP(NP))
         ENDIF
 100  CONTINUE
C
      IF (DEVPH .NE. ' ') THEN
         WRITE (MESSAGE, 1212) STOKES, ANT1, ANT2
 1212    FORMAT (A2, ' Phase for baseline ',2I4)
         CALL PIXPGGRF(NP, DTIME, DPHASE, TMIN, TMAX, PMIN, PMAX,
     $      DEVPH, 'Time, s', 'Phase, degrees',
     $      MESSAGE, STYLE, 0)
      ENDIF
      IF (DEVAM .NE. ' ') THEN
         WRITE (MESSAGE, 1222) STOKES, ANT1, ANT2
 1222    FORMAT (A2, ' Amplitude for baseline ',2I4)
         CALL PIXPGGRF (NP, DTIME, DAMP, TMIN, TMAX, AMIN, AMAX,
     $      DEVAM, 'Time, s', 'Amp, Jy',
     $      MESSAGE, STYLE, 0)
      ENDIF
         
C
C Can jump to here if an error found
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
      
      
      
