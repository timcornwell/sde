C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visself.f	1.1 11/19/94
C
      SUBROUTINE VISSELF (VIS, MODE, STOKES, TAMP, TPHASE)
C
CD Do selfcal separately for different stokes
C
C Assumes presence of OBS and MOD classes.
C
C Audit trail:
C-----------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	VIS, MODE, STOKES
      REAL		TAMP, TPHASE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSELF')
C
      CHARACTER*(SYSMXNAM)	STRM2
      REAL		SUMWT, SUMWTS, VBEFORE, VAFTER, CHISQ,
     $   		IGAIN
      INTEGER		NU
C==================================================================
      IF (ERROR) GOTO 999
C
      CALL VISCHISQ (VIS, 'OBS/I', 'MOD/I', CHISQ,
     $   SUMWT, SUMWTS, NU)
      IF (ERROR) GO TO 999
      VBEFORE=SQRT(CHISQ/(2.0*SUMWT))
      WRITE (MESSAGE, 2710) VBEFORE
 2710 FORMAT ('Before selfcal, visibility fit is ',1PE12.3,
     $   ' Jy for unit weight')
      CALL MSGPUT (MESSAGE, 'I') 
      IF(STOKES.EQ.'IV') THEN
         CALL MSGPUT ('Selfcalibrating R and L separately', 'I')
         IF(MODE.EQ.'AMPNORMPHI') THEN
            CALL MSGPUT ('First R', 'I')
            CALL FLYSSCAL (VIS, 'OBS/RR', 'MOD/I', 'OBS/RR', 
     $         'AMPPHI', TAMP, TPHASE)
            CALL MSGPUT ('Now L', 'I')
            CALL FLYSSCAL (VIS, 'OBS/LL', 'MOD/I', 'OBS/LL', 
     $         'AMPPHI', TAMP, TPHASE)
         ELSE
            CALL MSGPUT ('First R', 'I')
            CALL FLYSSCAL (VIS, 'OBS/RR', 'MOD/I', 'OBS/RR', 
     $         MODE, TAMP, TPHASE)
            CALL MSGPUT ('Now L', 'I')
            CALL FLYSSCAL (VIS, 'OBS/LL', 'MOD/I', 'OBS/LL', 
     $         MODE, TAMP, TPHASE)
         END IF
         CALL ARRLC (
     &      STRM2(VIS,'OBS/RR/VIS'), 0.5,
     $      STRM2(VIS,'OBS/LL/VIS'), 0.5,
     &      STRM2(VIS,'OBS/I/VIS'))
         CALL ARRLC (
     $      STRM2(VIS,'OBS/RR/VIS'), 0.5,
     $      STRM2(VIS,'OBS/LL/VIS'), -0.5,
     &      STRM2(VIS,'OBS/V/VIS'))
         IF (MODE.EQ.'AMPNORMPHI') THEN
            CALL VISNORM (VIS, 'OOBS/I', 'OBS/I', IGAIN)
            IF (ERROR) GO TO 999
            WRITE (MESSAGE, 3666) IGAIN
 3666       FORMAT ('I gain is ',F10.5)
            CALL MSGPUT (MESSAGE, 'I')
            CALL ARRSCALE (STRM2(VIS,'OBS/I/VIS'), IGAIN, 0.0,
     $         STRM2(VIS,'OBS/I/VIS'))
         END IF
      ELSE IF(STOKES.EQ.'I') THEN
         CALL FLYSSCAL (VIS, 'OBS/I', 'MOD/I', 'OBS/I',
     $      MODE, TAMP, TPHASE)
      ELSE
         CALL MSGPUT ('Cannot selfcalibrate this Stokes parameter',
     &      'I')
      END IF
      CALL ARRSUBTR (
     $   STRM2(VIS,'OBS/I/VIS'), STRM2(VIS,'MOD/I/VIS'), 
     1   STRM2(VIS,'RES/I/VIS'))
      IF (ERROR) GO TO 999
C
      CALL VISCHISQ (VIS, 'OBS/I', 'MOD/I', CHISQ, SUMWT,
     $   SUMWTS, NU)
      IF (ERROR) GO TO 999
      VAFTER = SQRT(CHISQ/(2.0*SUMWT))
      WRITE (MESSAGE, 2700) VAFTER
 2700 FORMAT ('After selfcal, visibility fit is ',1PE12.3,
     $   ' Jy for unit weight')
      CALL MSGPUT (MESSAGE, 'I') 
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
