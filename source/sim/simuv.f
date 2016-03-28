C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)simuv.f	1.12	 6/28/94
C
      SUBROUTINE SIMUV (FREQ, ANT, HAMIN, HAMAX, DECL, ELMIND, TSTART,
     $   INTT, AUTOC, AUTOW, VIS)
C
CD Make a u,v database for a simulated array ANT with obs. parms.
C This fills in some initial guesses for various things, but these
C should probably be changed later.
C
C      ANT/NANT        INT     Number of antennas
C      ANT/SITELAT     DBLE    Site latitude
C      ANT/VX          DBLE    Earth-based cartesian VLB x coord. of ants.
C      ANT/VY          DBLE    Earth-based cartesian VLB y coord. of ants.
C      ANT/VZ          DBLE    Earth-based cartesian VLB z coord. of ants.
C      ANT/RX          DBLE    Local cartesian x coord. of ants.
C      ANT/RY          DBLE    Local cartesian y coord. of ants.
C      ANT/RZ          DBLE    Local cartesian z coord. of ants.
C      ANT/DIAM        DBLE    Diameter of ants.
C      ANT/UANT        DBLE    Re-usable array to hold u coord.
C      ANT/VANT        DBLE    Re-usable array to hold u coord.
C      ANT/WANT        DBLE    Re-usable array to hold u coord.
C      ANT/SHAD        LOG(*)  Re-usable array to hold shadowing info.
C
C      FREQ	DBLE	input	Frequency
C      ANT	CH*(*)	input	Name of directory entry for array descr.
C      HAMIN    DBLE    input  Minimum hour angle 
C      HAMAX    DBLE    input  Maximum hour angle 
C      DECL     DBLE    input  Declination of observation
C      ELMIND   DBLE    input  Minimum allowed elevation [degrees]
C      TSTART	DBLE	input  Starting time of observation
C      INTT     DBLE    input  Integration time
C      AUTOC    LOG     input  Are autocorrelations required
C      AUTOW    REAL    input  Weighting for autocorrelations 
C      VIS     CH*(*)   input  Name of directory for visibility file
C Audit trail:
C	Added SDETYPE
C				T.J.Cornwell	Feb 4 1989
C	Added TINT to output header
C				R.Braun 	Jul 9 1989
C	Antennas can now be flagged by setting the diameter to
C	a non-positive number
C				T.J. Cornwell	Sept 25 1989
C	Informative message about shadowing
C				T.J. Cornwell	Oct 2 1989
C       FITS Groups related Keywords added
C                               R.G. Marson     Nov 1 1990
C       Changed GCOUNT from Real to an INT
C                               R.G. Marson     Nov 2 1990
C	Initialized Visibility to zero
C				T.J. Cornwell	Jan 28 1991
C	Added PARANGLE (real, degrees) to output header
C				M.A.Holdaway	Feb 28 1991
C	Added AZ, EL, and PA arrays to VIS directory;
C       Write RX, RY, RZ arrays to the output VIS directory;
C	CHANGED CALLING:  beginning TIME is input
C				M.A.Holdaway	May 9 1991
C	Added a new time array VIS/STIME that is easily accesible with
C	station-based information (AZ, EL, PA, RX, RY, RZ)
C	RX, RY, RZ: NAX = 1, NAXIS(1) = NANT
C	AZ, EL, PA: NAX = 2, NAXIS(1) = NANT, NAXIS(2) = NUMINT
C	STIME:      NAX = 1, NAXIS(1) = NUMINT
C				M.A.Holdaway	May 15 1991
C	Calculations updated to allow simulation of VLBI arrays.
C       The blockage calcs are going to be a tad off in this case,
C       but it hardly matters.
C				D.S.Briggs	Jul 25 1992
C	Prior to now, LST (ie, hour angle) and UT (ie, integration time)
C	were based on the same clock!
C				M.A.Holdaway	Sep 23 1992
C	Now weights can be simulated with weights:
C	WT = ((1.0 + ZenithNoise)/( 1.0 + AtmosNoise))**2
C 	AtmosNoise = Tatm * (1.0 - EXP(-TAU/SIN(D2R*EL1))) * 
C                      EXP(TAU/SIN(D2R*EL1))
C	ZenithNoise = Tatm * (1.0 - EXP(-TAU)) * EXP(TAU)
C	Tatm is the temperature of the atmosphere
C	TAU is the optical depth at the zenith
C
C	Zenith Observations will have a wt of 1.0
C	Only works when simulating an observation from scratch
C				M.A. Holdaway	June 14 1994
C	Also, ELMIN was previously in Radians, and was being compared
C	to EL1, EL2 which were in DEGREES.  Replaced ELMIN with
C	ELMIND, it seems to work correctly now.
C				M.A. Holdaway	June 14 1994
C	Removed low elevation approximation, now more accurate
C				M.A. Holdaway	June 28 1994
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ANT, VIS
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIMUV')
C
      INTEGER		NANT, NDUMMY, I, LXADD, LYADD, LZADD, DMADD,
     $   		SHADD, UIADD, VIADD, WIADD, LATADD,
     $   		LONGADD
      DOUBLE PRECISION  SITELAT, PI, TWOPI, DECL, ELMIND, INTT, FREQ,
     $   		DEC, INTTIME, H, H0, HAMIN, HAMAX,
     $   		TSTART, HMIN, HMAX, TINTD, FB1, FB2, AZ, EL,
     $   		PA, LAT, D2R
      LOGICAL           AUTOC
      REAL		AUTOW, TINT, EL1, EL2, TAU, TATM, TREC, ZNOISE
C
      REAL		WAVE, T, PAD, ELD, AZD, HAD, TFACTOR
      REAL		ATMONOIS
C
      INTEGER		NAX, NAXIS(SYSMXDIM), SNAX, SNAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER		NUMINT, NBASE, NVIS, IA1, IA2, INTNDX, NSHAD,
     $			NEL, INT
      INTEGER		UADD, VADD, WADD, TADD, BADD, VSADD, WTADD,
     $   		ELADD, AZADD, PAADD, STADD, HAADD
C
      INTEGER			DATADD
      CHARACTER*(SYSMXNAM)	STRM2
      LOGICAL			DATEXIST
C
      DATA		NAXIS	/SYSMXDIM*1/
      DATA		CTYPE	/SYSMXDIM*' '/
      DATA		CRPIX	/SYSMXDIM*0.0/
      DATA		CDELT	/SYSMXDIM*0.0/
      DATA		CROTA	/SYSMXDIM*0.0/
      DATA		CRVAL	/SYSMXDIM*0.0D0/
C=======================================================================
      IF (ERROR) GO TO 999
      TFACTOR = (360.0 + 360.0/365.25)/360.0
      D2R = ATAN2(1.0, 1.0)/45.0
C
C Get antennas data
C
      CALL DATGETI (ANT, 'NANT', NANT, 1, NDUMMY)
      CALL DATGETD (ANT, 'SITELAT', SITELAT, 1, NDUMMY)
      LATADD = DATADD(STRM2(ANT, 'LAT'))
      LONGADD = DATADD(STRM2(ANT, 'LONG'))
      LXADD = DATADD(STRM2(ANT, 'LX'))
      LYADD = DATADD(STRM2(ANT, 'LY'))
      LZADD = DATADD(STRM2(ANT, 'LZ'))
      DMADD = DATADD(STRM2(ANT, 'DIAM'))
      UIADD = DATADD(STRM2(ANT, 'UANT'))
      VIADD = DATADD(STRM2(ANT, 'VANT'))
      WIADD = DATADD(STRM2(ANT, 'WANT'))
      SHADD = DATADD(STRM2(ANT, 'SHANT'))
      IF (DATEXIST(STRM2(ANT, 'Tau'))) THEN
         CALL DATGETR ('Antfile', 'Tau', TAU, 1, NDUMMY)
         CALL DATGETR ('Antfile', 'Tatm', TATM, 1, NDUMMY)
         CALL DATGETR ('Antfile', 'Trec', TREC, 1, NDUMMY)
         ZNOISE = TREC + TATM * (1.0 - EXP(-TAU))*EXP(TAU)
      ELSE
         TAU = 0.0
      ENDIF

C
C Create uv directory
C
      CALL DATCREAT (VIS)
      CALL DATCREAT (STRM2(VIS, 'OBS'))
      CALL DATCREAT (STRM2(VIS, 'OBS/I'))
      CALL DATSETTP (VIS, 'VIS')
C
      CALL DATPUTC (VIS, 'OBJECT', 'TEST', 1)
      CALL DATPUTC (VIS, 'TELESCOP', '?', 1)
      CALL DATPUTC (VIS, 'INSTRUME', '?', 1)
      CALL DATPUTC (VIS, 'DATE-OBS', '?', 1)
      CALL DATPUTC (VIS, 'OBSERVER', '?', 1)
      TINT = INTT
      CALL DATPUTR (VIS, 'TINT',TINT, 1)
C
      WAVE = 3.0E8 / FREQ
      NAX = 3
      CRVAL(1) = 0.0
      CRVAL(2) = DECL
      CRVAL(3) = FREQ
      CTYPE(1) = 'RA'
      CTYPE(2) = 'DEC'
      CTYPE(3) = 'FREQ'
      CDELT(3) = 0.1 * FREQ
      CALL CRDPUT (STRM2(VIS, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL, 
     1   CRPIX, CDELT, CROTA)
C
C Set up some parameters
C
      PI = 4D0*ATAN(1D0)
      TWOPI = 8D0*ATAN(1D0)
      HMIN = HAMIN * TWOPI/24D0
      HMAX = HAMAX * TWOPI/24D0
      DEC = DECL * TWOPI / 360D0
      TINTD = TINT / (60D0*60D0*24D0)
      INTTIME = TINT * TWOPI /86400D0
      NBASE = (NANT * (NANT-1))/2
      IF (AUTOC) NBASE = NBASE + NANT
      NUMINT = MAX(NINT((HMAX-HMIN)/(INTTIME * TFACTOR)),1)
      NVIS = NBASE * NUMINT
C
C Now make the various arrays
C
      NAX = 1
      NAXIS(1) = NVIS
      CALL DATPUTL(VIS, 'GROUPS', .TRUE., 1)
      CALL DATPUTI(VIS, 'GCOUNT', NVIS, 1)
      CALL DATPUTI(VIS, 'PCOUNT', 5, 1)
      CALL DATMAKAR (STRM2(VIS, 'UU'), NAX, NAXIS, 'R', UADD)
      CALL DATPUTC(VIS, 'PTYPE1', 'UU', 1)
      CALL DATMAKAR (STRM2(VIS, 'VV'), NAX, NAXIS, 'R', VADD)
      CALL DATPUTC(VIS, 'PTYPE2', 'VV', 1)
      CALL DATMAKAR (STRM2(VIS, 'WW'), NAX, NAXIS, 'R', WADD)
      CALL DATPUTC(VIS, 'PTYPE3', 'WW', 1)
      CALL DATMAKAR (STRM2(VIS, 'BASELINE'), NAX, NAXIS, 'R', BADD)
      CALL DATPUTC(VIS, 'PTYPE4', 'BASELINE', 1)
      CALL DATMAKAR (STRM2(VIS, 'TIME'), NAX, NAXIS, 'R', TADD)
      CALL DATPUTC(VIS, 'PTYPE5', 'TIME', 1)
      CALL DATMAKAR (STRM2(VIS, 'OBS/I/VIS'), NAX, NAXIS, 'X', VSADD)
      CALL DATMAKAR (STRM2(VIS, 'OBS/I/WT'), NAX, NAXIS, 'R', WTADD)
C
C Station-Based Arrays
C
      CALL ARRCOPY  (STRM2(ANT, 'LAT'), STRM2(VIS, 'LAT'))
      CALL ARRCOPY  (STRM2(ANT, 'LONG'), STRM2(VIS, 'LONG'))
      CALL ARRCOPY  (STRM2(ANT, 'RX'), STRM2(VIS, 'RX'))
      CALL ARRCOPY  (STRM2(ANT, 'RY'), STRM2(VIS, 'RY'))
      CALL ARRCOPY  (STRM2(ANT, 'RZ'), STRM2(VIS, 'RZ'))
      SNAX = 1
      SNAXIS(1) = NUMINT
      CALL DATMAKAR (STRM2(VIS, 'STIME'), SNAX, SNAXIS, 'R', STADD)
      SNAX = 2
      SNAXIS(1) = NANT
      SNAXIS(2) = NUMINT
      CALL DATMAKAR (STRM2(VIS, 'HA'), SNAX, SNAXIS, 'R', HAADD)
      CALL DATMAKAR (STRM2(VIS, 'EL'), SNAX, SNAXIS, 'R', ELADD)
      CALL DATMAKAR (STRM2(VIS, 'AZ'), SNAX, SNAXIS, 'R', AZADD)
      CALL DATMAKAR (STRM2(VIS, 'PA'), SNAX, SNAXIS, 'R', PAADD)
      IF (ERROR) GO TO 990
C
C Set up baseline array and u,v arrays
C
      NSHAD = 0
      NEL = 0
      H0 = HMIN
      T = TSTART
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 9876) TSTART
 9876    FORMAT ('TSTART = ', E14.6)
         CALL MSGPUT (MESSAGE, 'D')
      ENDIF
      INT = 0
      DO 100 INTNDX = 1, NVIS, NBASE
C
C Station-Based Information
C
         DO 10 IA1 = 0, NANT - 1
            H = H0 + MEMD (LONGADD + IA1)
            LAT = MEMD (LATADD + IA1)
            CALL GETANGLS (H, DEC, LAT, AZ, EL, PA)
            HAD = H * 360D0 / TWOPI
            PAD = PA * 360D0 / TWOPI
            ELD = EL * 360D0 / TWOPI
            AZD = AZ * 360D0 / TWOPI
            MEML (SHADD + IA1) = .FALSE.
            MEMR (HAADD + IA1 + INT*NANT ) = HAD
            MEMR (ELADD + IA1 + INT*NANT ) = ELD
            MEMR (AZADD + IA1 + INT*NANT ) = AZD
            MEMR (PAADD + IA1 + INT*NANT ) = PAD
  10     CONTINUE
         MEMR (STADD + INT) = T
         I = 0
C
C Correlation-Based Information
C
         CALL UVANT (H0, DEC, NANT, MEMD(LXADD), MEMD(LYADD),
     $      MEMD(LZADD), MEMD(UIADD), MEMD(VIADD), MEMD(WIADD))
         DO 30 IA1 = 1, NANT-1
            DO 20 IA2 = IA1+1, NANT
               EL1 = MEMR(ELADD+INT*NANT+IA1-1)
               EL2 = MEMR(ELADD+INT*NANT+IA2-1)
               MEMR (BADD + INTNDX + I - 1) = FLOAT (256 * IA1 + IA2)
               MEMR (UADD+INTNDX+I-1) = (MEMD(UIADD+IA2-1)
     1                           - MEMD(UIADD+IA1-1)) / WAVE
               MEMR (VADD+INTNDX+I-1) = (MEMD(VIADD+IA2-1)
     1                           - MEMD(VIADD+IA1-1)) / WAVE
               MEMR (WADD+INTNDX+I-1) = (MEMD(WIADD+IA2-1)
     1                           - MEMD(WIADD+IA1-1)) / WAVE
               IF((MEMD(DMADD+IA1-1).GT.0.0D0).AND.
     $            (MEMD(DMADD+IA2-1).GT.0.0D0)) THEN
                  CALL BLOCKAGE(IA1, IA2, MEMD(UIADD), MEMD(VIADD),
     1               MEMD(WIADD), MEMD(DMADD), FB1, FB2)
                  IF (FB1.GT.1D-5) MEML(SHADD + IA1 - 1) = .TRUE.
                  IF (FB2.GT.1D-5) MEML(SHADD + IA2 - 1) = .TRUE.
                  IF ((EL1.LT.ELMIND).OR.(EL2.LT.ELMIND)) THEN
                     MEMR(WTADD+INTNDX+I-1) = -1.0
                     NEL = NEL + 1
                  ELSE
                     IF (TAU .GT. 0.0) THEN
C
C Doesn't correctly treat EL1 .NE. EL2
C
                        ATMONOIS =  TREC + TATM * 
     $                       (1.0 - EXP(-TAU/SIN(D2R*EL1)))
     $                       * EXP(TAU/SIN(D2R*EL1))
                        MEMR(WTADD+INTNDX+I-1) = ((1.0+ZNOISE)/
     $                       (1.0 + ATMONOIS))**2
                     ELSE
                        MEMR(WTADD+INTNDX+I-1) = 1.0
                     ENDIF
                  END IF
               ELSE
                  MEMR(WTADD+INTNDX+I-1) = -1.0
               END IF
               MEMX (VSADD+INTNDX+I-1) = 0.0
               MEMR (TADD+INTNDX+I-1) = T
               I = I + 1
  20        CONTINUE
  30     CONTINUE
         IF (AUTOC) THEN
            DO 40 IA1 = 1, NANT
               EL1 = MEMR(ELADD+INT*NANT+IA1-1)
               MEMR (BADD + INTNDX + I - 1) = FLOAT (256 * IA1 + IA1)
               MEMR (UADD+INTNDX+I-1) = 0.
               MEMR (VADD+INTNDX+I-1) = 0.
               MEMR (WADD+INTNDX+I-1) = 0.
               IF(MEMD(DMADD+IA1-1).LT.0.0D0) THEN
                  MEMR(WTADD+INTNDX+I-1) = -1.0
               ELSE IF(EL1.LT.ELMIND) THEN
                  MEMR(WTADD+INTNDX+I-1) = -1.0
                  NEL = NEL + 1
               ELSE
                  MEMR (WTADD + INTNDX + I - 1) = AUTOW
               END IF
               MEMX (VSADD+INTNDX+I-1) = 0.0
               MEMR (TADD + INTNDX + I - 1) = T
               I = I + 1
  40        CONTINUE
         END IF
C
C Look after shadowing
C
         I = 0
         DO 60 IA1 = 1, NANT-1
            DO 50 IA2 = IA1+1, NANT
               IF ((MEMR(WTADD+INTNDX+I-1).GT.0.0).AND.
     $            ((MEML(SHADD+IA1-1).OR.MEML(SHADD+IA2-1)))) THEN
                  MEMR(WTADD+INTNDX+I-1) = -1.0
                  NSHAD = NSHAD + 1
               END IF
               I = I + 1
  50        CONTINUE
  60     CONTINUE
         IF (AUTOC) THEN
            DO 70 IA1 = 1, NANT
               IF ((MEMR(WTADD+INTNDX+I-1).GT.0.0).AND.
     $            (MEML(SHADD+IA1-1))) THEN
                  MEMR(WTADD+INTNDX+I-1) = -1.0
                  NSHAD = NSHAD + 1
               END IF
               I = I + 1
  70        CONTINUE
         END IF
         H0 = H0 + INTTIME*TFACTOR
         T = T + TINTD
         INT = INT + 1
 100  CONTINUE
C
C Save the last value of PA as 'characteristic of the run'
C
         CALL DATPUTR (VIS, 'PARANGLE', PAD, 1)
C
C Write out number of shadowed points
C
      WRITE (MESSAGE, 1000) NVIS, NEL, NSHAD
 1000 FORMAT (I7,' points, ',I6,' sub-elevation, ',I6,' shadowed')
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
