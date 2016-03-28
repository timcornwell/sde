C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)simtrp.f	1.3	 7/20/92
C
      SUBROUTINE SIMTRP (FREQ, ANT, HAMIN, HAMAX, DECL, ELM, INTT,
     1   AUTOC, AUTOW, TRP)
C
C Make a triple product database for a simulated array ANT with obs. parms.
C This fills in some initial guesses for various things, but these
C should probably be changed later. ANT is a directory entry with the
C following contents:
C
C      ANT/NANT        INT     Number of antennas
C      ANT/SITELAT     DBLE    Site latitude
C      ANT/LX          DBLE    Earth-based cartesian x coord. of ants.
C      ANT/LY          DBLE    Earth-based cartesian y coord. of ants.
C      ANT/LZ          DBLE    Earth-based cartesian z coord. of ants.
C      ANT/DIAM        DBLE    Diameter of ants.
C      ANT/UANT        DBLE    Re-usable array to hold u coord.
C      ANT/VANT        DBLE    Re-usable array to hold u coord.
C      ANT/WANT        DBLE    Re-usable array to hold u coord.
C      ANT/SHAD        LOG(*)  Re-usable array to hold shadowing info.
C
C	FREQ	DBLE	input	Frequency
C	ANT	CH*(*)	input	Name of directory entry for array descr.
C      HAMIN    DBLE    input  Minimum hour angle 
C      HAMAX    DBLE    input  Maximum hour angle 
C      DECL     DBLE    input  Declination of observation
C      ELM      DBLE    input  Minimum allowed elevation
C      INTT     DBLE    input  Integration time
C      AUTOC    LOG     input  Include autocorrelations?
C      AUTOW    REAL    input  Autocorrelation weight
C      TRP     CH*(*)  output  Name of directory for triple products
C Audit trail:
C	New routine
C				T.J.Cornwell	March 15 1989
C      Set triple-product values to zero
C				T.J.Cornwell	March 27 1989
C	Add autocorrelation data
C				T.J.Cornwell	Dec 18 1990
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      CHARACTER*(*)	ANT, TRP
      REAL		AUTOW
      LOGICAL		AUTOC
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SIMTRP')
C
      INTEGER		NANT, NDUMMY, I
      INTEGER           LXADD, LYADD, LZADD, DMADD, SHADD
      INTEGER           UIADD, VIADD, WIADD, DATADD
      DOUBLE PRECISION  SITELAT, PI, TWOPI, DECL, ELM, INTT, FREQ
      DOUBLE PRECISION  DEC, ELMIN, INTTIME, H, HAMIN, HAMAX
      DOUBLE PRECISION  HMIN, HMAX, TINT, FB1, FB2, FB3, AZ, EL
      DOUBLE PRECISION	PARANGLE
C
      REAL		WAVE, T
C
      INTEGER		NAX, NAXIS(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      REAL		CRPIX(SYSMXDIM), CDELT(SYSMXDIM),
     1			CROTA(SYSMXDIM)
      DOUBLE PRECISION	CRVAL(SYSMXDIM)
C
      INTEGER		NUMINT, NBASE, NTRP, IA1, IA2, IA3, INTNDX
      INTEGER		U1ADD, V1ADD, W1ADD
      INTEGER		U2ADD, V2ADD, W2ADD
      INTEGER		TADD, TRPADD, WTADD, TTTADD
      CHARACTER*(SYSMXNAM)	STRM2
C
      DATA		NAXIS	/SYSMXDIM*1/
      DATA		CTYPE	/SYSMXDIM*' '/
      DATA		CRPIX	/SYSMXDIM*0.0/
      DATA		CDELT	/SYSMXDIM*0.0/
      DATA		CROTA	/SYSMXDIM*0.0/
      DATA		CRVAL	/SYSMXDIM*0.0D0/
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C Get antennas data
C
      CALL DATGETI (ANT, 'NANT', NANT, 1, NDUMMY)
      CALL DATGETD (ANT, 'SITELAT', SITELAT, 1, NDUMMY)
      LXADD = DATADD(STRM2(ANT, 'LX'))
      LYADD = DATADD(STRM2(ANT, 'LY'))
      LZADD = DATADD(STRM2(ANT, 'LZ'))
      DMADD = DATADD(STRM2(ANT, 'DIAM'))
      UIADD = DATADD(STRM2(ANT, 'UANT'))
      VIADD = DATADD(STRM2(ANT, 'VANT'))
      WIADD = DATADD(STRM2(ANT, 'WANT'))
      SHADD = DATADD(STRM2(ANT, 'SHANT'))
C
C Create uv directory
C
      CALL DATCREAT (TRP)
      CALL DATCREAT (STRM2(TRP, 'OBS'))
      CALL DATCREAT (STRM2(TRP, 'OBS/I'))
      CALL DATSETTP (TRP, 'TRP')
C
      CALL DATPUTI (TRP, 'NANT', NANT, 1)
      CALL DATPUTC (TRP, 'OBJECT', 'TEST', 1)
      CALL DATPUTC (TRP, 'TELESCOP', '?', 1)
      CALL DATPUTC (TRP, 'INSTRUME', '?', 1)
      CALL DATPUTC (TRP, 'DATE-OBS', '?', 1)
      CALL DATPUTC (TRP, 'OBSERVER', '?', 1)
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
      CALL CRDPUT (STRM2(TRP, 'OBS/I'), NAX, CTYPE, NAXIS, CRVAL, 
     1   CRPIX, CDELT, CROTA)
C
C Set up some parameters
C
      PI = 4D0*ATAN(1D0)
      TWOPI = 8D0*ATAN(1D0)
      HMIN = HAMIN * TWOPI/24D0
      HMAX = HAMAX * TWOPI/24D0
      DEC = DECL * TWOPI / 360D0
      ELMIN = ELM * TWOPI /360D0
      TINT = INTT / (60D0*60D0*24D0)
      INTTIME = INTT * TWOPI /86400D0
      NBASE = (NANT * (NANT-1) * (NANT-2))/6
      IF (AUTOC) NBASE = NBASE + NANT
      NUMINT = MAX(NINT((HMAX-HMIN)/INTTIME),1)
      NTRP = NBASE * NUMINT
C
C Now make the various arrays: remember that now we must keep
C two sets of U,V,W and baseline.
C
      NAX = 1
      NAXIS(1) = NTRP
      CALL DATMAKAR (STRM2(TRP, 'UU1'), NAX, NAXIS, 'R', U1ADD)
      CALL DATMAKAR (STRM2(TRP, 'VV1'), NAX, NAXIS, 'R', V1ADD)
      CALL DATMAKAR (STRM2(TRP, 'WW1'), NAX, NAXIS, 'R', W1ADD)
      CALL DATMAKAR (STRM2(TRP, 'UU2'), NAX, NAXIS, 'R', U2ADD)
      CALL DATMAKAR (STRM2(TRP, 'VV2'), NAX, NAXIS, 'R', V2ADD)
      CALL DATMAKAR (STRM2(TRP, 'WW2'), NAX, NAXIS, 'R', W2ADD)
      CALL DATMAKAR (STRM2(TRP, 'TRIPLE'), NAX, NAXIS, 'R', TTTADD)
      CALL DATMAKAR (STRM2(TRP, 'TIME'), NAX, NAXIS, 'R', TADD)
      CALL DATMAKAR (STRM2(TRP, 'OBS/I/TRP'), NAX, NAXIS, 'X', TRPADD)
      CALL DATMAKAR (STRM2(TRP, 'OBS/I/WT'), NAX, NAXIS, 'R', WTADD)
      IF (ERROR) GO TO 990
C
C Set up baseline array and u,v arrays
C
      H = HMIN
      T = 0.0
      DO 100 INTNDX = 1, NTRP, NBASE
         DO 10 IA1 = 1, NANT
            MEML (SHADD + IA1 - 1) = .FALSE.
  10     CONTINUE
         I = 0
         CALL GETANGLS (H, DEC, SITELAT, AZ, EL, PARANGLE)
         CALL UVANT (H, DEC, NANT, MEMD(LXADD), MEMD(LYADD),
     1     MEMD(LZADD), MEMD(UIADD), MEMD(VIADD), MEMD(WIADD))
         DO 30 IA1 = 1, NANT-2
            DO 20 IA2 = IA1+1, NANT-1
               DO 15 IA3 = IA2+1, NANT
                  MEMX (TRPADD+INTNDX+I-1) = 0.0
                  MEMR (TTTADD+INTNDX+I - 1) = FLOAT (256**2*IA1+
     $               256*IA2+IA3)
                  MEMR (U1ADD+INTNDX+I-1) = (MEMD(UIADD+IA2-1)
     1                              - MEMD(UIADD+IA1-1)) / WAVE
                  MEMR (V1ADD+INTNDX+I-1) = (MEMD(VIADD+IA2-1)
     1                              - MEMD(VIADD+IA1-1)) / WAVE
                  MEMR (W1ADD+INTNDX+I-1) = (MEMD(WIADD+IA2-1)
     1                              - MEMD(WIADD+IA1-1)) / WAVE
                  CALL BLOCKAGE(IA1, IA2, MEMD(UIADD), MEMD(VIADD),
     1                         MEMD(WIADD), MEMD(DMADD), FB1, FB2)
                  MEMR (U2ADD+INTNDX+I-1) = (MEMD(UIADD+IA3-1)
     1                              - MEMD(UIADD+IA2-1)) / WAVE
                  MEMR (V2ADD+INTNDX+I-1) = (MEMD(VIADD+IA3-1)
     1                              - MEMD(VIADD+IA2-1)) / WAVE
                  MEMR (W2ADD+INTNDX+I-1) = (MEMD(WIADD+IA3-1)
     1                              - MEMD(WIADD+IA2-1)) / WAVE
                  CALL BLOCKAGE(IA2, IA3, MEMD(UIADD), MEMD(VIADD),
     1                         MEMD(WIADD), MEMD(DMADD), FB2, FB3)
                  IF (FB2.GT.1D-5) MEML(SHADD + IA2 - 1) = .TRUE.
                  IF (FB3.GT.1D-5) MEML(SHADD + IA3 - 1) = .TRUE.
                  IF (EL.LT.ELMIN) THEN
                     MEMR(WTADD+INTNDX+I-1) = -1.0
                  ELSE
                     MEMR (WTADD + INTNDX + I - 1) = 1.0
                  END IF
                  MEMR (TADD + INTNDX + I - 1) = T
                  I = I + 1
  15           CONTINUE
  20        CONTINUE
  30     CONTINUE
         IF (AUTOC) THEN
            DO 35 IA1 = 1, NANT
               MEMX (TRPADD+INTNDX+I-1) = 0.0
               MEMR (TTTADD+INTNDX+I-1) = FLOAT (256**2*IA1+
     $            256*IA1+IA1)
               MEMR (U1ADD+INTNDX+I-1) = (MEMD(UIADD+IA2-1)
     1                           - MEMD(UIADD+IA1-1)) / WAVE
               MEMR (V1ADD+INTNDX+I-1) = (MEMD(VIADD+IA2-1)
     1                           - MEMD(VIADD+IA1-1)) / WAVE
               MEMR (W1ADD+INTNDX+I-1) = (MEMD(WIADD+IA2-1)
     1                           - MEMD(WIADD+IA1-1)) / WAVE
               MEMR (U2ADD+INTNDX+I-1) = (MEMD(UIADD+IA3-1)
     1                           - MEMD(UIADD+IA2-1)) / WAVE
               MEMR (V2ADD+INTNDX+I-1) = (MEMD(VIADD+IA3-1)
     1                           - MEMD(VIADD+IA2-1)) / WAVE
               MEMR (W2ADD+INTNDX+I-1) = (MEMD(WIADD+IA3-1)
     1                           - MEMD(WIADD+IA2-1)) / WAVE
               IF(EL.LT.ELMIN) THEN
                  MEMR(WTADD+INTNDX+I-1) = -1.0
               ELSE
                  MEMR (WTADD + INTNDX + I - 1) = AUTOW
               END IF
               MEMR (TADD + INTNDX + I - 1) = T
               I = I + 1
  35        CONTINUE
         END IF
C
C Look after shadowing
C
         I = 0
         DO 60 IA1 = 1, NANT-1
            DO 50 IA2 = IA1+1, NANT
               DO 40 IA3 = IA2+1, NANT
                  IF (MEML(SHADD+IA1-1).OR.MEML(SHADD+IA2-1).OR.
     1                MEML(SHADD+IA3-1)) THEN
                     MEMR(WTADD+INTNDX+I-1) = -1.0
                  END IF
                  I = I + 1
  40           CONTINUE
  50        CONTINUE
  60     CONTINUE
         H = H + INTTIME
         T = T + TINT
 100  CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
