C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visttrpp.f	1.2    11/7/90
C
      SUBROUTINE VISTTRPP (VIS, BASL, TIME, WT, NVIS, TRIPLE, TRPANT,
     $     TTIME, WTT, NEWVIS, NEWWT, NTRP, NANT, NPH, NFRAMES)
C
CD Insert a visibility data set into a triple product data set.
C Add photon noise as appropriate.
C
C     $     TTIME, WTT, NEWVIS, NEWWT, NTRP, NANT, NPH, NFRAMES)
C
C	VIS	CMPLX	input	Input visibilities
C	BASL	REAL(*)	input	Baselines
C	TIME	REAL(*)	input	Times
C	WT	REAL(*)	input	Input weights
C	NVIS	INT	input	Number of visibilities
C	TRIPLE	CMPLX	output	Output triple product
C      TRPANT  REAL    input   Antennas in triple product
C      TTIME   REAL    input   Time of triple product
C      WTT     REAL    input   Weight of triple product
C	NEWVIS	CMPLX	output	Output visibilities (amplitudes only)
C	NEWWT	REAL	output	Output weights
C      NTRP    INT     input   Number of triple product records
C      NANT    INT     input   Number of antennas
C	NPH	REAL	input	Number of photons in image
C	NFRAMES	INTEGER	input	Number of frames per integration
C
C Audit trail:
C      New routine
C				T.J.Cornwell	March 30 1989
C	Added capability to add photon noise before integration
C				T.J. Cornwell	Oct 23 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER		NVIS, NTRP, NANT, NFRAMES
      REAL		BASL(*), TIME(*), WT(*), TRPANT(*),
     $                  WTT(*), TTIME(*), NPH, NEWWT(*)
      COMPLEX		VIS(*), TRIPLE(*), NEWVIS(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISTTRPP')
C
      INTEGER		IVIS, IA1, IA2, IA3, IT
      INTEGER		MAXNANT
      PARAMETER		(MAXNANT = 28)
      COMPLEX		CVIS(MAXNANT,MAXNANT), C
      COMPLEX		PHVIS, NPHVIS(MAXNANT,MAXNANT)
      COMPLEX		ANTGAIN(MAXNANT)
      REAL		CWT(MAXNANT,MAXNANT)
      REAL		TCURRENT, TWOPI, PHASE
      INTEGER		NREAL, NIMAG
      INTEGER		IBEG, IEND, ITBEG, ITEND, SS, PP, IFRAME
      REAL		NPHTOT
      DATA		SS/200001/
      DATA		PP/100001/
C=====================================================================
      TWOPI = 8 * ATAN(1.0)
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C ********************** Start of loop over integrations *************
C
C
C Initialize stuff
C
      DO 5 IT = 1, NTRP
         TRIPLE(IT) = 0.0
         WTT(IT) = -1.0
 5    CONTINUE
      IBEG = 1
      ITBEG = 1
  1   CONTINUE
C
C Initialize accumulators
C
      DO 10 IA2 = 1, NANT
         DO 11 IA1 = 1, NANT
            CVIS(IA1, IA2) = 0.0
            NPHVIS(IA1, IA2) = 0.0
            CWT(IA1, IA2) = 0.0
  11     CONTINUE
  10  CONTINUE
C
C Start loop over all data: first read visibility data until we reach
C the end of an integration, then calculate the corresponding triple
C product data
C
      TCURRENT = TIME(IBEG)
      DO 100 IVIS = IBEG, NVIS
         IEND = IVIS
         IF (TIME(IVIS).GT.TCURRENT) GO TO 120
         IF (WT(IVIS).LE.0.0) GO TO 100
         NEWVIS(IVIS) = 0.0
         NEWWT(IVIS) = 0.0
         IA1 = NINT(BASL(IVIS)/256.0)
         IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))
         CVIS(IA1, IA2) = VIS(IVIS)
         CVIS(IA2, IA1) = CONJG(VIS(IVIS))
         CWT(IA1, IA2) = WT(IVIS)
         CWT(IA2, IA1) = WT(IVIS)
 100  CONTINUE
 120  CONTINUE
C 
C End of integration: put into triple product arrays 
C
C No photon noise so everything is very straightforward
C
      IF((NPH.EQ.0.0).OR.(NFRAMES.EQ.0)) THEN
         DO 180 IT = ITBEG, NTRP
            ITEND = IT
            IF(TTIME(IT).GT.TCURRENT) GO TO 181
            IA1 = NINT(TRPANT(IT))/256**2
            IA2 = NINT(TRPANT(IT)-FLOAT(256**2*IA1))/256.0
            IA3 = NINT(TRPANT(IT)-FLOAT(256**2*IA1+256*IA2))
            TRIPLE(IT) = CVIS(IA1,IA2) * CVIS(IA2,IA3) * CONJG
     $         (CVIS(IA1,IA3))
            WTT(IT) = CWT(IA1,IA2) * CWT(IA2,IA3) * CWT(IA1,IA3)
 180     CONTINUE
 181     CONTINUE
         DO 185 IVIS = IBEG, NVIS
            IF (TIME(IVIS).GT.TCURRENT) GO TO 186
            IF (WT(IVIS).LE.0.0) GO TO 185
            IA1 = NINT(BASL(IVIS)/256.0)
            IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))            
            NEWVIS(IVIS) = ABS(CVIS(IA1,IA2))
            NEWWT(IVIS) = WT(IVIS)
 185     CONTINUE
 186     CONTINUE
      ELSE
C
C Make an estimate of the total number of photons per frame
C
         NPHTOT = 0.0
         DO 189 IFRAME = 1, NFRAMES
            CALL UTLPOISS (NPH, NREAL, PP)
            NPHTOT = NPHTOT + NREAL
 189     CONTINUE
         NPHTOT = NPHTOT / FLOAT(NFRAMES)
C
C Loop through all frames. A frame is related to the coherence time
C of the atmosphere.
C
         DO 192 IFRAME = 1, NFRAMES
C
C Find antenna phase factors. We could add Airy-disk pointing errors
C here as amplitude errors.
C
            DO 196 IA1 = 1, NANT
               CALL UTLRAND (PHASE, SS)
               PHASE = TWOPI * PHASE 
               ANTGAIN(IA1) = CMPLX(COS(PHASE), SIN(PHASE))
 196        CONTINUE
C
C Now add photon noise to each quadrature channel
C
            C = CMPLX(1.0,1.0) * NPH
            DO 193 IA1 = 1, NANT
               DO 194 IA2 = IA1+1, NANT
                  PHVIS = C + NPH * CVIS(IA1,IA2) * ANTGAIN(IA1) *
     $               CONJG(ANTGAIN(IA2))
                  CALL UTLPOISS (REAL(PHVIS),  NREAL, PP)
                  CALL UTLPOISS (AIMAG(PHVIS), NIMAG, PP)
                  NPHVIS(IA1,IA2) = CMPLX(NREAL, NIMAG)
                  NPHVIS(IA2,IA1) = CMPLX(NREAL, -NIMAG)
 194           CONTINUE
 193        CONTINUE
C
C Now form the triple products from the visibilities for each frame
C and average into the complete triple product for this series of
C integrations. Remove bias term at same time
C
            C = CMPLX(1.0,1.0) * NPHTOT
            DO 190 IT = ITBEG, NTRP
               ITEND = IT
               IF(TTIME(IT).GT.TCURRENT) GO TO 191
               IA1 = NINT(TRPANT(IT))/256**2
               IA2 = NINT(TRPANT(IT)-FLOAT(256**2*IA1))/256.0
               IA3 = NINT(TRPANT(IT)-FLOAT(256**2*IA1+256*IA2))
               IF((CWT(IA1,IA2).GT.0.0).AND.(CWT(IA2,IA3).GT.0.0)
     $            .AND.(CWT(IA3,IA1).GT.0.0)) THEN
                  TRIPLE(IT) = TRIPLE(IT) + (NPHVIS(IA1,IA2)-C) *
     $               (NPHVIS(IA2,IA3)-C) * CONJG(NPHVIS(IA1,IA3)-C)
     $            / FLOAT(NFRAMES)
                  WTT(IT) = CWT(IA1,IA2) * CWT(IA2,IA3) *
     $               CWT(IA3,IA1)
                ENDIF
 190        CONTINUE
 191        CONTINUE
C
C Now form estimates for amplitudes, correcting for bias as required
C
            DO 250 IVIS = IBEG, NVIS
               IF (TIME(IVIS).GT.TCURRENT) GO TO 251
               IF (WT(IVIS).LE.0.0) GO TO 250
               IA1 = NINT(BASL(IVIS)/256.0)
               IA2 = NINT(BASL(IVIS)-FLOAT(256*IA1))            
               IF(CWT(IA1,IA2).GT.0.0) THEN
                  NEWWT(IVIS) = NEWWT(IVIS) + ABS(NPHVIS(IA1,IA2))**2
                  NEWVIS(IVIS) = NEWVIS(IVIS) + NPHVIS(IA1,IA2)
               END IF
 250        CONTINUE
 251        CONTINUE
C
C End of loop over frames
C
 192     CONTINUE
C
      END IF
C
C Go back for more?
C
      IBEG = IEND
      ITBEG = ITEND
      IF(IBEG.LT.NVIS) GO TO 1
C
C ********************** End of loop over integrations *****************
C
C
      IF((NPH.NE.0.0).AND.(NFRAMES.NE.0)) THEN
         DO 300 IVIS = 1, NVIS
            NEWVIS(IVIS) = NEWVIS(IVIS) / FLOAT(NFRAMES)
            NEWWT(IVIS) = NEWWT(IVIS) / FLOAT(NFRAMES)
     $         - REAL(NEWVIS(IVIS)) - AIMAG(NEWVIS(IVIS))
     $         - REAL(NEWVIS(IVIS))**2 - AIMAG(NEWVIS(IVIS))**2
            IF (NEWWT(IVIS).GT.0.0) THEN
               NEWVIS(IVIS) = SQRT(NEWWT(IVIS))
               NEWWT(IVIS) = WT(IVIS)
            ELSE
               NEWVIS(IVIS) = 0.0
               NEWWT(IVIS) = -1.0
            END IF
 300     CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
