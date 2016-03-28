C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdeusiz.f	1.4 7/18/97
C
      SUBROUTINE GRDEUSIZ (UADDI, VADDI, WADDI, WTADDI, NVISI,
     $   URI, VRI, SHIFTI, BS)
C
CD Estimate optimum bin size for 2-D ungridded weighting
C
C	UADDI	INT		input	Coordinates of data (address)
C	VADDI	INT		input	Coordinates of data
C	WADDI	INT		input	Coordinates of data
C	WTADDI	INT		input	Data weights
C	NVISI	INT		input	Number to be weighted
C	URI	REAL		input	Weighting radius in u
C	VRI	REAL		input	Weighting radius in v
C	SHIFT	DBLE(3,3)	input	Rotation matrix
C	BS	INT		in/out	Bin Size
C
C This routine has more or less been superceded by GRDEUSZ0, since the
C sophisticated attempt to analyze the runtime was no more successful
C that the crude (and much faster) estimate from GRDEUSZ0.  But one day
C this may again be useful.
C
C If BS is zero, the routine will use compiled in limits and starting
C guess.  If it is negative, the absolute value is the maximum allowed
C bin size.  If it positive, it is used as an initial guess at an
C optimum size.
C
C The constants in the tables below were determined by direct fit to
C timing runs on various machines.  They probably won't vary all *that*
C much from machine to machine.  This whole mess is pretty heuristic
C and approximate.  The results of this routine do not effect the actual
C output of the task, only the running time.
C
C If it looks like things aren't converging in a reasonable amount of
C time, we blow it off and fly with a default.
C
C Audit trail:
C	Original version
C				D.S.Briggs	Sept 28 1993
C	Minor cleaning to elim compiler warnings; no functional mods.
C				M. Stupar	Dec 28 1994
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	NVISI, BS, UADDI, VADDI, WADDI, WTADDI
      REAL	URI, VRI
      DOUBLE  PRECISION	SHIFTI (3,3)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER	(ROUTINE = 'GRDEUSIZ')
C
      INTEGER	MAXTRY
      PARAMETER	(MAXTRY=5)
C
      INTEGER	PMAXBS
      PARAMETER (PMAXBS = 2000)
C
      INTEGER	MINBS
      PARAMETER	(MINBS = 100)
C
C Common for communication with the minimization subroutine
C
      REAL C0, D1, D2, D3, C3S, C4
      REAL UR, VR, UMAX, VMAX, FILL
      INTEGER	NVIS, UADD, VADD, WADD, WTADD
      DOUBLE PRECISION SHIFT(3,3)
      COMMON /GRDEUCOM/ C0, D1, D2, D3, C3S, C4, SHIFT,
     $   UR, VR, UMAX, VMAX, FILL, NVIS, UADD, VADD, WADD, WTADD
C
      INTEGER	MAXBS, I, J
      REAL	GA, GB, GC, GMIN, TA, TB, TC
C
      EXTERNAL	GRDERT, GRDECNST
C
      REAL	GRDERT
      LOGICAL	DATEXIST
C==========================================================================
      IF (ERROR) GO TO 999
C
      UADD = UADDI
      VADD = VADDI
      WADD = WADDI
      WTADD = WTADDI
      UR = URI
      VR = VRI
      NVIS = NVISI
      DO 20 J = 1,3
         DO 10 I = 1, 3
            SHIFT(I,J) = SHIFTI(I,J)
 10      CONTINUE
 20   CONTINUE
C
      IF (BS.LT.0) THEN
         MAXBS = ABS(BS)
      ELSE
         MAXBS = PMAXBS
      END IF
C
      GC = MAXBS
      TC = GRDERT(GC)
      GA = MINBS
      TA = GRDERT(GA)
C
      IF (BS.GT.0) THEN
         GB = BS
         TB = GRDERT(TB)
         IF ((TB.GE.TA).OR.(TB.GE.TC)) THEN
            CALL MSGPUT ('Initial guess not a minimum','W')
            BS = -1
         END IF
      END IF
C
      IF (BS.LE.0) THEN
         IF (TA.LT.TC) THEN
            GB = GA + .01 * (GC-GA)
         ELSE
            GB = GC + .01 * (GA-GC)
         END IF
         TB = GRDERT(GB)
      END IF
C
      IF (TA.LE.TB) THEN
         BS = NINT(GA)
         WRITE (MESSAGE, 1020) BS
 1020    FORMAT ('Using minimum allowed bin size of ',I5)
         CALL MSGPUT (MESSAGE, 'W')
         GO TO 990
      END IF
C
      IF (TC.LE.TB) THEN
         BS = NINT(GC)
         WRITE (MESSAGE, 1030) BS
 1030    FORMAT ('Using maximum allowed bin size of ',I5)
         CALL MSGPUT (MESSAGE, 'W')
         GO TO 990
      END IF
C
      CALL GRDBRENT (GA, GB, GC, TB, GRDERT, .05, GMIN)
C
 700  BS = MIN(MAX(NINT(GMIN), MINBS), MAXBS)
      WRITE (MESSAGE, 1700) BS
 1700 FORMAT ('Estimated optimum binning gridsize is ',I5)
      CALL MSGPUT (MESSAGE, 'I')
C
      IF (DATEXIST('GRDERT-TMP')) CALL DATDELET ('GRDERT-TMP')
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  RETURN
      END
C
      BLOCK DATA GRDECNST
C
C Compiled in machine constants go here
C
C Machine dependent timing constants
C
      REAL C0, D1, D2, D3, C3S, C4
      REAL UR, VR, UMAX, VMAX, FILL
      INTEGER	NVIS, UADD, VADD, WADD, WTADD
      DOUBLE PRECISION SHIFT(3,3)
      COMMON /GRDEUCOM/ C0, D1, D2, D3, C3S, C4, SHIFT,
     $   UR, VR, UMAX, VMAX, FILL, NVIS, UADD, VADD, WADD, WTADD
C				determined on a Sun IPX
#ifdef COMP_SUN
      DATA C0, D1, D2, D3, C3S, C4 /-1.54020E-04, 5.06539E-07,
     $   2.59272E-04, -4.93185E-04, 6.96708E-04, 2.47103E-05/
#else
C				default to Sun set
      DATA C0, D1, D2, D3, C3S, C4 /-1.54020E-04, 5.06539E-07,
     $   2.59272E-04, -4.93185E-04, 6.96708E-04, 2.47103E-05/
#endif
C
      END
C
      REAL FUNCTION GRDERT (GRIDSIZE)
#include	"stdinc.h"
      REAL GRIDSIZE
C
C Return estimated running time as a function of gridsize and everything
C else.  This is passed to a 1-D function minimizer.
C
      REAL C0, D1, D2, D3, C3S, C4
      REAL UR, VR, UMAX, VMAX, FILL
      INTEGER	NVIS, UADD, VADD, WADD, WTADD
      DOUBLE PRECISION SHIFT(3,3)
      COMMON /GRDEUCOM/ C0, D1, D2, D3, C3S, C4, SHIFT,
     $   UR, VR, UMAX, VMAX, FILL, NVIS, UADD, VADD, WADD, WTADD
C
      REAL URP, VRP, URM, VRM, DU, DV, U0, V0, K, PIN, POUT,
     $   PI4, PI, ROOTPI, NUV, ERT, L
C
      INTEGER	NU, NV, TADD
C
      INTEGER	ARRNPIX, DATADD
      LOGICAL	DATEXIST
C==========================================================================
      NV = NINT(GRIDSIZE)
      NU = NV / 2
      NV = NU * 2
C
C Calculating the filling factor is the expensive bit
C
      IF (DATEXIST('GRDERT-TMP')) THEN
         IF (ARRNPIX('GRDERT-TMP').LT.NU*NV) THEN
            CALL DATDELET ('GRDERT-TMP')
            CALL DATMAKAR ('GRDERT-TMP', 1, NINT(1.2*NU*NV), 'I', TADD)
         ELSE
            TADD = DATADD ('GRDERT-TMP')
         END IF
      ELSE
         CALL DATMAKAR ('GRDERT-TMP', 1, NINT(1.2*NU*NV), 'I', TADD)
      END IF
C
      CALL GRDEUFIL (MEMR(TADD), NU, NV, MEMR(UADD), MEMR(VADD),
     $   MEMR(WADD), MEMR(WTADD), NVIS, SHIFT, FILL, UMAX, VMAX)

      PI4 = ATAN(1.D0)
      PI = PI4 * 4.D0
      ROOTPI = SQRT(PI)
C
      DU = UMAX / (GRIDSIZE/2 - 0.5)
      DV = VMAX / (GRIDSIZE/2 - 0.5)
      NUV = (2.0*UR/DU+1)*(2.0*VR/DV+1)
      URP = UR + DU
      URM = UR - DU
      VRP = VR + DV
      VRM = VR - DV
      L = NVIS / (FILL * GRIDSIZE**2 / 2)
C      
      IF ((UR .GT. DU) .AND. (VR .GT. DV)) THEN
         PIN = PI4 * URM * VRM / (UR * VR)
      ELSE
         PIN = 0.0
      END IF
C      
      IF ((UR/URP)**2 + (VR/VRP)**2 .LE. 1.0) THEN
         POUT = 0.0
      ELSE
C			area of sector above and below box
         U0 = URP*SQRT(1-(VR/VRP)**2)
         V0 = VR
         K = VRP/URP
         POUT = 2*(ATAN(K*U0/V0)*(K*U0**2 + V0**2/K) - U0*V0)
C			area of sector on either side of box
         U0 = UR
         V0 = VRP*SQRT(1-(UR/URP)**2)
         K = URP/VRP
         POUT = POUT +
     $      2*(ATAN(K*V0/U0)*(K*V0**2 + U0**2/K) - U0*V0)
C			area of excluded region in box
         POUT = 4*UR*VR - PI*URP*VRP + POUT
C			normalized to a probability
         POUT = POUT / (4*UR*VR)
      END IF
C      
      IF ((POUT .GT. 1.0) .OR. (POUT .LT. 0.0)) THEN
         PAUSE 'POUT is stuffed'
      END IF
C      
      ERT = C0 * NVIS + (D1 + FILL*D2 + FILL*PIN*D3 +
     $   FILL*POUT*C3S + FILL*(1.0-PIN-POUT)*L*C4) * NVIS * NUV
C
      IF (SYSDEBUG) THEN
         WRITE (MESSAGE, 1000) FILL, GRIDSIZE, ERT
 1000    FORMAT ('Fill = ',F8.5, '  BS = ',F10.3,'  ERT = ',F10.3)
         CALL MSGPUT (MESSAGE, 'I')
      END IF
C
      GRDERT = ERT
      RETURN
      END
C
      SUBROUTINE GRDEUFIL (H, NU, NV, U, V, W, WT, NVIS, SHIFT, FILL,
     $   UMAX, VMAX)
C
C Find the filling factor for a given grid
C
C The main loop is a much slimmed down version of that found in
C grduruw2.f
C
C------------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER	NU, NV, H(NU, NV), NVIS
      REAL	WT(NVIS), U(NVIS), V(NVIS), W(NVIS), FILL, UMAX, VMAX
      DOUBLE  PRECISION	SHIFT (3,3)
C
      REAL	USCALE, VSCALE, U0, V0, UCELL, VCELL
      INTEGER	I, J, NV2, UBIN, VBIN, IVIS, IFILL
      LOGICAL	DOSHIFT
C
      INTEGER	UTLGINT
C------------------------------------------------------------------------
      NV2 = NV / 2
      NV = NV2 * 2
      DO 6 VBIN = 1, NV
         DO 5 UBIN = 1, NU
            H(UBIN,VBIN) = 0
 5       CONTINUE
 6    CONTINUE
C
C Is there a shift?
C
      DOSHIFT = (SHIFT(1,1).NE.1.0D0).OR.(SHIFT(2,2).NE.1.0D0).OR.
     1   (SHIFT(3,3).NE.1.0D0)
C
C Find min & max of possibly shifted coordinates
C
      UMAX = 0.0
      VMAX = 0.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
         IF (DOSHIFT) THEN
            U0 = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            V0 = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
         ELSE
            U0 = U(IVIS)
            V0 = V(IVIS)
         END IF
         IF (ABS(U(IVIS)).GT.UMAX) UMAX = ABS(U(IVIS))
         IF (ABS(V(IVIS)).GT.VMAX) VMAX = ABS(V(IVIS))
 50   CONTINUE
C
      USCALE = (NV/2 - 0.5) / UMAX
      VSCALE = (NV/2 - 0.5) / VMAX
C
C Start of loop elements to be binned
C
      DO 100 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 100
         IF (DOSHIFT) THEN
            U0 = SHIFT(1,1) * U(IVIS) + SHIFT(2,1) * V(IVIS) +
     $         SHIFT(3,1) * W(IVIS)
            V0 = SHIFT(1,2) * U(IVIS) + SHIFT(2,2) * V(IVIS) +
     $         SHIFT(3,2) * W(IVIS)
            UCELL = USCALE * U0
            VCELL = VSCALE * V0
         ELSE
            UCELL = USCALE * U(IVIS)
            VCELL = VSCALE * V(IVIS)
         END IF
         IF (UCELL.GE.0.0) THEN
            UBIN = UTLGINT(UCELL) + 1
            VBIN = UTLGINT(VCELL) + NV/2 + 1
         ELSE
            UBIN = UTLGINT(-UCELL) + 1
            VBIN = UTLGINT(-VCELL) + NV/2 + 1
         END IF
         IF ((UBIN.GE.1).AND.(UBIN.LE.NU).AND.(VBIN.GE.1).AND.
     $       (VBIN.LE.NV)) H(UBIN,VBIN) = 1
 100  CONTINUE
C
      IFILL = 0
      DO 120 J = 1, NV
         DO 110 I = 1, NU
            IF (H(I,J).NE.0) IFILL = IFILL + 1
 110     CONTINUE
 120  CONTINUE
C
      FILL = REAL(IFILL) / (NU * NV)
      END

C
C A slightly hacked up version from NR.  Tiny ITMAX, and passed in initial
C function value.
C
      FUNCTION GRDBRENT(AX,BX,CX,FB,F,TOL,XMIN)
      PARAMETER (ITMAX=10,CGOLD=.3819660,ZEPS=1.0E-10)
      EXTERNAL F
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=FB
      FV=FX
      FW=FX
      DO 11 ITER=1,ITMAX
        XM=0.5*(A+B)
        TOL1=TOL*ABS(X)+ZEPS
        TOL2=2.*TOL1
        IF(ABS(X-XM).LE.(TOL2-.5*(B-A))) GOTO 3
        IF(ABS(E).GT.TOL1) THEN
          R=(X-W)*(FX-FV)
          Q=(X-V)*(FX-FW)
          P=(X-V)*Q-(X-W)*R
          Q=2.*(Q-R)
          IF(Q.GT.0.) P=-P
          Q=ABS(Q)
          ETEMP=E
          E=D
          IF(ABS(P).GE.ABS(.5*Q*ETEMP).OR.P.LE.Q*(A-X).OR. 
     *        P.GE.Q*(B-X)) GOTO 1
          D=P/Q
          U=X+D
          IF(U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
          GOTO 2
        ENDIF
1       IF(X.GE.XM) THEN
          E=A-X
        ELSE
          E=B-X
        ENDIF
        D=CGOLD*E
2       IF(ABS(D).GE.TOL1) THEN
          U=X+D
        ELSE
          U=X+SIGN(TOL1,D)
        ENDIF
        FU=F(U)
        IF(FU.LE.FX) THEN
          IF(U.GE.X) THEN
            A=X
          ELSE
            B=X
          ENDIF
          V=W
          FV=FW
          W=X
          FW=FX
          X=U
          FX=FU
        ELSE
          IF(U.LT.X) THEN
            A=U
          ELSE
            B=U
          ENDIF
          IF(FU.LE.FW .OR. W.EQ.X) THEN
            V=W
            FV=FW
            W=U
            FW=FU
          ELSE IF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
            V=U
            FV=FU
          ENDIF
        ENDIF
11    CONTINUE
3     XMIN=X
      GRDBRENT=FX
      RETURN
      END
