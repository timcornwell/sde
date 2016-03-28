C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)vissta0p.f	1.1    12/27/91
C
      SUBROUTINE VISSTA0P (VIS, WT, TIME, BASL, U, V, W, NPTS,
     $   MINANT, MAXANT, MINUV, MAXUV, MINUVW, MAXUVW, MINUU, MAXUU,
     $   MINVV, MAXVV, MINWW, MAXWW, MINTIME, MAXTIME, MINVIS, MAXVIS)
C
CD Find min/max of various quantities in a visibility data set.  (pix level)
C
C	VIS	CMPLX(NPTS)	input	Visibilities
C	WT	REAL(NPTS)	input	Weights
C	TIME	REAL(NPTS)	input	Times
C	BASL	REAL(NPTS)	input	Baselines
C	U,V,W	REAL(NPTS)	input	U, V, and W of visibilities
C	NPTS	INTEGER		input	Number of visibilities
C	MINANT	INT	output	Minimum antenna number
C	MAXANT	INT	output	Maximum antenna number
C	MINUV	REAL	output	Minimum uv radius
C	MAXUV	REAL	output  Maximum uv radius
C	MINUVW	REAL	output	Minimum uvw radius
C	MAXUVW	REAL	output	Maximum uvw radius
C	MINUU	REAL	output	Minimum uu
C	MAXUU	REAL	output	Maximum uu
C	MINVV	REAL	output	Minimum vv
C	MAXVV	REAL	output	Maximum vv
C	MINWW	REAL	output	Minimum ww
C	MAXWW	REAL	output	Maximum ww
C	MINTIME	REAL	output	Minimum time
C	MAXTIME	REAL	output	Maximum time
C	MINVIS	CMPLX	output	Minimum vis (amp)
C	MAXVIS	CMPLX	output	Maximum vis (amp)
C
C Audit trail:
C	New Routine
C				D.S.Briggs	Sept 7 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NPTS
      COMPLEX		VIS(NPTS)
      REAL		WT(NPTS), TIME(NPTS), BASL(NPTS),
     $			U(NPTS), V(NPTS), W(NPTS)
      INTEGER		MINANT, MAXANT
      REAL		MINUV, MAXUV, MINUVW, MAXUVW, MINUU, MAXUU,
     $   		MINVV, MAXVV, MINWW, MAXWW, MINTIME, MAXTIME
      COMPLEX		MINVIS, MAXVIS
C
      INTEGER		I, IA1, IA2
      REAL		AMP, MINAMP, MAXAMP, UV, UVW
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISSTA0P')
C=====================================================================
      IF (ERROR) GO TO 999
C
      MINANT = 10000
      MAXANT = 0
      MINUV = 1.E15
      MAXUV = -1.E15
      MINUVW = 1.E15
      MAXUVW = -1.E15
      MINUU = 1.E15
      MAXUU = -1.E15
      MINVV = 1.E15
      MAXVV = -1.E15
      MINWW = 1.E15
      MAXWW = -1.E15
      MINTIME = 1.E15
      MAXTIME = -1.E15
      MINAMP = 1.E15
      MAXAMP = -1.E15
      MINVIS = (0.,0.)
      MAXVIS = (0.,0.)
C
      DO 100 I = 1, NPTS
         IF (WT(I).GT.0.0) THEN
C
            IF (U(I).GT.MAXUU) MAXUU = U(I)
            IF (U(I).LT.MINUU) MINUU = U(I)
            IF (V(I).GT.MAXVV) MAXVV = V(I)
            IF (V(I).LT.MINVV) MINVV = V(I)
            IF (W(I).GT.MAXWW) MAXWW = W(I)
            IF (W(I).LT.MINWW) MINWW = W(I)
            IF (TIME(I).GT.MAXTIME) MAXTIME = TIME(I)
            IF (TIME(I).LT.MINTIME) MINTIME = TIME(I)
C
            IA1 = NINT(BASL(I)/256.0)
            IA2 = NINT(BASL(I)-FLOAT(256*IA1))
            AMP = ABS(VIS(I))
            UV = SQRT(U(I)*U(I) + V(I)*V(I))
            UVW = SQRT(U(I)*U(I) + V(I)*V(I) + W(I)*W(I))
C
            IF (IA1.GT.MAXANT) MAXANT = IA1
            IF (IA2.GT.MAXANT) MAXANT = IA2
            IF (IA1.LT.MINANT) MINANT = IA1
            IF (IA2.LT.MINANT) MINANT = IA2
C
            IF (AMP.GT.MAXAMP) THEN
               MAXAMP = AMP
               MAXVIS = VIS(I)
            END IF
            IF (AMP.LT.MINAMP) THEN
               MINAMP = AMP
               MINVIS = VIS(I)
            END IF
C
            IF (UV.GT.MAXUV) MAXUV = UV
            IF (UV.LT.MINUV) MINUV = UV
            IF (UVW.GT.MAXUVW) MAXUVW = UVW
            IF (UVW.LT.MINUVW) MINUVW = UVW
         END IF
 100  CONTINUE
C
 999  CONTINUE
      END
