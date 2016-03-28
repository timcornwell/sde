C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visrrwtp.f	1.1	 6/28/94
C
      SUBROUTINE VISRRWTP (WT, U, V, NVIS, RADIUS,
     $     DENSITY, NRAD, SHAPE)
C
CD Radial visibility reweighting, pixel level
C
C	WT	R(*)	in/out	WT of each vis
C	U	R(*)	in	UU
C	V	R(*)	in	VV
C	NVIS	INT	in	Number of WT, U, V
C	RADIUS	R(*)	in	Radial Density function, R
C	DENSITY	R(*)	in	Radial Density Function, N(R)
C	NRAD	INT	in	Number of items in the N(R) arrays
C	SHAPE	R(2)	in	SHAPE of N(r): S(1)=Baj/Bmin; S(2)=PA of Bmaj
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Hodlaway	 June 9, 1994
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      REAL		WT(*), U(*), V(*), RADIUS(*), DENSITY(*)
      REAL		SHAPE(2)
      INTEGER		NVIS, NRAD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISRRWTP')      
C
      INTEGER		IV, IR
      REAL		DELT, UV, GLOBALWT, DENS, D2R
      REAL		COSTHETA, SINTHETA, UP, VP
C=====================================================================
      IF (ERROR) GOTO 999
      D2R = ATAN2(1.0, 1.0)/45.0
C
C Loop through all visibilities, finding the density which is closest
C
      DELT = RADIUS(3) - RADIUS(2)
      COSTHETA = COS(D2R*SHAPE(2)-90.0)
      SINTHETA = SIN(D2R*SHAPE(2)-90.0)
      DO 500 IV = 1, NVIS
         IF (WT(IV) .LE. 0.0) GOTO 500
         UP =  COSTHETA *(- U(IV)) + SINTHETA * V(IV)
         VP = -SINTHETA *(- U(IV)) + COSTHETA * V(IV)
         UV = SQRT((UP/SHAPE(1))**2 + VP**2)
         IR = INT(UV /DELT) + 1
         IF (IR .GT. NRAD) IR = NRAD
         IF (DENSITY(IR) .GT. 0.0) THEN
            DENS = DENSITY(IR)
         ELSE IF (IR .LT. NRAD .AND. DENSITY(IR+1) .GT. 0.0) THEN
            DENS = DENSITY(IR+1)
         ELSE IF (IR .GT. 1 .AND. DENSITY(IR-1) .GT. 0.0) THEN
            DENS = DENSITY(IR-1)
         ELSE
            CALL ERRREPOR(ERRLOGIC, ROUTINE, 
     $           'Dividing by zero density')
            GOTO 999
         ENDIF
         GLOBALWT = 1.0 / DENS
         WT(IV) = WT(IV) * GLOBALWT
 500  CONTINUE
C
 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
