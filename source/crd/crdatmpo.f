C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdatmpo.f	1.3    9/15/94
C
      SUBROUTINE CRDATMPO  (ATMOS, VIS)
C
CD Find position in atmosphere From antenna position, az, el, time, wind 
C  velocity.  1/SIN(EL) is applied.
C  Results are written to arrays hanging off of the VIS directory:
C
C	ATMOS/AtmoX	IX pixel on the atmosphere
C	ATMOS/AtmoY	IY pixel on the atmosphere
C	ATMOS/AtmoW	Value of Water at IX, IY (1/SIN(EL) is applied)
C
C	ATMOS	CH*(*)	input	Name of atmosphere
C	VIS	CH*(*)	input	Name of Visibility Dataset
C
C
C
C  These three subroutines are used in conjunction with each other and
C  are somewhat confusing until you understand what is going on:
C
C  CRDATMSH will initialize the atmosphere with respect to the visibility
C  set and should be run once. 
C 
C  CRDATMPO will use the times of the Visibility
C  measurements and the Atmospheric coordinates, wind velocity, and pixel
C  values to create an array of Water Vapor column density along each
C  AZ-EL line of site.  This needs to be run only once.
C
C  VISATMOS is passed the Vis data and the Atmosphere with this Water array
C  tacked on and will read the water vapor off, convert it to a phase, and
C  apply it to each visibility, looping through integration times.
C
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	May 10 1991
C	Reorganized to do it all at once rather than one at a time
C				M.A.Holdaway	May 13 1991
C	Formerly: averaged the water at the beginning and end of an 
C       integration time.
C	NOW: ATMOMAKE does "preaveraging", so we just pick off a pixel
C	value at one time per integration.
C				M.A.Holdaway	July 19 1991
C	Get T0 from the reference time in the ATMOS header, added documentation
C				M.A. Holdaway	Sept 14 1994
C---------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	ATMOS, VIS
C
      CHARACTER*(*)     ROUTINE
      PARAMETER		(ROUTINE = 'CRDATMPO')
C
      REAL		EL, AZ, RX, RY, RZ, TIME, TIMESEC
      REAL		WATER, T0
      REAL		D2R, HEIGHT
      INTEGER		TADD, RXADD, RYADD, RZADD, AXADD, AYADD, AWADD,
     $   		ELADD, AZADD
      INTEGER		IX, IY, IANT, IINT, NANT, NINTS, I
      INTEGER           NDUMMY, ATADD, WATADD
      INTEGER           NAX, NAXIS(SYSMXDIM), SNAX, SNAXIS(SYSMXDIM)
      DOUBLE PRECISION  RVAL(SYSMXDIM)
      REAL              RPIX(SYSMXDIM), DELT(SYSMXDIM), ROTA(SYSMXDIM)
      REAL		OBSX, OBSY
      CHARACTER*8       TYPE(SYSMXDIM)
      CHARACTER*1	ATYPE
C
      REAL		VELOCITY(2)
C
      CHARACTER*(SYSMXNAM)	STRM2
      INTEGER		DATADD
      LOGICAL		DATEXIST
C=====================================================================
      IF(ERROR) GO TO 999
C
      D2R = 4. * ATAN2(1.0, 1.0)/ 180.0
C 
      TADD = DATADD  (STRM2(VIS, 'STIME'))
      RXADD = DATADD (STRM2(VIS, 'RX'))
      RYADD = DATADD (STRM2(VIS, 'RY'))
      RZADD = DATADD (STRM2(VIS, 'RZ'))
      AZADD = DATADD (STRM2(VIS, 'AZ'))
      ELADD = DATADD (STRM2(VIS, 'EL'))
      CALL DATGETAR  (STRM2(VIS, 'EL'), SNAX, SNAXIS, ATYPE, ELADD)
C
      IF (DATEXIST(STRM2(ATMOS, 'AtmoX'))) THEN
         CALL DATDELET (STRM2(ATMOS, 'STIME'))
         CALL DATDELET (STRM2(ATMOS, 'AtmoX'))
         CALL DATDELET (STRM2(ATMOS, 'AtmoY'))
         CALL DATDELET (STRM2(ATMOS, 'AtmoW'))
      ENDIF
      CALL DATMAKAR (STRM2(ATMOS, 'AtmoX'), SNAX, SNAXIS, 'R', AXADD)
      CALL DATMAKAR (STRM2(ATMOS, 'AtmoY'), SNAX, SNAXIS, 'R', AYADD)
      CALL DATMAKAR (STRM2(ATMOS, 'AtmoW'), SNAX, SNAXIS, 'R', AWADD)
      CALL ARRCOPY  (STRM2 (VIS, 'STIME'), STRM2 (ATMOS, 'STIME') )
      NANT = SNAXIS(1)
      NINTS = SNAXIS(2)
      
      IF (ERROR) GOTO 990
C
      CALL DATGETR (ATMOS, 'VELOCITY', VELOCITY, 2, NDUMMY)
      CALL CRDGET (ATMOS, NAX, TYPE, NAXIS, RVAL, RPIX, DELT, ROTA)
      T0 = 0.0
      DO 25 I = 1, NAX
         IF (TYPE(I) .EQ. 'TIME') T0 = RVAL(I)
 25   CONTINUE
      ATADD = DATADD (ATMOS)
      IF (ERROR) GOTO 990
C
      DO 1100 IINT = 0, NINTS - 1
         TIME = MEMR ( TADD + IINT ) - T0
         TIMESEC = TIME * 86400.D0
         DO 1000 IANT = 0, NANT - 1
            RX = MEMR ( RXADD + IANT )
            RY = MEMR ( RYADD + IANT )
            RZ = MEMR ( RZADD + IANT )
            AZ = MEMR ( AZADD + IANT + IINT*NANT )
            EL = MEMR ( ELADD + IANT + IINT*NANT )
C
            HEIGHT = RVAL(3) - RZ
            IF (HEIGHT .LT. 0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE, 
     $              'Atmosphere''s height is < ZERO!')
               GOTO 990
            ENDIF
            IF (ERROR) GOTO 990
C
C Get OBSX on Atmos in Meters
C
            IF (EL .EQ. 90.0) THEN
               OBSX = RX
            ELSE IF (EL .LE. 0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     $            'Elevation is <= ZERO !')
               GOTO 990
            ELSE
               OBSX = RX + HEIGHT * SIN (D2R * AZ) / TAN (D2R * EL)
            ENDIF
C
C Get OBSY on Atmos in Meters
C
            IF (EL .EQ. 90.0) THEN
               OBSY = RY
            ELSE IF (EL .LE. 0.0) THEN
               CALL ERRREPOR (ERRBDARG, ROUTINE,
     $            'Elevation is <= ZERO !')
               GOTO 990
            ELSE
               OBSY = RY + HEIGHT * COS (D2R * AZ) / TAN (D2R * EL)
            ENDIF
C
C Convert OBSX, OBSY into pixel numbers
C
            IX = RPIX(1) + (OBSX - RVAL(1) - TIMESEC 
     $         * VELOCITY(1)) /DELT(1)
            IY = RPIX(2) + (OBSY - RVAL(2) - TIMESEC
     $         * VELOCITY(2)) /DELT(2)
            IF ((IX .GT. NAXIS(1) .OR. IX .LT. 1) .OR.
     $         (IY .GT. NAXIS(2) .OR. IY .LT. 1) ) THEN
               WRITE (MESSAGE, 9182) IX, IY
 9182          FORMAT ('Observed point out of atmosphere: ', 2I12)
               CALL ERRREPOR (ERRBDARG, ROUTINE, MESSAGE)
               GOTO 990
            ENDIF
C
            WATADD = ATADD + IX - 1 + (IY - 1)*NAXIS(1)
            WATER =  ( MEMR ( WATADD ) ) / SIN (D2R * EL)
C            WRITE (MESSAGE, 1111) IX, IY, WATER
C 1111       FORMAT ('Got Atmosphere: ix, iy, water: ',2I8, F10.4)
C            CALL MSGPUT (MESSAGE, 'D')
            MEMR (AXADD + IANT +IINT*NANT ) = IX
            MEMR (AYADD + IANT +IINT*NANT ) = IY
            MEMR (AWADD + IANT +IINT*NANT ) = WATER
C
 1000    CONTINUE
 1100 CONTINUE
C
  990 CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
