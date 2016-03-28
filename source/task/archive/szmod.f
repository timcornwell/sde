C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)szmod.f	1.2 10/16/92
C
      SUBROUTINE SDEMAIN
C
CD Program to make simulated SZ image
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell    Jan 22 1992
C       Modified to give decrement in K based on physical parameters
C       of cluster gas
C                                L. Norton       Sept 6 1992
C------------------------------------------------------------------------
#include          "stdinc.h"
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SZMOD')
C
      REAL                      CELLSIZE(3), SHIFT(3)
      REAL		        RA, DEC, FREQ, T0, N0
      REAL                   WIDTH, BETA, GAMMA
      REAL	        	PI
      PARAMETER                 (PI=3.14159274101257)
      CHARACTER*(SYSMXNAM)	IMGFILE
      INTEGER		        NDUMMY, IMSIZE(3)
      LOGICAL                   NORMALIZE
C==================================================================
      CALL MSGWELCO ('I make an SZ observation')
      CALL USRCTL
C
C Get input parameters
C
      CALL USRGETI('Imsize', IMSIZE, 3, NDUMMY)
      CALL USRGETR('Cellsize', CELLSIZE, 3, NDUMMY)
      CALL USRGETC('Image', IMGFILE, 1, NDUMMY)
      CALL USRGETR('Dec', DEC, 1, NDUMMY)
      CALL USRGETR('Freq', FREQ, 1, NDUMMY)
      CALL USRGETR('T0', T0, 1, NDUMMY)
      CALL USRGETR('N0', N0, 1, NDUMMY)
      CALL USRGETR('Width', Width, 1, NDUMMY)
      CALL USRGETR('Beta', BETA, 1, NDUMMY) 
      CALL USRGETR('Gamma', GAMMA, 1, NDUMMY)
      CALL USRGETL('Norm', NORMALIZE, 1, NDUMMY) 
      CALL USRGETL('Debug',SYSDEBUG,1,NDUMMY) 
C 
      CALL MSGPUT ('Finding model image', 'I')
      CALL IMGOMAKE (DBLE(RA), DBLE(DEC), DBLE(FREQ), CELLSIZE, 
     1   IMSIZE, SHIFT, 'R', 'Image')
C
      CALL SZIMG ('Image', T0, N0, WIDTH, BETA, GAMMA, NORMALIZE) 
C 
C Now write to disk 
C
      IF (IMGFILE.NE.' ') THEN
         CALL FILIMGPU ('Image', IMGFILE, ' ')
      END IF
C
 999  CONTINUE
      END
      SUBROUTINE SZIMG (IMAGE, T0, N0, WIDTH, BETA, GAMMA,
     1                   NORMALIZE)
C
CD Make an image for the SZ effect
C
C	IMAGE	CH*(*)	input	Name of directory entry
C	T0	REAL	input	Temperature 
C       N0      REAL    input    central density
C	Width	REAL	input	Core radius (of galaxies, in asec)
C       Beta    REAL    input   galaxy/gas scale height ratio
C       Gamma   REAL   input   ratio of specific heats for gas
C       Normalize LOGICAL input  whether to normalize to 1.0 at center
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 22 1992
C       Modified to give decrement in K based on physical parameters
C       of cluster gas
C                                L. Norton       Sept 6 1992
C
C---------------------------------------------------------------------
#include         "stdinc.h"
C
C
      CHARACTER*(*)	IMAGE
      REAL		T0, N0
      REAL            WIDTH, BETA, GAMMA
      LOGICAL             NORMALIZE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SZIMG')
C
      REAL		IMRPIX (SYSMXDIM), IMDELT(SYSMXDIM),
     1			IMROTA (SYSMXDIM)
      DOUBLE PRECISION	IMRVAL(SYSMXDIM)
      CHARACTER*8	IMTYPE(SYSMXDIM)
      INTEGER		IMNAX, IMNAXIS(SYSMXDIM), IMADD, DATADD
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL CRDGET (IMAGE, IMNAX, IMTYPE, IMNAXIS, IMRVAL, IMRPIX, 
     1   IMDELT, IMROTA)
C
      IMADD = DATADD(IMAGE)
C
      IF ((IMNAX.EQ.2).OR.((IMNAX.GE.3).AND.(IMNAXIS(3).EQ.1))) THEN
         CALL SZIMG2D (T0, N0, WIDTH, BETA, GAMMA, MEMR(IMADD), 
     1      IMNAXIS(1), IMNAXIS(2), IMRPIX(1), 3600.0*IMDELT(1), 
     2      IMRPIX(2), 3600.0*IMDELT(2), NORMALIZE)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Only support 2 dimensions')
         GO TO 999
      END IF
C
      CALL DATPUTC (IMAGE, 'BUNIT', 'K', 1)
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
      SUBROUTINE SZIMG2D (T0, N0, WIDTH, BETA, GAMMA, IMAGE, NX, NY,  
     &   REFX, DELTX, REFY, DELTY, NORMALIZE)
C
CD Make an SZ image 
C	T0	REAL	input	Temperature of gas [keV]
C       N0      REAL    input   Central density
C	WIDTH	REAL	input	Core radius in arcsec
C       BETA    REAL    input   galaxy/gas scale height ratio
C       GAMMA   REAL    input   ratio of specific heats of gas
C	IMAGE	REAL	output	Output image
C	NX, NY	INT	input	Number of pixels on x, y axes
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in X
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 22 1992
C       Modified to give decrement in K based on physical parameters
C       of cluster gas
C                                L. Norton       Sept 6 1992
C---------------------------------------------------------------------
#include "stdinc.h"
C
C
      INTEGER		NX, NY
      REAL		T0, N0
      REAL              WIDTH, BETA, GAMMA, B
      REAL		REFX, REFY, DELTX, DELTY, IMAGE(NX, *)
      LOGICAL           NORMALIZE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'SZIMG2D')
C
      INTEGER		IX, IY
      REAL	PI, ASEC2CM, NORM
      PARAMETER	(PI=3.14159274101257)
      REAL		DX, DY, R, POWER, FACTOR, KEV2K
      REAL              LOOKUP, TABLE(1000,2)
      REAL              START_B, STOP_B
      INTEGER           NUM_POINTS, NUM_BS
      PARAMETER    (NUM_POINTS = 32)
      PARAMETER     (FACTOR = -2.245E-34)
C  some numerical constants, like Thompson cross section etc, in cgs.
      PARAMETER     (KEV2K =  1.1605E+7)
C  conversion from keV to K for gas central temperature.
      PARAMETER      (ASEC2CM = 3.65E+22)
C  this is 4.85e-6 rad per asec times 2440 Mpc (Ho=50)
C    times 3.086e18 cm per pc, for cluster at z=0.54
      REAL           INTEGRATE_SZ
      REAL           GAMMASTUFF
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (DELTX.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in X')
         GO TO 999
      END IF
      IF (DELTY.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero increment in Y')
         GO TO 999
      END IF
      IF (WIDTH.EQ.0.0) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Zero core radius')
         GO TO 999
      END IF
C
C  if gamma=1 (isothermal) can use a formula to calculate SZ effect,
C  otherwise need to do numerical integration to make a lookup table.
C
      IF (GAMMA .EQ. 1) THEN
         POWER = 0.5 - (3.0 * BETA/2)
         IF (NORMALIZE) THEN
            NORM = 1.0
         ELSE 
            NORM = 2.7 * FACTOR * T0*KEV2K * N0 
     1            * SQRT(PI) * WIDTH*ASEC2CM * GAMMASTUFF(POWER)
         END IF
         DO 11 IY = 1, NY
            DY = (FLOAT(IY) - REFY) * DELTY
            DO 10 IX = 1, NX
               DX = (FLOAT(IX) - REFX) * DELTX
               R = (DX**2+DY**2)/WIDTH**2
               IMAGE(IX, IY) = NORM * (1.0+R)**POWER
 10         CONTINUE
 11      CONTINUE
      ELSE  
         START_B = 0.0
         STOP_B = SQRT((NX*DELTX/2)**2 + (NY*DELTY/2)**2) + MAX(DELTX,
     1                  DELTY)
         NUM_BS = 0.5 * MAX((NX/2), (NY/2))
         IF (NORMALIZE) THEN
            NORM = 1.0/ INTEGRATE_SZ(NUM_POINTS,GAMMA,BETA,WIDTH,0.0,
     1                                ASEC2CM) 
         ELSE 
              NORM = 2.7 * FACTOR * T0*KEV2K * N0 * 2
         END IF
         CALL MAKETABLE(NORM,GAMMA, BETA, WIDTH, START_B, STOP_B,
     1                  NUM_BS, NUM_POINTS, TABLE, ASEC2CM) 
         DO 13 IY = 1, NY
            DY = (FLOAT(IY) - REFY) * DELTY
            DO 12 IX = 1, NX
                DX = (FLOAT(IX) - REFX) * DELTX
                B = SQRT(DX**2 + DY**2)
                IMAGE(IX, IY) = LOOKUP(TABLE, B, NUM_BS)
 12         CONTINUE
 13      CONTINUE
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
C
      SUBROUTINE MAKETABLE(NORM, GAMMA, BETA, WIDTH, START_B, STOP_B,
     1                     NUM_BS, NUM_POINTS, TABLE, ASEC2CM)
C
C Make a lookup table for interpolation of SZ effect
C        GAMMA      REAL    input   gas ratio of specific heats
C        NORM      REAL             makes SZ effect come out in right units
C        BETA       REAL    input   galaxy/gas scale height ratio
C        WIDTH      REAL    input   core radius of galaxies (asec)
C        START_B    REAL            minimum distance to cluster center in table
C        STOP_B     REAL            maximum
C        NUM_BS     INTEGER         number of entries in lookup table
C        NUM_POINTS INTEGER      how many intervals in the numerical integration
C        TABLE      REAL             the lookup table with SZ effect
C        ASEC2CM    REAL            converts from asec to cm for z=0.54
C---------------------------------------------------------
#include "stdinc.h"
C
C
      REAL          GAMMA, BETA, WIDTH, START_B, STOP_B, NORM
      REAL          ASEC2CM
      REAL          DELTA_B	
      INTEGER       NUM_BS, I, NUM_POINTS
      REAL          TABLE(1000,2)
      REAL          INTEGRATE_SZ
C===========================================================================
C
      DELTA_B = ABS(STOP_B - START_B) / (NUM_BS - 1.0)
      DO 20 I = 0, (NUM_BS-1)
         TABLE(I, 1) = DELTA_B * I
         TABLE(I, 2) = NORM * INTEGRATE_SZ(NUM_POINTS, GAMMA, BETA,
     1                    WIDTH, (TABLE(I,1)), ASEC2CM)  
 20   CONTINUE
      END
C
      REAL FUNCTION LOOKUP(TABLE, B, NUM_BS)
C    TABLE   REAL  input   The lookup table in which we'll find SZ effect
C    B       REAL  input   Distance from cluster center (asec)
C    NUM_BS  INTEGER  input   The number of entries in lookup table
C---------------------------------------------------------------------------
      REAL TABLE(1000,2), B
      INTEGER       I, NUM_BS
C==========================================================================
      DO 30 I=0, (NUM_BS-1)
         IF (TABLE(I,1) .EQ. B) THEN 
               LOOKUP = TABLE(I,2)
               GOTO 40
         ELSE IF ((B .GT .TABLE(I,1)) .AND. 
     1            (B .LT. TABLE(I+1,1))) THEN
               X = (B - TABLE(I,1)) / (TABLE(I+1,1) - TABLE(I,1))
               LOOKUP = TABLE(I,2) +((TABLE(I+1,2)-TABLE(I,2))*X)
               GOTO 40
         END IF
 30   CONTINUE
 40   RETURN 
      END
