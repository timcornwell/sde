C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrdp.f	1.5    6/21/93
C
      SUBROUTINE PIXRDP (PRIMARY, FRSIZEX, FRSIZEY, 
     $     DATA, XAXIS, YAXIS, FRAME, NPHOTONS,
     $     ODATA, OXAXIS, OYAXIS, OFRAME)
C
CD   Depersist one Frame of IPCS Data 
C     
C    PRIMARY    INTEGER INPUT	2D Array with each pixel reperesenting
C                               The frame number of the last photon
C                               to land in that pixel
C    FRSIZEX,Y  INTEGER INPUT	The size of the above array
C    DATA       REAL    INPUT   Input Photon Values
C    XAXIS      REAL    INPUT   Input Photon X addresses
C    YAXIS      REAL    INPUT   Input Photon Y addresses
C    FRAME      REAL    INPUT   Input Photon Frame Values
C    NPHOTONS   INTEGER INPUT   Size of the above arrays AND
C                               Size of bottom 4 arrays on output
C    ODATA      REAL    INPUT   Output Photon Values
C    OXAXIS     REAL    INPUT   Output Photon X addresses
C    OYAXIS     REAL    INPUT   Output Photon Y addresses
C    OFRAME     REAL    INPUT   Output Photon Frame Values
C
C    Audit trail:
C    Cloned from pixrdscat               
C                                    R.G. Marson     Mar 21 1990
C    Rewritten to increase its speed
C                                    R.G. Marson     Mar 21 1992
C    Cleaned up after writing ipcscr
C                                    R. G. Marson    Dec 21 1992
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C     
      INTEGER      FRSIZEX, FRSIZEY, NPHOTONS
      INTEGER      PRIMARY(0:FRSIZEX, 0:FRSIZEY)
      REAL         DATA(NPHOTONS), XAXIS(NPHOTONS), YAXIS(NPHOTONS)
      REAL         FRAME(NPHOTONS)
      REAL         ODATA(NPHOTONS), OXAXIS(NPHOTONS), OYAXIS(NPHOTONS)
      REAL         OFRAME(NPHOTONS)
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRDP')
C     
      INTEGER		I, X, Y, F, ONPH
      LOGICAL           PERSISTING
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C
C Go thru the current frame and identify which photons are persisting
C
      ONPH = 0
      DO I = 1, NPHOTONS
         X = NINT(XAXIS(I))
         Y = NINT(YAXIS(I))
         F = NINT(FRAME(I)) - 1
         PERSISTING = .FALSE.
C
C Check first for co-incident photons in adjacent frames (most common)
C
         IF((PRIMARY(X,Y)).EQ.F) THEN 
            PERSISTING = .TRUE.
         ELSE 
C
C Check for adjacent photons assuming photon is in center of detector
C
            IF ((X.GT.0).AND.(X.LT.FRSIZEX).AND.
     $          (Y.GT.0).AND.(Y.LT.FRSIZEY)) THEN
               IF((PRIMARY(X-1,Y)).EQ.F) THEN
                  PERSISTING = .TRUE.
               ELSE IF (PRIMARY(X,Y-1).EQ.F) THEN
                  PERSISTING = .TRUE.
               ELSE IF (PRIMARY(X+1,Y).EQ.F) THEN
                  PERSISTING = .TRUE.
               ELSE IF (PRIMARY(X,Y+1).EQ.F) THEN
                  PERSISTING = .TRUE.
               END IF
            ELSE
C
C The edge of the detector needs special treatment
C
               IF (X.GT.0) THEN
                  IF(PRIMARY(X-1,Y).EQ.F) PERSISTING = .TRUE.
               END IF
               IF (Y.GT.0) THEN
                  IF(PRIMARY(X,Y-1).EQ.F) PERSISTING = .TRUE.
               END IF
               IF (X.LT.FRSIZEX) THEN
                  IF(PRIMARY(X+1,Y).EQ.F) PERSISTING = .TRUE.
               END IF
               IF (Y.LT.FRSIZEY) THEN
                  IF(PRIMARY(X,Y+1).EQ.F) PERSISTING = .TRUE.
               END IF
            END IF
         END IF
C
C Only copy non-persisting photons to the output
C
         IF (.NOT.PERSISTING) THEN
            ONPH = ONPH + 1
            ODATA(ONPH) = DATA(I)
            OXAXIS(ONPH) = XAXIS(I)
            OYAXIS(ONPH) = YAXIS(I)
            OFRAME(ONPH) = FRAME(I)
         END IF
      END DO
C
C Now update the primary array to include the current frame
C
      DO I = 1, NPHOTONS
         PRIMARY(NINT(XAXIS(I)), NINT(YAXIS(I))) = 
     $        NINT(FRAME(I))
      END DO
      NPHOTONS = ONPH
C     
C     Can jump to here if an error found
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
