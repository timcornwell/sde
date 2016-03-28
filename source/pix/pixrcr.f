C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixrcr.f	1.1    6/21/93
C
      SUBROUTINE PIXRCR (PRIMARY, FRSIZE1, FRSIZE2,
     $     DATA, XAXIS, YAXIS, FRAME, NPHOTONS,
     $     ODATA, OXAXIS, OYAXIS, OFRAME)
C
CD   Centroid one Frame of IPCS Data 
C     
C    PRIMARY    INTEGER INPUT	2D Array with each pixel reperesenting
C                               The frame number of the last photon
C                               to land in that pixel
C    FRSIZE1,2   INTEGER INPUT	The size of the above array
C    DATA       REAL    INPUT   Input Photon Values
C    XAXIS      REAL    INPUT   Input Photon X addresses
C    YAXIS      REAL    INPUT   Input Photon Y addresses
C    FRAME      REAL    INPUT   Input Photon Frame Values
C    NPHOTONS   INTEGER INPUT   SIZE of the above 4 arrays
C    INRANGE    INTEGER INPUT   inclusive range of the above arrays
C    ODATA      REAL    INPUT   Output Photon Values
C    OXAXIS     REAL    INPUT   Output Photon X addresses
C    OYAXIS     REAL    INPUT   Output Photon Y addresses
C    OFRAME     REAL    INPUT   Output Photon Frame Values
C    OUTINDX    INTEGER INPUT   pointer to first free location in 
C                               output arrays
C
C    Audit trail:
C    Cloned from pixrdscat               
C                                    R.G. Marson     Mar 21 1990
C    Rewritten to increase its speed
C                                    R.G. Marson     Mar 21 1992
C    
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C Input Variables
C
      INTEGER      FRSIZE1, FRSIZE2, NPHOTONS
      INTEGER      PRIMARY(0:FRSIZE1, 0:FRSIZE2)
      REAL         DATA(NPHOTONS), XAXIS(NPHOTONS), YAXIS(NPHOTONS)
      REAL         FRAME(NPHOTONS)
      REAL         ODATA(NPHOTONS), OXAXIS(NPHOTONS), OYAXIS(NPHOTONS)
      REAL         OFRAME(NPHOTONS)
C
C Parameter Definitions
C     
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXRCR')
      INTEGER           NORMAL, IGNORE, CENTROID
      PARAMETER         (NORMAL = 5000)
      PARAMETER         (IGNORE = 5001)
      PARAMETER         (CENTROID = 5002)
C
C Function definitions
C

C
C Local Variables
C     
      REAL              Q
      INTEGER		I, STATUS, FRNUM, N, X, Y, F, OPH, IQ, SEED, J
C
      DATA              SEED/17/
      SAVE              SEED
C=====================================================================
C     
C     If an error on input then exit immediately
C     
      IF (ERROR) GO TO 999
C
C Deduce the current frame number
C
      FRNUM = FRAME(1)
      OPH = 0
C
C Go thru the current frame and grid this set of photons
C
      DO I = 1, NPHOTONS
         PRIMARY(NINT(XAXIS(I)), NINT(YAXIS(I))) = FRNUM 
      END DO
C
C Now find out what to do for each photon 
C
      DO I = 1, NPHOTONS
         X = NINT(XAXIS(I))
         Y = NINT(YAXIS(I))
         F = NINT(FRAME(I))
         STATUS = NORMAL
         IF (Y.LT.FRSIZE2) THEN
            IF (PRIMARY(X, Y + 1).EQ.FRNUM) THEN
               GOTO 889
            END IF
         END IF
         IF ((Y - 1).GE.0) THEN
            IF (PRIMARY(X, Y - 1).NE.FRNUM) THEN
               OPH = OPH + 1
               OXAXIS(OPH) = XAXIS(I)
               OYAXIS(OPH) = YAXIS(I)
               OFRAME(OPH) = FRAME(I)
               ODATA(OPH) = DATA(I)
            ELSE
               DO N = 2, FRSIZE2
                  IF((Y - N).GE.0) THEN
                     IF (PRIMARY(X, Y - N).NE.FRNUM) GOTO 888
                  ELSE
                     GOTO 888
                  END IF
               END DO
 888           IF ((N/2)*2.EQ.N) THEN
                  CALL UTLRAND(Q, SEED)
                  IQ = INT(2 * Q)
               ELSE
                  IQ = 1
               END IF
               DO J = 1, N, 2
                  OPH = OPH + 1
                  OXAXIS(OPH) = XAXIS(I)
                  OYAXIS(OPH) = YAXIS(I) + IQ - J
                  OFRAME(OPH) = FRAME(I)
                  ODATA(OPH) = DATA(I)
               END DO
            END IF
         END IF
 889     CONTINUE
C#      IF (STATUS.EQ.NORMAL) THEN 
C#C#            PRINT *, 'Photon at ', X, Y, ' is NORMAL'
C#         ELSE IF(STATUS.EQ.IGNORE) THEN 
C#C#            PRINT *, 'Photon at ', X, Y, ' is IGNORED'
C#         ELSE
C#            PRINT *, 'Photon at ', X, Y, ' is CENTROIDED'
C#            PRINT *, 'SIZE is ', N
C#         END IF
      END DO
      NPHOTONS = OPH
C     
C     Can jump to here if an error found
C     
      IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C     
 999  CONTINUE
      END
