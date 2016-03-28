C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcsfrmv.f	1.1    1/28/93
C
      SUBROUTINE IPCSFRMV (XPIXEL, YPIXEL, NPHOTONS, LOOKUP, SIZE, 
     $     METHOD)
C
CD Move the photons in the detector to compensate for wavelength
C
C	XPIXEL	REAL	INPUT   X location of all the photons
C	YPIXEL	REAL	INPUT   Y location of all the photons
C       NPHOTONS INT    INPUT   Size of the above arrays
C	LOOKUP  REAL    INPUT   Lookup Table
C       SIZE(2) INT     INPUT   Size of Lookup Table
C       METHOD  CHAR    INPUT   LInear or Nearest neighbour (L or N)
C
C Audit trail:
C      Cloned from ipcssort
C                              R.G. Marson     Feb 11 1991
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	METHOD
      INTEGER           SIZE(2), NPHOTONS
      REAL          	LOOKUP(SIZE(1), SIZE(2))
      REAL              XPIXEL(NPHOTONS), YPIXEL(NPHOTONS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCSFRMV')
C
C Function Declerations
C

C
C Local Declerations
C
      REAL              VALUE, DICE, MINPIX, MAXPIX
      INTEGER           I, SEED, XP, YP, SV
C
C==================================================================
C
      IF (ERROR) GO TO 999
C
C Start looping straight away
C
      MINPIX = YPIXEL(1)
      MAXPIX = MINPIX
      IF (METHOD(1:1).EQ.'N') THEN
         DO I = 1, NPHOTONS
            XP = NINT(XPIXEL(I)) + 1
            YP = NINT(YPIXEL(I)) + 1
            IF ((XP.GT.0).AND.(XP.LE.SIZE(1)).AND.
     $           (YP.GT.0).AND.(YP.LE.SIZE(2))) THEN
               YPIXEL(I) = FLOAT(NINT(LOOKUP(XP, YP)))
               MINPIX = MIN(MINPIX, YPIXEL(I))
               MAXPIX = MAX(MAXPIX, YPIXEL(I))
            END IF
         END DO
      ELSE
         SEED = 5
         DO I = 1, NPHOTONS
            XP = NINT(XPIXEL(I)) + 1
            YP = NINT(YPIXEL(I)) + 1
            IF ((XP.GT.0).AND.(XP.LE.SIZE(1)).AND.
     $           (YP.GT.0).AND.(YP.LE.SIZE(2))) THEN
               CALL UTLRAND(DICE, SEED)
               VALUE = LOOKUP(XP, YP)
               SV = SIGN(1., VALUE)
               IF (DICE.LT.ABS(MOD(VALUE,1.))) THEN
                  YPIXEL(I) = FLOAT(INT(VALUE + SV))
               ELSE
                  YPIXEL(I) = FLOAT(INT(VALUE))
               END IF
               MINPIX = MIN(MINPIX, YPIXEL(I))
               MAXPIX = MAX(MAXPIX, YPIXEL(I))
            END IF
         END DO
      END IF
      DO I = 1, NPHOTONS
         YPIXEL(I) = YPIXEL(I) - MINPIX
      END DO
      SIZE(2) = NINT(MAXPIX - MINPIX + 1)
C
 990  IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
 999  CONTINUE
      END
