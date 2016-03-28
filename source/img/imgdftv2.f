C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdftv2.f	1.2	 3/26/91
C
      SUBROUTINE IMGDFTV2 (VIS, NVIS, WT, U, V, BL, 
     $   IMAGE, X1IMAGE, NX, NY, REFX, REFY, DELTX, DELTY, VP)
C
CD Pixel-level DFT routine IMG->VIS in 2D, each ant has its own 2-D VP
C
C	VIS	X	output	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C	IMAGE	REAL	input	Input image
C	X1IMAGE	X	scratch	X scratch array
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	VP	X	input	A bloody voltage pattern for every bloody antenna
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Feb 28 1991
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY
      REAL		BL(*)
      COMPLEX		VIS(*), X1IMAGE(NX, *)
      COMPLEX		VP (NX, NY, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTV2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      INTEGER		IVP1, IVP2, IA1, IA2
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		PHASE
C
      REAL	PI, CF, PSCL
      PARAMETER	(PI=3.14159274101257)
C
      REAL	COST, SINT, X
C=====================================================================
      COST(X) = MEMR(CSADD+COFFSET+NCS+NINT(PSCL*MOD(X,2*PI)))
      SINT(X) = MEMR(CSADD+NCS+NINT(PSCL*MOD(X,2*PI)))
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CF =  2. * (4. * ATAN(1.0))**2 / 180.0
C
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      IVP1 = 0
      IVP2 = 0
      DO 100 IVIS = 1, NVIS
         VIS(IVIS) = 0.0
         IF (WT(IVIS).LE.0.0) GO TO 100
C
C Load VP(IA1) * IMAGE into X1IMAGE if IA1 changes
C
         IA1 = NINT (BL(IVIS)/256.)
         IA2 = NINT (BL(IVIS) - FLOAT(256*IA1))
         IF (IA1 .NE. IVP1) THEN
            IVP1 = IA1
            DO 30 IY = 1, NY
               DO 20 IX = 1, NX
                  X1IMAGE(IX, IY) = VP(IX, IY, IA1) * IMAGE(IX, IY)
 20            CONTINUE
 30         CONTINUE
         ENDIF

C
         IF((U(IVIS).EQ.0.0).AND.(V(IVIS).EQ.0.0)) THEN
            DO 32 IY = 1, NY
               DO 31 IX = 1, NX
                  VIS(IVIS) = VIS(IVIS) + X1IMAGE(IX, IY) * 
     $               CONJG ( VP(IX, IY, IA2) )
 31            CONTINUE
 32         CONTINUE
         ELSE
            DO 35 IX = 1, NX
               PHASE = CF * U(IVIS) * DELTX * (FLOAT(IX) - REFX)
               ROTX(IX) = CMPLX(COST(PHASE), SINT(PHASE))
  35        CONTINUE
            DO 37 IY = 1, NY
               PHASE = CF * V(IVIS) * DELTY * (FLOAT(IY) - REFY)
               ROTY(IY) = CMPLX(COST(PHASE), SINT(PHASE))
  37        CONTINUE
            DO 50 IY = 1, NY
               DO 40 IX = 1, NX
                  VIS(IVIS) = VIS(IVIS) + X1IMAGE(IX, IY) * 
     $               CONJG (VP(IX, IY, IA2)) * ROTX(IX) * ROTY(IY)
 40            CONTINUE
 50         CONTINUE
         ENDIF
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

