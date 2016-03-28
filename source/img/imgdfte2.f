C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgdfte2.f	1.4    2/4/91
C
      SUBROUTINE IMGDFTE2 (VIS, NVIS, WT, U, V, TIME, BASL, 
     $      RAOFF, DECOFF, NANT, NUMINT, IMAGE, NX, NY, REFX, DELTX, 
     $      REFY, DELTY, OBSRA, OBSDEC, BMX, BMY, NVP, VPARR, RADMAX, 
     $      VP1IMAGE, VP2, IMAGNAME)
C
CD Pixel-level DFT routine IMG->VIS in 2D, applies primary beam before
C transformation
C
C	VIS	CMPLX	output	Visibility function
C	NVIS	INT	input	Number of visibilities
C	WT	REAL	input	Weights
C	U, V	REAL	input	spatial frequencies in waves.
C	TIME	REAL	input	time of visibility measurement
C	BASL	REAL	input	encoded: ANT1 and ANT2
C	RAOFF	REAL	input	pointing error in RA
C	DECOFF	REAL	input	pointing error in DEC
C	NANT	INT	input	Number of antennas
C	NUMINT	INT	input	Number of integrations
C	IMAGE	REAL	input	Input image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	OBSRA	REAL	input	Observed RA
C	BMX	REAL	input	Scaling for X
C	BLANK	REAL	input	Value of blank pixel
C	NVP	INT	input	Length of array for Voltage Pattern
C	VPARR	REAL	input	Antenna voltage pattern
C	RADMAX	REAL	input	Maximum radius allowed for Voltage pattern
C	VP1IMAGE X	-----	Work array: Volt Patt of ant 1 * image
C	VP2	 X	-----	Work Array: Voltage Pattern of ant 2
C	IMAGNAME CH(*)  input	Image NAME (for CRDWTOP)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	Jan 5 1989
C	Altered to deal with VOLTAGE PATTERN instead of PRIMARY BEAM.
C	Takes individual antenna pointing errors into account also
C				M.A.Holdaway	Sep 22 1989
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NVP, NANT, NUMINT
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, BMX, BMY
      DOUBLE PRECISION	OBSRA, OBSDEC
      REAL		VPARR(*), TIME(*), BASL(*), RADMAX
      COMPLEX		VIS(*), VP1IMAGE(NX, *), VP2(NX, *)
      REAL		RAOFF(NANT, *), DECOFF(NANT, *)
      CHARACTER*(*)	IMAGNAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGDFTE2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		YSQ, RAD, RSCL, TAPER, PSCL
      REAL		PHASE, CF, TLAST
      INTEGER		XBEG, XEND, YBEG, YEND, VBEG, VEND
      INTEGER		INTNDX, IA1, IA2, IVP1, IVP2
C
      REAL	PI, WPC(SYSMXDIM), PIXPC(SYSMXDIM)
      PARAMETER	(PI=3.14159274101257)
C
      REAL	COST, SINT, X
      DATA	WPC	/SYSMXDIM*0.0/
      DATA	PIXPC	/SYSMXDIM*0.0/
      DATA	TLAST	/0.0/
C=====================================================================
      COST(X) = MEMR(CSADD+COFFSET+NCS+NINT(PSCL*MOD(X,2*PI)))
      SINT(X) = MEMR(CSADD+NCS+NINT(PSCL*MOD(X,2*PI)))
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      RSCL = FLOAT(NVP-1) / RADMAX
C
      CF = 2. * (4 * ATAN(1.0))**2 / 180.0
C
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      IVP1 = 0
      IVP2 = 0
      VBEG = 0
      VEND = 0
      TLAST = TIME(1)
C
C     the idea: VIS are ordered with many AN1 in a row, so to save
C     time, we calculate VP of AN1 * IMAGE, which can be used over
C     for the correlations with all AN2 (AN2 > AN1).  We must
C     calculate VP of AN2 new each time.
C
      DO 100 INTNDX = 1, NUMINT
         VBEG=VEND+1
         DO 10 IVIS=VBEG,NVIS-1
            IF(TIME(IVIS+1).GT.(TLAST+1.0/86400.0)) THEN
               VEND=IVIS
               GO TO 11
            END IF
 10      CONTINUE
         VEND = NVIS
 11      CONTINUE
         TLAST=TIME(VEND)
         IF (VEND .GT. NVIS) THEN
            WRITE(MESSAGE, '('' VEND and NVIS dont agree: '',
     $         2i7)') VEND, NVIS
            CALL MSGPUT (MESSAGE, 'W')
C           CALL ERRREPOR (ERRLOGIC, ROUTINE, 'VEND and NVIS disagree')
            GOTO 999
         ENDIF
         DO 90 IVIS = VBEG, VEND
            IF (WT(IVIS).LE.0.0) GO TO 90
            VIS(IVIS) = 0.0
            IA1 = NINT (BASL(IVIS)/256.)
            IA2 = NINT (BASL(IVIS) - FLOAT(256*IA1))
            IF (IA1 .NE. IVP1) THEN
C              make the array VP1IMAGE
               WPC(1)  = OBSRA  +  RAOFF(IA1, INTNDX)
               WPC(2) = OBSDEC +  DECOFF(IA1, INTNDX)
               CALL CRDWTOP( IMAGNAME, WPC, PIXPC)
               XBEG = MAX(1,  NINT(PIXPC(1)  - (RADMAX)/SQRT(BMX)))
               XEND = MIN(NX, NINT(PIXPC(1)  + (RADMAX)/SQRT(BMX)))
               YBEG = MAX(1,  NINT(PIXPC(2)  - (RADMAX)/SQRT(BMY)))
               YEND = MIN(NY, NINT(PIXPC(2)  + (RADMAX)/SQRT(BMY)))
               DO 30 IY = YBEG, YEND
                  YSQ = BMY * (FLOAT(IY)- PIXPC(2))**2
                  DO 20 IX = XBEG, XEND
                     RAD = SQRT (YSQ + BMX*(FLOAT(IX)- PIXPC(1))**2)
                     IF (RAD .LE. RADMAX) THEN
                        TAPER = VPARR(1 + NINT(RSCL * RAD))
                        VP1IMAGE(IX,IY) = TAPER * IMAGE(IX,IY)
                     ELSE
                        VP1IMAGE(IX,IY) = (0.,0.)
                     END IF
 20               CONTINUE
 30            CONTINUE
               IVP1 = IA1 
            END IF
            WPC(1)  = OBSRA  +  RAOFF(IA2, INTNDX)
            WPC(2) = OBSDEC +  DECOFF(IA2, INTNDX)
            CALL CRDWTOP( IMAGNAME, WPC, PIXPC)
            DO 50 IY = YBEG, YEND
               YSQ = BMY * (FLOAT(IY)-PIXPC(2))**2
               DO 40 IX = XBEG, XEND
                  RAD = SQRT (YSQ + BMX*(FLOAT(IX)-PIXPC(1))**2)
                  IF (RAD .LE. RADMAX) THEN
                     VP2(IX,IY) = VPARR(1 + NINT(RSCL * RAD))
                  ELSE
                     VP2(IX,IY) = (0.,0.)
                  END IF
 40            CONTINUE
 50         CONTINUE
            IVP2 = IA2 
C
            DO 60 IX = XBEG, XEND
               PHASE = CF * U(IVIS) * DELTX * (FLOAT(IX) - REFX)
               ROTX(IX) = CMPLX(COST(PHASE), SINT(PHASE))
 60         CONTINUE
            DO 65 IY = YBEG, YEND
               PHASE = CF * V(IVIS) * DELTY * (FLOAT(IY) - REFY)
               ROTY(IY) = CMPLX(COST(PHASE), SINT(PHASE))
 65         CONTINUE
            DO 80 IY = YBEG, YEND
               DO 70 IX = XBEG, XEND
                  VIS(IVIS) = VIS(IVIS) + VP1IMAGE(IX,IY) *
     $               CONJG(VP2(IX,IY)) * ROTX(IX) * ROTY(IY)
  70           CONTINUE
  80        CONTINUE
 90      CONTINUE
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
