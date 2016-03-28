C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visdfte2.f	1.1    5/17/91
C
      SUBROUTINE VISDFTE2 (VIS, NVIS, WT, U, V, TIME, BASL, 
     $      RAOFF, DECOFF, NANT, NUMINT, IMAGE, NX, NY, REFX, DELTX, 
     $      REFY, DELTY, OBSRA, OBSDEC, BMX, BMY, NVP, VPARR, RADMAX, 
     $      IMAGNAME)
C
CD Pixel-level DFT routine VIS->IMG in 2D, applies primary beam after
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
C	IMAGNAME CH(*)  input	Image NAME (for CRDWTOP)
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				T.J.Cornwell	May 10 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      INTEGER		NX, NY, NVIS, NVP, NANT, NUMINT
      REAL		IMAGE(NX, *), WT(*), U(*), V(*)
      REAL		REFX, REFY, DELTX, DELTY, BMX, BMY
      DOUBLE PRECISION	OBSRA, OBSDEC
      REAL		VPARR(*), TIME(*), BASL(*), RADMAX
      COMPLEX		VIS(*)
      REAL		RAOFF(NANT, *), DECOFF(NANT, *)
      CHARACTER*(*)	IMAGNAME
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISDFTE2')
C
      INTEGER		IX, IY, IVIS, NCS, COFFSET, CSADD
      COMPLEX		ROTX(1024), ROTY(1024)
      REAL		Y1SQ, Y2SQ, RAD1, RAD2, RSCL, PSCL
      REAL		PHASE, CF, SUMWT, RNORM
      INTEGER		XBEG, XEND, YBEG, YEND, VBEG, VEND
      INTEGER		INTNDX, IA1, IA2
C
      REAL	PI, WPC(SYSMXDIM), PIXPC1(SYSMXDIM), PIXPC2(SYSMXDIM)
      PARAMETER	(PI=3.14159274101257)
C
      REAL	COST, SINT, X
      DATA	WPC	/SYSMXDIM*0.0/
      DATA	PIXPC1	/SYSMXDIM*0.0/
      DATA	PIXPC2	/SYSMXDIM*0.0/
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
      CF = (2.0 * PI) * (PI/180.0)
C
      CALL ARRCS (NCS, CSADD, PSCL, COFFSET)
C
      VBEG = 0
      VEND = 0
C
      DO 2 IY = 1, NY
         DO 1 IX = 1, NX
            IMAGE(IX,IY) = 0.0
 1       CONTINUE
 2    CONTINUE
C
      SUMWT = 0.0
      DO 50 IVIS = 1, NVIS
         IF (WT(IVIS).LE.0.0) GO TO 50
            SUMWT = SUMWT + WT(IVIS)
  50  CONTINUE
      IF (SUMWT.GT.0.0) THEN
         RNORM = 1.0 / SUMWT
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'No valid data')
         GO TO 999
      END IF
C
      DO 100 INTNDX = 1, NUMINT
         VBEG=VEND+1
         DO 10 IVIS=VBEG,NVIS-1
            IF(TIME(IVIS+1).GT.(TIME(VBEG)+1.0/86400.0)) THEN
               VEND=IVIS
               GO TO 11
            END IF
 10      CONTINUE
         VEND = NVIS
 11      CONTINUE
         IF (VEND .GT. NVIS) THEN
            WRITE(MESSAGE, '('' VEND and NVIS do not agree: '',
     $         2I7)') VEND, NVIS
            CALL MSGPUT (MESSAGE, 'W')
C           CALL ERRREPOR (ERRLOGIC, ROUTINE, 'VEND and NVIS disagree')
            GOTO 999
         ELSE
            WRITE (MESSAGE, 1000) INTNDX
 1000       FORMAT ('Integration ',I4)
            CALL MSGPUT (MESSAGE, 'I')
         ENDIF
         DO 90 IVIS = VBEG, VEND
            IF (WT(IVIS).LE.0.0) GO TO 90
            IA1 = NINT (BASL(IVIS)/256.)
            IA2 = NINT (BASL(IVIS) - FLOAT(256*IA1))
            WPC(1)  = OBSRA  +  RAOFF(IA1, INTNDX)
            WPC(2) = OBSDEC +  DECOFF(IA1, INTNDX)
            CALL CRDWTOP( IMAGNAME, WPC, PIXPC1)
            WPC(1)  = OBSRA  +  RAOFF(IA2, INTNDX)
            WPC(2) = OBSDEC +  DECOFF(IA2, INTNDX)
            CALL CRDWTOP( IMAGNAME, WPC, PIXPC2)
            XBEG = MAX(1,  NINT(PIXPC1(1) - (RADMAX)/SQRT(BMX)),
     $                     NINT(PIXPC2(1) - (RADMAX)/SQRT(BMX)))
            XEND = MIN(NX, NINT(PIXPC1(1) + (RADMAX)/SQRT(BMX)),
     $                     NINT(PIXPC2(1) + (RADMAX)/SQRT(BMX)))
            YBEG = MAX(1,  NINT(PIXPC1(2) - (RADMAX)/SQRT(BMY)),
     $                     NINT(PIXPC2(2) - (RADMAX)/SQRT(BMY)))
            YEND = MIN(NY, NINT(PIXPC1(2) + (RADMAX)/SQRT(BMY)),
     $                     NINT(PIXPC2(2) + (RADMAX)/SQRT(BMY)))
            DO 60 IX = XBEG, XEND
               PHASE = CF * U(IVIS) * DELTX * (FLOAT(IX) - REFX)
               ROTX(IX) = WT(IVIS) * VIS(IVIS) * RNORM *
     $            CMPLX(COST(PHASE), -SINT(PHASE))
 60         CONTINUE
            DO 65 IY = YBEG, YEND
               PHASE = CF * V(IVIS) * DELTY * (FLOAT(IY) - REFY)
               ROTY(IY) = CMPLX(COST(PHASE), -SINT(PHASE))
 65         CONTINUE
            DO 80 IY = YBEG, YEND
               Y1SQ = BMY * (FLOAT(IY)- PIXPC1(2))**2
               Y2SQ = BMY * (FLOAT(IY)- PIXPC2(2))**2
               DO 70 IX = XBEG, XEND
                  RAD1 = SQRT (Y1SQ + BMX*(FLOAT(IX)- PIXPC1(1))**2)
                  RAD2 = SQRT (Y2SQ + BMX*(FLOAT(IX)- PIXPC2(1))**2)
                  IF((RAD1.LE.RADMAX).AND.(RAD2.LE.RADMAX)) THEN
                     IMAGE(IX, IY) = IMAGE(IX, IY) +
     $                  VPARR(1 + NINT(RSCL * RAD1)) *
     $                  VPARR(1 + NINT(RSCL * RAD2)) *
     $                  REAL(ROTX(IX) * ROTY(IY))
                  ENDIF
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
