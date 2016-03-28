C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpbav2.f	1.1	 4/3/91
C
      SUBROUTINE IMGPBAV2 (VP, NANT, AVEPB, NX, NY, REFX, REFY, 
     $   DELTX, DELTY, RADMAX, RCONST, PB, NPB)
C
CD Pixel-level DFT routine IMG->VIS in 2D, each ant has its own 2-D VP
C
C	VP	X	input	A bloody voltage pattern for every bloody antenna
C	NANT	INT	input	Number of antennas
C	AVEPB	REAL	output	Average PB image
C	NX, NY	INT	input	Size of image
C	REFX	REAL	input	Reference pixel
C	DELTX	REAL	input	Increment in x
C	RADMAX	REAL	output	Max radius of Beam
C	RCONST	REAL	input	Scaling constant
C	PB	REAL(*)	out	PB(x) array
C	NPB	INT	input	Size of PB array
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	March 7 1991
C	Changed philosophy:  Get the average of V(i)V(i)*, instead of
C	cross correlations.
C				M.A.Holdaway	April 3 1991
C	This is a bit of a rush job.  Things like NINTERP
C	and NTAKE and NRA could be firmed up a bit to ensure
C	proper sampling always.  Better accuracy can be had
C	by increasing the number of terms in the SINC INTERP
C	(currently at 500)....yielding accuracy of .0005 after
C	10 interpolations.
C				M.A.Holdaway	April 3 1991
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      REAL		REFX, REFY, DELTX, DELTY
      REAL		RADMAX, RCONST, PB(*)
      INTEGER		NX, NY, NPB, NANT
      REAL		AVEPB(NX, *)
      COMPLEX		VP (NX, NY, *)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IMGPBAV2')
C
      INTEGER		IX, IY, I, IXADD, JXADD, IYADD, JYADD, IHADD
      INTEGER		IA1, IA2, N, NINTERP, NFINAL, IFINAL
      INTEGER		NAXIS(2), INDX, NMIS
      REAL		RPIX(2), XDEL, RSCL, RAD
      INTEGER		NRA, NRAM, NCEN, NTAKE, NRA2
      PARAMETER		(NRAM = 200)
      CHARACTER*6	STRINT
      LOGICAL		DATEXIST
      REAL		RAVE(NRAM), PIXEL(NRAM), XMIN, XMAX, YMIN, YMAX
      REAL		RAVE2(NRAM), PIXEL2(NRAM)
      REAL		RAVE3(NRAM), PIXEL3(NRAM)
      REAL		NOTZERO
      PARAMETER		(NOTZERO = .05)
      DATA		RAVE /NRAM * 0.0/
      DATA		PIXEL /NRAM * 0.0/
      DATA		RAVE2 /NRAM * 0.0/
      DATA		PIXEL2 /NRAM * 0.0/
      DATA		RAVE3 /NRAM * 0.0/
      DATA		PIXEL3 /NRAM * 0.0/
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
C A rough guess as to what we need
C
      NTAKE = 10
      NRA = 10 * NTAKE  + 1
      IF (NRA .GT. NRAM) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Increase NRAM')
         GOTO 990
      ENDIF
C
      DO 10 IY = 1, NY
         DO 5 IX = 1, NX
            AVEPB(IX, IY) = 0.0
 5       CONTINUE
 10   CONTINUE
C
      CALL MSGPUT ('Averaging Primary Beams', 'D')
      DO 50 IA1 = 1, NANT
            DO 30 IY = 1, NY
               DO 20 IX = 1, NX
                  AVEPB(IX, IY) = AVEPB(IX, IY) +
     $                VP(IX, IY, IA1) * CONJG (VP(IX, IY, IA1)) 
 20            CONTINUE
 30         CONTINUE
 50   CONTINUE
C
      DO 70 IY = 1, NY
         DO 60 IX = 1, NX
            AVEPB(IX, IY) = AVEPB(IX, IY)/ FLOAT (NANT)
 60      CONTINUE
 70   CONTINUE
C
      NAXIS(1) = NX
      NAXIS(2) = NY
      RPIX(1) = REFX
      RPIX(2) = REFY      
      CALL ARRRAVE (AVEPB, RAVE, PIXEL, NAXIS, RPIX, NRA, 3600.*DELTY)
C
C Sample the radially averaged beam every NTAKE
C
      NRA2 = 0
      DO 73 I = 1, NRA, NTAKE
         NRA2 = NRA2 + 1
         RAVE2(NRA2) = RAVE(I)
         PIXEL2(NRA2) = PIXEL(I)
 73   CONTINUE
C
C Make a mirror image so the interpolation goes "smoothly"
C
      NCEN = NRA2+1
      PIXEL3(NCEN) = PIXEL2(1)
      RAVE3(NCEN) = RAVE2(1)
      DO 80 I = 2, NRA2
         RAVE3(NCEN+(I-1))  =  RAVE2(I)
         PIXEL3(NCEN+(I-1)) =  PIXEL2(I)
         RAVE3(NCEN-(I-1))  =  RAVE2(I)
         PIXEL3(NCEN-(I-1)) = -PIXEL2(I)
 80   CONTINUE
      PIXEL3(1) = PIXEL3(2) - (PIXEL2(3) - PIXEL2(2))
      RAVE3(1)  = 0.0
C
C Figure out how many midpoint interpolations we must do
C We will need the scaling stuff for this!
C
      NINTERP = NINT (LOG(FLOAT(NPB))/LOG(2.0)) - 3
      IF (.NOT.DATEXIST('SCRATCH')) CALL DATCREAT ('SCRATCH')
      N = 2*NRA2
      CALL DATMAKAR ('SCRATCH/InterpY1', 1, 2*N, 'R', IYADD)
      CALL DATMAKAR ('SCRATCH/InterpX1', 1, 2*N, 'R', IXADD)
      CALL PIXSINCI (N, RAVE3, MEMR(IYADD))
      XDEL = (PIXEL3(3) - PIXEL3(2))/2.0
      DO 400 IX = 0, N-1
         MEMR(IXADD + 2*IX) = PIXEL3(IX+1) 
         MEMR(IXADD + 2*IX+1) = MEMR(IXADD + 2*IX) + XDEL
 400  CONTINUE
C
      CALL MSGPUT ('NINTERP = '//STRINT(NINTERP), 'D')
      CALL MSGPUT ('N       = '//STRINT(N), 'D')
      DO 500 I = 2, NINTERP
         JYADD = IYADD
         JXADD = IXADD
         N = N * 2
         CALL DATMAKAR ('SCRATCH/InterpY'//STRINT(I), 1, 2*N, 'R', 
     $      IYADD)
         CALL DATMAKAR ('SCRATCH/InterpX'//STRINT(I), 1, 2*N, 'R', 
     $      IXADD)
         CALL PIXSINCI (N, MEMR(JYADD), MEMR(IYADD))
         XDEL = (MEMR(JXADD+2) - MEMR(JXADD+1))/2.0
         DO 450 IX = 0, N-1
            MEMR(IXADD + 2*IX) = MEMR(JXADD + IX) 
            MEMR(IXADD + 2*IX+1) = MEMR(IXADD + 2*IX) + XDEL
 450     CONTINUE
         CALL MSGPUT ('Interpolation '//STRINT(I), 'D')
C         CALL PIXRLIS1 (MEMR(IYADD+N), 20, 1, 'Interpolation')
         CALL DATDELET ('SCRATCH/InterpY'//STRINT(I-1) )
         CALL DATDELET ('SCRATCH/InterpX'//STRINT(I-1) )
         NFINAL = 2*N
         IFINAL = I
 500  CONTINUE
C
C Convert from  'SCRATCH/Interp'//STRINT(NINTERP) to PB
C
      CALL MSGPUT ('Done Interpolating', 'D')
      IF (NFINAL .LT. 2*NPB) 
     $   CALL MSGPUT ('Not enough interpolation', 'W')
      CALL MSGPUT ('Clearing PB array', 'D')
      DO 600 I = 1, NPB
         PB(I) = 0.0
 600  CONTINUE
      NMIS = 0
      CALL MSGPUT ('Filling PB array', 'D')
      RADMAX = MEMR(IXADD+NFINAL-1)/RCONST
      WRITE (MESSAGE, 7495) RADMAX, NFINAL/2
 7495 FORMAT ('Using RADMAX = ',E14.7, ' NFINAL/2 = ',I10)
      CALL MSGPUT (MESSAGE, 'D')
      RSCL = FLOAT(NPB-1) / RADMAX
      DO 700 I = NFINAL/2+1, NFINAL
         RAD = MEMR(IXADD + I)/RCONST
         INDX = 1 + NINT(RSCL * RAD)
         IF (INDX .LE. NPB) THEN
            PB(INDX) = MEMR(IYADD + I)
         ELSE
            NMIS = NMIS + 1
         ENDIF
 700  CONTINUE
      PB(1) = 1.0
      CALL MSGPUT ('Checking for ZEROES in PB array', 'D')
      DO 800 I = 2, NPB
         IF (PB(I) .GT. 1.0) PB(I) = 1.0
         IF (PB(I) .EQ. 0.0  .AND. PB(I-1) .GE. NOTZERO) PB(I) =PB(I-1)
 800  CONTINUE
C      CALL PIXRLIS1 (PB, 20, 1, 'Interpolated PB')
C
C List some of the array
C
      CALL DATDELET ('SCRATCH/InterpY'//STRINT(IFINAL) )
      CALL DATDELET ('SCRATCH/InterpX'//STRINT(IFINAL) )
      CALL MSGPUT ('Done with making PB array', 'D')
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END



