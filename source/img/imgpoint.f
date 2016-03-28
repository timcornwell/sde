C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)imgpoint.f	1.3	 21 Jul 1995
C
      SUBROUTINE IMGPOINT (NAME, IMAGE, NX, NY, RVAL, RPIX, DELT, ROTA, 
     $     PDELTA, LIBERAL, SOURCE, BAND, USERDEF, EPOCH, IDUR,
     $     SENS, FLAT, PB,
     $     STYLE, OUTFILE)
C
CD Get telescope pointings such that they fit in a mask image
C  This is really a mixed level routine, part PIXEL level, part IMG level
C
C	NAME	CH	input	Name of image
C	IMAGE	R(*,*)	input	Actual image
C	NX	INT(*)	input	Size of image
C	NY	INT(*)	input	Size of image
C	RVAL	DOUB(*)	input	Reference value
C	RPIX	REAL(*)	input	Reference Pixel
C	DELT	REAL(*)	input	Image Cell size
C	ROTA	REAL(*)	input	Image rotation
C	PDELTA	REAL	input	Pointing separation
C	LIBERAL	LOG	input	[T: more points | F: Less points]
C	SOURCE	CH(*)	input	Name of source
C	BAND	CH(*)	input	Name of VLA Band
C	USERDEF	CH(*)	input	Name of UserDefault
C	EPOCH	CH(*)	input	[B|J]
C	IDUR	INT	input	scan duration in seconds
C	SENS	CH(*)	input	Sensitivity Image
C       FLAT	CH(*)	input   Flat (scratch) image
C	PB	CH(*)	input	PB (scratch) image
C	STYLE	CH(*)	input	Output style [ POINTS | OFFSET ]
C	OUTFILE	CH(*)	input	Output file
C
C
C Audit trail:
C	New subroutine
C					M.A. Holdaway	Jan 18 1995
C	DeNAXISed
C					T.J. Cornwell   Jul 21 1995
C
C-------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	NAME, STYLE, OUTFILE, SENS, FLAT, PB
      INTEGER		NX, NY
      REAL		IMAGE(NX, NY)
      DOUBLE PRECISION	RVAL(*)
      REAL		DELT(*), RPIX(*), ROTA(*), PDELTA
      CHARACTER*(*)	STYLE, OUTFILE
      LOGICAL		LIBERAL
C
      CHARACTER*(*)     ROUTINE
      PARAMETER         (ROUTINE = 'IMGPOINT')
C
      INTEGER		NDUMMY
      CHARACTER*1	SIGN
      REAL	WORLD(SYSMXDIM), PIXEL(SYSMXDIM)
      INTEGER	IX, IY, IY0, IY1, IY2, IX1, IX2, IP
      INTEGER	RA1, RA2, DEC1, DEC2, NX, NY
      REAL	RA3, DEC3
      REAL	RIX0, RIY0, DY, DX
      REAL		DURT, DURM, DURS, ARRMAX, SCALE
      INTEGER		IDUR, IDUR2
      CHARACTER*(SYSMXNAM)	SOURCE, BAND, USERDEF, EPOCH
      CHARACTER*10	RASTR
      CHARACTER*11	DECSTR
      CHARACTER*2	ISTRING2
      CHARACTER*6	ISTRING6
C====================================================================
      IF (ERROR) GOTO 999
C
      CALL TXTOPEN (ROUTINE, OUTFILE, 'WRITE')
      IP = 0
C
C Find the Y extent of nonzero pixels in the image
C
      DO 110 IY = 1, NY
         DO 100 IX = 1, NX
            IF (IMAGE(IX, IY) .GT. 0.0) THEN
               IY1 = IY
               GOTO 120
            ENDIF
 100     CONTINUE
 110  CONTINUE
      CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Mask file has no valid pixels')
      GOTO 990
C
 120  CONTINUE
      DO 210 IY = NY, 1, -1
         DO 200 IX = 1, NX
            IF (IMAGE(IX, IY) .GT. 0.0) THEN
               IY2 = IY
               GOTO 220
            ENDIF
 200     CONTINUE
 210  CONTINUE
 220  CONTINUE
C
      DY = ABS(DELT(2)) * (IY2 - IY1)      
      IF (LIBERAL) THEN
         NY = INT( DY / PDELTA ) + 1
      ELSE
         NY = INT( DY / PDELTA )
      ENDIF
C
      DO 500 IY = 1, NY
         IF (NY .EQ. 1) THEN
            IY0 = (IY1 + IY2)/2.0
            RIY0 = IY0
         ELSE IF (IY .EQ. 1) THEN
            IY0 = IY1
            RIY0 = IY0
         ELSE IF (IY .EQ. NY) THEN
            IY0 = IY2
            RIY0 = IY0
         ELSE
            IY0 = (IY2 + IY1)/2 - FLOAT(NY-1)/2.0 * PDELTA/ABS(DELT(2))
     $           + (IY-1) * PDELTA/ABS(DELT(2))
            RIY0 = FLOAT(IY2 + IY1)/2.0 - FLOAT(NY-1)/2.0 * 
     $           PDELTA/ABS(DELT(2)) + FLOAT(IY-1) * PDELTA/ABS(DELT(2))            
         ENDIF
C            
         DO 250 IX = 1, NX
            IF (IMAGE(IX, IY0) .GT. 0.0) THEN
               IX1 = IX
               GOTO 260
            ENDIF
 250     CONTINUE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Oops, missed pixels1')
         GOTO 990
 260     CONTINUE
         DO 270 IX = NX, 1, -1
            IF (IMAGE(IX, IY0) .GT. 0.0) THEN
               IX2 = IX
               GOTO 280
            ENDIF
 270     CONTINUE
         CALL ERRREPOR (ERRLOGIC, ROUTINE, 'Oops, missed pixels2')
         GOTO 990
 280     CONTINUE
C
         DX = ABS(DELT(1)) * (IX2 - IX1)      
         IF (LIBERAL) THEN
            NX = INT( DX / PDELTA ) + 1
         ELSE
            NX = INT( DX / PDELTA )
         ENDIF
C
         SIGN = ' '
         DO 300 IX = NX, 1, -1
            IP = IP + 1
            RIX0 = (IX2 + IX1)/2 - FLOAT(NX-1)/2.0 * PDELTA/ABS(DELT(1))
     $           + (IX-1) * PDELTA/ABS(DELT(1))
            PIXEL(1) = RIX0
            PIXEL(2) = RIY0
            CALL CRDPTOW (NAME, PIXEL, WORLD)
C
            CALL DATPUTD (FLAT, 'OBSRA', DBLE(WORLD(1)), 1)
            CALL DATPUTD (FLAT, 'OBSDEC', DBLE(WORLD(2)), 1)
            CALL DATPUTD (PB, 'OBSRA', DBLE(WORLD(1)), 1)
            CALL DATPUTD (PB, 'OBSDEC', DBLE(WORLD(2)), 1)
            CALL IMGPB (FLAT, PB, 'APPLY')
            CALL ARRLC (SENS, 1.0, PB, 1.0, SENS)
C
            RA1 = INT(WORLD(1)/15.0)
            RA2 = INT((WORLD(1)/15.0 - RA1)*60)
            RA3 = (WORLD(1)/15.0 - RA1 - RA2/60) * 60.0
            IF ( WORLD(2) .LT. 0.0 ) SIGN = '-'
            WORLD(2) = ABS(WORLD(2))
            DEC1 = INT(WORLD(2))
            DEC2 = INT((WORLD(2) - DEC1)*60)
            DEC3 = (WORLD(2) - DEC1 - DEC2/60) * 60.0
            RASTR = ISTRING2(RA1)//ISTRING2(RA2)//ISTRING6(RA3)
            DECSTR = SIGN//ISTRING2(DEC1)//ISTRING2(DEC2)//
     $           ISTRING6(DEC3)
            IF (STYLE(1:1) .EQ. 'O' .AND. IX .EQ. NX) THEN
               DURT = IDUR*NX+30
               DURM = INT(DURT/60)
               DURS = DURT - DURM*60
               IDUR2 = 100 * DURM + DURS
               WRITE (MESSAGE, 1010) SOURCE(1:9),IDUR2,EPOCH(1:1), 
     $              RASTR, DECSTR,
     $              BAND(1:2), USERDEF(1:2), BAND(1:2), NX
 1010          FORMAT (A9,',,',I4,',',A1,',',A10,',',A11,',',
     $              A2,',',A2,',',A2,',',I4)
               CALL TXTWRITE (ROUTINE, MESSAGE)
            ELSE IF (STYLE(1:1) .EQ. 'P') THEN
               DURM = INT(IDUR/60)
               DURS = IDUR - DURM*60
               IDUR2 = 100 * DURM + DURS
               WRITE (MESSAGE, 1020) SOURCE(1:9),IP,IDUR2,EPOCH(1:1), 
     $              RASTR, DECSTR,
     $              BAND(1:2), USERDEF(1:2), BAND(1:2)
 1020          FORMAT (A9,',',I3,',',I4,',',A1,',',A10, ',',
     $              A11,',',A2,',',A2,',',A2)
               CALL TXTWRITE (ROUTINE, MESSAGE)
            ENDIF
 300     CONTINUE
 500  CONTINUE
C
      IF (LIBERAL) THEN
         WRITE (MESSAGE, 1032) IP
      ELSE
         WRITE (MESSAGE, 1042) IP
      ENDIF
 1032 FORMAT('Total pointings, LIBERAL placement: ',I5)
 1042 FORMAT('Total pointings, CONSERVATIVE placement: ',I5)
      CALL MSGPUT (MESSAGE, 'Z')
C
C Sqrt and Normalize sensitivity image
C
      CALL ARRPOWER (SENS, 0.5, 0.0, SENS)
      CALL ARRSTAT (SENS, ' ')
      CALL DATGETR (SENS, 'ARRMAX', ARRMAX, 1, NDUMMY)
      SCALE = 1.0/ARRMAX
      CALL ARRSCALE (SENS, SCALE, 0.0, SENS)
C
 990     CONTINUE
      IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
      CHARACTER*6 FUNCTION ISTRING6 (R)
C
C We take a real and write it as 25.280 or 05.762
C
C
C
C	R	REAL	INP	Seconds
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	24 Jan 94
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      REAL	R
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ISTRING6')
C
      CHARACTER*6 STR
C=======================================================================
C
C If an error on input then exit immediately
C
      STR = '00.000'
      IF (ERROR) GO TO 999
C
      IF (R .LT. 10.0) THEN
         WRITE (STR, 100) R
 100     FORMAT ('0',F5.3)
      ELSE
         WRITE (STR, 200) R
 200     FORMAT (F6.3)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      ISTRING6 = STR
      RETURN
      END
      CHARACTER*2 FUNCTION ISTRING2 (I)
C
C We take an integer I and write out a CH*(2) string, like '22', '03'
C
C
C
C	I	INT	INP	INTEGER
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	Dec 17 1989
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER I
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ISTRING2')
C
      CHARACTER*2 STR
C=======================================================================
C
C If an error on input then exit immediately
C
      STR = '00'
      IF (ERROR) GO TO 999
C
      IF (I .LE. 9) THEN
         WRITE (STR, 100) I
 100     FORMAT ('0',I1)
      ELSE
         WRITE (STR, 200) I
 200     FORMAT (I2)
      ENDIF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      ISTRING2 = STR
      RETURN
      END
C
