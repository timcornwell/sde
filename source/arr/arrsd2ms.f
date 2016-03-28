C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)arrsd2ms.f	1.3	 4/8/93
C
      SUBROUTINE ARRSD2MS (SDIMAGE, MOS, ANTFILE, FREQ, WT,
     $   BLC, TRC, STEP, TELE, TELDIAM, STOKES)
C
CD Grunt work: turn SDIMAGE into MOS database
C
C	SDIMAGE		CH*(*)	inp	Name of SD Image
C	MOS		CH*(*)	inp	Name of Mosaic Database
C	ANTFILE		CH*(*)	inp	Antfile for SD
C   	FREQ		R	inp	Frequency (0=look in header)
C	BLC		I(2)	inp	BLC of SD Image to work with
C	TRC		I(2)	inp	TRC of SD Image to work with
C	STEP		I(2)	inp	Pixel Step in each direction
C	TELE		CH*(*)	inp	Telescope name
C	TELDIAM		R	inp	Size of telescope
C	STOKES		CH*(*)	inp	Give me 1 Stokes Parameter
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	March 25 1993
C	Added STOKES
C				M.A.Holdaway	April 6 1993
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      CHARACTER*(*)	SDIMAGE, MOS, ANTFILE, TELE, STOKES
      REAL		TELDIAM, FREQ, WT
      INTEGER		BLC(*), TRC(*), STEP(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'ARRSD2MS')
C
      CHARACTER*1	T
      INTEGER	INAX, INAXIS(SYSMXDIM), VNAX, VNAXIS(SYSMXDIM)
      INTEGER	SDADD, VSADD, WADD
      INTEGER	IPC, NPC, IX, IY, I, NDUMMY
      REAL	PIXEL(2), WORLD(2)
      DOUBLE PRECISION		CRVAL(SYSMXDIM), VCRVAL(SYSMXDIM)
      CHARACTER*8	CTYPE(SYSMXDIM)
      CHARACTER*(SYSMXNAM)	VIS, SUB
C
      CHARACTER*(SYSMXNAM)	STRM2, STRM3
      CHARACTER*6		STRINT
      INTEGER			STRSEARC
C=====================================================================
      IF (ERROR) GO TO 999
C
      CALL DATGETAR (SDIMAGE, INAX, INAXIS, T, SDADD)
C
C Verify BLC, TRC
C
      IF (BLC(1) .LE. 0) BLC(1) = 1
      IF (BLC(2) .LE. 0) BLC(2) = 1
      IF (TRC(1) .LE. 0) TRC(1) = INAXIS(1)
      IF (TRC(2) .LE. 0) TRC(2) = INAXIS(2)
      IF (TRC(1) .GT. INAXIS(1)) TRC(1) = INAXIS(1) 
      IF (TRC(2) .GT. INAXIS(2)) TRC(2) = INAXIS(2)
      IF (STEP(1) .EQ. 0) STEP(1) = 1
      IF (STEP(2) .EQ. 0) STEP(2) = 1
C
      CALL DATGETD (SDIMAGE, 'CRVAL', CRVAL, SYSMXDIM, NDUMMY)
      CALL DATGETC (SDIMAGE, 'CTYPE', CTYPE, SYSMXDIM, NDUMMY)
      IF (FREQ .LE. 0.0) THEN
         IF (ERROR) GO TO 990
         I = STRSEARC ('FREQ', CTYPE, NDUMMY)
         IF (I.EQ.0) THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'No frequency axis')
            GO TO 999
         ELSE
            FREQ = CRVAL(I)
         END IF
      ENDIF
      IPC = 0
      IF (STOKES(1:1) .EQ. ' ') THEN
         SUB = 'OBS/I'
      ELSE
         SUB = 'OBS/'//STOKES(1:1)
      ENDIF
C
      IF (ERROR) GOTO 990
      DO 110 IY = BLC(2), TRC(2), STEP(2)
         DO 100 IX = BLC(1), TRC(1), STEP(1)
            IPC = IPC+1
            VIS = STRM2(MOS, 'PC'//STRINT(IPC))
            CALL SIMUV (DBLE(FREQ), 'Antfile', 0.D0, 0.D0, CRVAL(2), 
     1         0.D0, 0.D0, 100.D0, .TRUE.,  1.0, VIS)
            IF (STOKES(1:1) .NE. 'I') THEN
               CALL DATRENAM (STRM2(VIS, 'OBS/I'), STRM2(VIS, SUB))
            ENDIF
            CALL DATGETD (STRM2(VIS, SUB), 'CRVAL', VCRVAL, 
     $         SYSMXDIM, NDUMMY)
            VCRVAL(1) = CRVAL(1)
            CALL DATPUTD (STRM2(VIS, SUB), 'CRVAL', VCRVAL, 
     $         SYSMXDIM)
            CALL DATGETAR (STRM3(VIS, SUB, 'VIS'), VNAX, VNAXIS, 
     $         T, VSADD)
            MEMX (VSADD) = CMPLX( MEMR(SDADD + IX-1 + (IY-1)*INAXIS(1)),
     $         0.0)
           CALL DATGETAR (STRM3(VIS, SUB, 'WT'), VNAX, VNAXIS, 
     $         T, WADD)
           MEMR(WADD) = WT
            CALL DATPUTC (VIS, 'TELESCOP', TELE, 1)
            IF (TELDIAM .GT. 0.0)  CALL DATPUTR (VIS, 'TELDIAM', 
     $         TELDIAM, 1)
C
            PIXEL(1) = IX
            PIXEL(2) = IY
            CALL CRDPTOW (SDIMAGE, PIXEL, WORLD)
            CALL DATPUTD (VIS, 'OBSRA', DBLE(WORLD(1)), 1)
            CALL DATPUTD (VIS, 'OBSDEC', DBLE(WORLD(2)), 1)
            IF (ERROR) GOTO 990
 100     CONTINUE
 110  CONTINUE

      NPC = IPC
      CALL DATPUTI (MOS, 'NPC', NPC, 1)
      CALL DATSETTP (MOS, 'VISMOSAIC')



 990  IF (ERROR) CALL ERRTRACE (ROUTINE)
C
 999  CONTINUE
      END
