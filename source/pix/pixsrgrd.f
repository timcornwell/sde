C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsrgrd.f	1.2	 7/17/95
C
      SUBROUTINE PIXSRGRD (IN, INX, INY, INZ, IDZ, IRZ, IRVZ,
     $   OUT, ONX, ONY, ONZ, ODZ, ORZ, ORVZ, BLANK, METHOD)
C
CD Spectral Cube Regrid:  Puts planes at different freqs
C
C	IN	R	inp	Input image cube
C	INX	I	inp	NX for input
C	INY	I	inp	NY for input
C	INZ	I	inp	NZ for input
C	IDZ	R	inp	Z increment for input
C	IRZ	R	inp	Reference pixel for Z in
C	IRVZ	D	inp	Reference Valus for Z in
C
C	OUT	R	out	Output image cube
C	ONX	I	inp	NX for output
C	ONY	I	inp	NY for output
C	ONZ	I	inp	NZ for output
C	ODZ	R	inp	Z increment for output
C	ORZ	R	inp	Reference pixel for Z out
C	ORVZ	D	inp	Reference Valus for Z out
C
C	BLANK	R	inp	Value to blank with
C	METHOD	C*(*)	inp	Method to use (currently ignored)
C
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C
C	Had I been less lazy, it would have been more utilitarian to
C	put the guts into another routine which acted on 1-D spectra
C
C				M.A. Holdaway	March 17 1993
C	Added Reference Values which may differ for the IN and OUT
C				M.A. Holdaway	July 17, 1995
C--------------------------------------------------------------------
#include	"stdinc.h"
C      	
      INTEGER	INX, INY, INZ, ONX, ONY, ONZ
      REAL	IN(INX, INY, *), OUT(ONX, ONY, *)
      REAL	IDZ, ODZ, IRZ, ORZ, BLANK, IRVZ, ORVZ
      CHARACTER*(SYSMXNAM)	METHOD
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSRGRD')
C
      REAL	A, B
      INTEGER	IX, IY, IZ, JZ
      INTEGER	MF
      PARAMETER	(MF = 1024)
      REAL	BIFRQ(MF), BOFRQ(MF), EIFRQ(MF), EOFRQ(MF) 
      REAL	SUMWT, SUMSPEC, WT
C=====================================================================
C
      IF (ERROR) GOTO 999
C
C Check for insanity
C
      A = IDZ / ABS(IDZ)
      B = ODZ / ABS(ODZ)
      IF (A .NE. B) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'The two freq axes must run in the same direction')
         GOTO 990
      ENDIF
      IF (ABS(ODZ) .LT. ABS(IDZ) ) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Output channel spacing must be greater than input')
         GOTO 990
      ENDIF
      IF (INX .NE. ONX   .OR.  INY .NE. ONY) THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE,
     $      'Input and Output NX or NY disagree')
         GOTO 990
      ENDIF
      IF (INZ .GT. MF  .OR. ONZ .GT. MF) THEN
         CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $      'Recompile PIXSRGRID with bigger MF')
         GOTO 990
      ENDIF
C
C Find Beginning and Ending Freqs of each channel, In and Out
C central channel arbitrarilly set to 0.0
C
      DO 20 IZ = 1, INZ
         BIFRQ(IZ) = IRVZ + ((IZ - IRZ) - 0.5) * IDZ
         EIFRQ(IZ) = IRVZ + ((IZ - IRZ) + 0.5) * IDZ
 20   CONTINUE
      DO 22 IZ = 1, ONZ
         BOFRQ(IZ) = ORVZ + ((IZ - ORZ) - 0.5) * ODZ
         EOFRQ(IZ) = ORVZ + ((IZ - ORZ) + 0.5) * ODZ
 22   CONTINUE
C
C Now do spectral munging
C
      DO 100 IY = 1, INY
         DO 90 IX = 1, INX
            DO 50 IZ = 1, ONZ
               IF (A*BIFRQ(1) .GT. A*BOFRQ(IZ) ) THEN
                  OUT (IX, IY, IZ) = BLANK
               ELSE IF (A*EIFRQ(INZ) .LT. A*EOFRQ(IZ) ) THEN
                  OUT (IX, IY, IZ) = BLANK
               ELSE
                  SUMWT = 0.0
                  SUMSPEC = 0.0
                  DO 40 JZ = 1, INZ
                     IF (A*BIFRQ(JZ) .LT. A*BOFRQ(IZ)   .AND.
     $                   A*EIFRQ(JZ) .GT. A*BOFRQ(IZ) ) THEN
                        WT = (EIFRQ(JZ) - BOFRQ(IZ)) /
     $                       (EIFRQ(JZ) - BIFRQ(JZ))
                        SUMWT = SUMWT + WT
                        SUMSPEC = SUMSPEC + IN(IX, IY, JZ) * WT
                     ELSE IF (A*BIFRQ(JZ) .LT. A*EOFRQ(IZ)   .AND.
     $                   A*EIFRQ(JZ) .GT. A*EOFRQ(IZ) ) THEN
                        WT = (EOFRQ(IZ) - BIFRQ(JZ)) /
     $                       (EIFRQ(JZ) - BIFRQ(JZ))
                        SUMWT = SUMWT + WT
                        SUMSPEC = SUMSPEC + IN(IX, IY, JZ) * WT
                     ELSE IF (A*BIFRQ(JZ) .GE. A*BOFRQ(IZ)   .AND.
     $                   A*EIFRQ(JZ) .LE. A*EOFRQ(IZ) ) THEN
                        SUMWT = SUMWT + 1.0
                        SUMSPEC = SUMSPEC + IN(IX, IY, JZ)
                     ENDIF
 40               CONTINUE
                  IF (SUMWT .LE. 0.0) THEN
                     CALL ERRREPOR (ERRLOGIC, ROUTINE,
     $                  'SUMWT should be > 0 if you got here')
                     GOTO 990
                  ENDIF
                  OUT(IX, IY, IZ) = SUMSPEC / SUMWT
               ENDIF
 50         CONTINUE
 90      CONTINUE
 100  CONTINUE
C
 990  CONTINUE
      IF (ERROR) THEN
         CALL ERRTRACE(ROUTINE)
      ENDIF
C
 999  CONTINUE
      END
