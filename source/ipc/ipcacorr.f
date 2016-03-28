C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)ipcacorr.f	1.4    1/28/93
C
      SUBROUTINE IPCACORR (CORR, XSIZE, YSIZE, XVALUE, YVALUE, 
     $     FRAME, PHOTONS, NPHOTONS)
C
CD Auto-Correlate two ipcs data runs together
C
C	corr	REAL	input	Output correlation array (2D)
C	xsize	INT	input	half Size of above array
C	ysize	INT	input	half Size of above array
C	photons REAL    input   number of photons at each point
C	xvalue  REAL    input   x location of each photon
C	yvalue  REAL    input   y location of each photon
C	frame   REAL    input   frame number of each photon
C	nphotons INT    input   total size of above arrays
C Audit trail:
C	Cloned from arr3corr
C				R.G. Marson     Sep 16 1990
C
C--------------------------------------------------------------------
#include	"stdinc.h"
C
      INTEGER          XSIZE, YSIZE, NPHOTONS
      REAL             CORR(-XSIZE+1:XSIZE-1, -YSIZE+1:YSIZE-1)
      REAL             FRAME(NPHOTONS), PHOTONS(NPHOTONS)
      REAL             YVALUE(NPHOTONS), XVALUE(NPHOTONS)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'IPCACORR')
C
      REAL              CURFRAME, PHOTONSSQ, PH
      INTEGER           I, J, DX, DY, XV, YV
C=====================================================================
      IF (ERROR) GO TO 999
C
C Correlate every photon with every other photon
C
      DO I = 1, NPHOTONS - 1
C
C Set up variables so that they do not have to be indexed in the inner loop
C
         CURFRAME = FRAME(I)
         IF ((I/10000)*10000.EQ.I) THEN
            WRITE(MESSAGE,'(A,I8,A,I6,A,I6,A)') 'Processed ',I,
     $           ' Photons: Currently at Frame ', NINT(CURFRAME),
     $           ' (',NINT(FRAME(NPHOTONS)),')'
            CALL MSGPUT(MESSAGE, 'I')
         END IF
         XV = NINT(XVALUE(I))
         YV = NINT(YVALUE(I))
         PH = PHOTONS(I)
C
C If we have more than one photon at the current pixel then calculate
C  The correlation but do not correlate photons with themselves
C
         IF(PH.GT.1.5) THEN
            CORR(0,0) = (PH-1)*PH
         END IF
C
C Now correlate this photon with all the others (in the current frame)
C  This is really just a poor mans while loop
C
         DO J = I+1, NPHOTONS
            IF (FRAME(J) .EQ. CURFRAME) THEN
               DX = MIN(MAX(NINT(XVALUE(J)) - XV,-XSIZE+1),XSIZE-1)
               DY = MIN(MAX(NINT(YVALUE(J)) - YV,-YSIZE+1),YSIZE-1)
               PHOTONSSQ = PH * PHOTONS(J)
               CORR(DX,DY) = CORR(DX,DY) + PHOTONSSQ
               CORR(-DX,-DY) = CORR(-DX,-DY) + PHOTONSSQ
            ELSE
               GOTO 100
            END IF
         END DO  
 100     CONTINUE
      END DO
C
 999  CONTINUE
      END
