C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)pixsoflx.f	1.1    8/16/91
C
      SUBROUTINE PIXSOFLX (ARR, NX, NY, MODE, X, Y, DX, DY, N, FLUX)
C
CD Determines the flux in N regions specified by X,Y; DX,DY
C
C	ARR	REAL(*)	in	Input array
C	NX	INT	in	Size of array
C	NY	INT	in	Size of array
C	MODE	CH*(*)	in	'INTEGRATED', 'CENTER', AVERAGE'
C	X	INT(*)	in	X positions
C	Y	INT(*)	in	Y positions
C	DX	INT(*)	in	half width about X
C	DY	INT(*)	in	half width about Y
C	N	INT	in	dimension of X,Y,DX,DY
C	FLUX	REAL(*)	out	Flux associated with X,Y
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A.Holdaway	11 July 1991
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NX, NY, N
      REAL		ARR(NX, *), FLUX(*)
      INTEGER		X(*), Y(*), DX(*), DY(*)
      CHARACTER*(*)	MODE
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'PIXSOFLX')
C
      INTEGER		I, NPIX, IX, IY
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      IF (MODE(1:1) .NE. 'I' .AND.
     $   MODE(1:1) .NE. 'C'  .AND.
     $   MODE(1:1) .NE. 'A') THEN
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown mode '//MODE)
         GOTO 990
      ENDIF
C
      DO 1000 I = 1, N
         IF (X(I) .LT. 1 .OR. X(I) .GT. NX)  THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'X list corrupted')
            GOTO 990
         ENDIF
         IF (Y(I) .LT. 1 .OR. Y(I) .GT. NY)  THEN
            CALL ERRREPOR (ERRBDARG, ROUTINE, 'Y list corrupted')
            GOTO 990
         ENDIF
         IF (MODE(1:1) .EQ. 'C') THEN
            FLUX(I) = ARR(X(I), Y(I))
         ELSE
            FLUX(I) = 0.0
            NPIX = 0.0
            DO 200 IY = MAX(1, Y(I)-DY(I)), MIN(NY, Y(I)+DY(I))
               DO 100 IX = MAX(1, X(I)-DX(I)), MIN(NX, X(I)+DX(I))
                  NPIX = NPIX + 1
                  FLUX(I) = FLUX(I) + ARR(IX, IY)
 100           CONTINUE
 200        CONTINUE
C            
            IF (MODE(1:1) .EQ. 'A') THEN
               NPIX = MAX (1,  NPIX)
               FLUX(I) = FLUX(I) / FLOAT (NPIX)
            ENDIF
         ENDIF
 1000 CONTINUE
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
