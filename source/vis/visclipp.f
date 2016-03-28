C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)visclipp.f	1.1    5/13/93
C
      SUBROUTINE VISCLIPP (VIS1, WT1, VIS2, WT2, VIS3, WT3, U, V,
     $   NVIS, UVLIM, THRES, NFLAG, MODE)
C
CD Clip vis: MODE = DIV:  IF (VIS1/VIS2 .GT. VMAX or .LT. VMIN), flag it in WT3
C  or, if    MODE = CLIP, IF (VIS1      .GT. VMAX or .LT. VMIN), flag it in WT3
C
C	VIS1	X(*)	inp	Data Vis to TEST
C	WT1	R(*)	inp	Wt of VIS1
C	VIS2	X(*)	inp	Divisor of VIS1 (if MODE = DIV)
C	WT2	R(*)	inp	Wt of VIS2
C	VIS3	X(*)	out	Out Vis
C	WT3	R(*)	out	Wt of VIS3 (yes, used)
C	U	R(*)	inp	U
C	V	R(*)	inp	V
C	NVIS	INT	inp	Number of vis
C	UVLIM	R(2)	inp	MIN, MAX UV RANGE
C	THRES	R(2)	inp	MIN MAX VIS for CLIPPING
C	NFLAG	INT	out	Number of vis flagged
C	MODE	CH*(*)	inp	Mode [DIV or CLIP]
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				M.A. Holdaway	April 9 1993
C
C-----------------------------------------------------------------------
C
#include		"stdinc.h"
C
      INTEGER		NFLAG, NVIS
      CHARACTER*(*)	MODE
      REAL		WT1(*), WT2(*), WT3(*), U(*), V(*)
      REAL		UVLIM(2), THRES(2)
      COMPLEX		VIS1(*), VIS2(*), VIS3(*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'VISCLIPP')
C
      INTEGER		I
      COMPLEX		XVAL
      REAL		RVAL, R
C=======================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      NFLAG = 0
      DO 100 I = 1, NVIS
         IF (WT1(I) .LE. 0.0) THEN
            WT3(I) = WT1(I)
            VIS3(I) = VIS1(I)
            GOTO 100
         ENDIF
C
         R = SQRT (U(I)*U(I) + V(I)*V(I) )
         IF (R .LT. UVLIM(1) .OR. R .GT. UVLIM(2) ) THEN
            VIS3(I) = VIS1(I)
            WT3(I) = WT1(I)
         ELSE
            IF (MODE(1:3) .EQ. 'DIV') THEN
               XVAL = VIS1(I) / VIS2(I)
               RVAL = CABS(XVAL)
            ELSE IF (MODE(1:4) .EQ. 'CLIP') THEN
               RVAL = CABS(VIS1(I))
            ELSE
               CALL ERRREPOR (ERRBDARG, ROUTINE, 'Unknown Mode')
               GOTO 999
            ENDIF
C
            IF (RVAL .LT. THRES(1) .OR. RVAL .GT. THRES(2)) THEN
               WT3(I) = 0.0
               VIS3(I) = 0.0
               NFLAG = NFLAG + 1
            ELSE
               WT3(I) = WT1(I)
               VIS3(I) = VIS1(I)
            ENDIF
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
