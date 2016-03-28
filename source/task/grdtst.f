C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)grdtst.f	1.3	 7/18/97
C
      SUBROUTINE SDEMAIN
C
CD Program to test GRDH2D
C
C Audit trail:
C	Original version: Audit trail comments go on this line
C	and successive lines
C				R.Payne  April 30 1991
C
C-----------------------------------------------------------------------
#include	"stdinc.h"
#define nvis 1000
C
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'GRDTST')
C
      INTEGER		I, N, N1
      REAL		STARTTIM, ENDTIME, SYSECPUT, XDUMMY
C Ok set some grdh2d variables
C
      INTEGER	NU, SUPPU, OSAMPU, NV, SUPPV, OSAMPV
      INTEGER	UORIGIN, VORIGIN
      COMPLEX 	VIS(nvis)
      LOGICAL	PSF
      REAL	WT(nvis)
      REAL	U(nvis), USCALE, UOFFSET, GRIDFNU(128)
      REAL	V(nvis), VSCALE, VOFFSET, GRIDFNV(128)
      REAL	W(nvis)
      DOUBLE PRECISION	SHIFT(3,3)
      REAL	SUMWT
C
      INTEGER 	IVIS
      INTEGER   NAXIS(SYSMXDIM), IADD, XNAXIS(SYSMXDIM)
      CHARACTER*(SYSMXNAM) OUT
      CHARACTER*6	   STRINT
      DATA	NAXIS	/SYSMXDIM*1/
C==================================================================
C
      CALL MSGWELCO ('I time GRDH2D for several grid sizes')
C
C Now create the data
C
      IVIS = nvis
      SHIFT(1,1) = 1.0D0
      SHIFT(2,2) = 1.0D0
      SHIFT(3,3) = 1.0D0
C
      DO 1 I = 1, IVIS
         VIS(I) = 1.0
         U(I) = MOD(I,60)
         V(I) = MOD(I,60)
         W(I) = 1.0
         WT(I) = 1.0
 1    CONTINUE   
C
      DO 2 I = 1, 128
         GRIDFNU(I) = 1.0
         GRIDFNV(I) = 1.0
 2    CONTINUE
C
C Now create the data
C
      NAXIS(1) = 64
      NAXIS(2) = 64
C
C Now loop through grid sizes
C
 3    CONTINUE
C
      NAXIS(1) = 2 * NAXIS(1)
      NAXIS(2) = 2 * NAXIS(2)
      N = NAXIS(1)
C
C Make 2D array
C
      OUT = 'Out'//STRINT(NAXIS(1))
      XNAXIS(1) = NAXIS(2)/2 + 1
      XNAXIS(2) = 2 * NAXIS(1)
      CALL DATMAKAR (OUT, 2, XNAXIS, 'X', IADD)
C
      USCALE = 1.0
      VSCALE = 1.0
      UOFFSET = 0.0
      VOFFSET = 0.0
      UORIGIN = 1
      VORIGIN = N/2
      PSF = .FALSE.
      NU = N
      NV = N
      SUPPU = 3
      SUPPV = 3
      OSAMPU = 1
      OSAMPV = 1
      SUMWT = 0.0
C Now time it
C
      STARTTIM = SYSECPUT(XDUMMY)
      CALL GRDH2D (MEMX(IADD), WT, U, V, W, IVIS, USCALE, UOFFSET, 
     1   VSCALE, VOFFSET, UORIGIN, VORIGIN, MEMX(IADD), PSF, NU, NV,
     2   GRIDFNU, SUPPU, OSAMPU, GRIDFNV, SUPPV, OSAMPV, SHIFT, SUMWT)
C
      ENDTIME = SYSECPUT(XDUMMY)
      WRITE (MESSAGE, 1010) 'Time per complex visibility gridding = ',
     1   1000*(ENDTIME-STARTTIM)/(SUMWT), ' milliseconds'
 1010 FORMAT (A,F10.3,A)
      CALL MSGPUT (MESSAGE, 'I')
      WRITE(MESSAGE,775) SUMWT, N
      CALL MSGPUT (MESSAGE, 'I')
 775  FORMAT(' Total weight =',F12.3,'  for grid=',I5)
      N1 = 0
C      DO 25 I = 1, NV
C      N = 0
C         DO 24 K =1, NU
C         RT = GVIS(I,K)
C         IF (RT.NE.0) THEN
C            N = N + 1
C            RD(N) = RT
C            ENDIF
C 24      CONTINUE
C         N = MIN(N,8)
C         IF(N.GT.0) THEN
C            WRITE(MESSAGE,776) I, (RD(K),K=1,N)
C            CALL MSGPUT (MESSAGE, 'I')
C 776        FORMAT(I3,' ',8F8.3)
C            N1 = N1 + 1
C            IF (N1.GE.10) GOTO 999
C            ENDIF
C 25   CONTINUE
 999  CONTINUE
      IF(NAXIS(1).LT.1024) GO TO 3
      END
