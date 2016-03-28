C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C @(#)crdshift.f	1.4    2/13/91
C
      SUBROUTINE CRDSHIFT (VIS, IMG, SHIFT)
C
CD Find the shift which will align VIS with IMG. The shift is to be
C applied to the u,v,w coordinates.
C
C
C	IMG	CH*(*)		input	Name of directory entry
C	VIS	CH*(*)		input	Name of directory entry
C	SHIFT	DBLE(3,*)	output	Shift matrix
C Audit trail:
C	Changed to shift matrix
C				T.J.Cornwell	Feb 13 1989
C      Corrected for possible rotation of UV plane
C				T.J.Cornwell	May 25 1989
C      Changed to Double for SHIFT
C				T.J.Cornwell	Jan 3 1990
C	Fixed a bug: TMAT(3,2) was zero instead of what it should be.
C	This would introduce a small error for pointings far away from
C	the MOSAIC image center.
C
C---------------------------------------------------------------------
#include	"stdinc.h"
C
C
      CHARACTER*(*)	IMG, VIS
      DOUBLE PRECISION	SHIFT(3,*)
C
      CHARACTER*(*)	ROUTINE
      PARAMETER		(ROUTINE = 'CRDSHIFT')
C
      DOUBLE PRECISION  RMAT(3,3), TMAT(3,3), SMAT(3,3)
      DOUBLE PRECISION	RA, DEC, RAO, DECO, DELRA, DELDEC, DTOR
      DOUBLE PRECISION	INPOS(3), OUTPOS(3)
      REAL              ROTA
      CHARACTER*8	VTYPE(SYSMXDIM)
      DOUBLE PRECISION	VRVAL(SYSMXDIM)
      REAL      	VROTA(SYSMXDIM)
      CHARACTER*8	ITYPE(SYSMXDIM)
      DOUBLE PRECISION	IRVAL(SYSMXDIM)
      INTEGER		VNAX, INAX, I, J, K, STRSEARC
C=====================================================================
C
C If an error on input then exit immediately
C
      IF (ERROR) GO TO 999
C
      CALL DATGETC (VIS, 'CTYPE', VTYPE, SYSMXDIM, VNAX)
      CALL DATGETC (IMG, 'CTYPE', ITYPE, SYSMXDIM, INAX)
      CALL DATGETD (VIS, 'CRVAL', VRVAL, SYSMXDIM, VNAX)
      CALL DATGETD (IMG, 'CRVAL', IRVAL, SYSMXDIM, INAX)
      CALL DATGETR (VIS, 'CROTA', VROTA, SYSMXDIM, VNAX)
      IF (ERROR) GO TO 990
C
C Find RA axis
C
      I = STRSEARC('RA', VTYPE, VNAX)
      IF (I.NE.0) THEN
         INPOS(1) = VRVAL(I)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot find RA axis')
         GO TO 999
      END IF
      I = STRSEARC('DEC', VTYPE, VNAX)
      IF (I.NE.0) THEN
         INPOS(2) = VRVAL(I)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 'Cannot find DEC axis')
         GO TO 999
      END IF
      I = STRSEARC('RA---SIN', ITYPE, INAX)
      IF (I.NE.0) THEN
         OUTPOS(1) = IRVAL(I)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find RA---SIN axis')
         GO TO 999
      END IF
      I = STRSEARC('DEC--SIN', ITYPE, INAX)
      IF (I.NE.0) THEN
         OUTPOS(2) = IRVAL(I)
      ELSE
         CALL ERRREPOR (ERRBDARG, ROUTINE, 
     1      'Cannot find DEC---SIN axis')
         GO TO 999
      END IF
C
C Find rotation matrix to convert from one position to another
C
      DTOR = 4.0D0 * ATAN(1.0D0) / 180.0D0
      RAO = DTOR * INPOS(1)
      DECO = DTOR * INPOS(2)
      RA = DTOR * OUTPOS(1)
      DEC = DTOR * OUTPOS(2)
C
C These equations came from Rick Perley
C
      TMAT(1,1) = COS(RA-RAO)
      TMAT(2,1) = SIN(RA-RAO) * SIN(DECO)
      TMAT(3,1) = - SIN(RA-RAO) * COS(DECO)
      TMAT(1,2) = - SIN(RA-RAO) * SIN(DEC)
      TMAT(2,2) =   COS(RA-RAO) * SIN(DEC) * SIN(DECO) +
     1   COS(DEC) * COS(DECO)
      TMAT(3,2) = - COS(RA-RAO) * COS(DECO) * SIN(DEC) +
     1   COS(DEC) * SIN(DECO)
      TMAT(1,3) = SIN(RA-RAO) * COS(DEC)
      TMAT(2,3) = - COS(RA-RAO) * SIN(DECO) * COS(DEC) +
     1   SIN(DEC) * COS(DECO)
      TMAT(3,3) =   COS(RA-RAO) * COS(DEC) * COS(DECO) +
     1   SIN(DEC) * SIN(DECO)
C
C Now make rotation matrix to correct for UV rotation
C
      ROTA = DTOR * VROTA(3)
      IF(ROTA.NE.0.0) THEN
         RMAT(1,1) = COS(ROTA)
         RMAT(1,2) = SIN(ROTA)
         RMAT(1,3) = 0.0
         RMAT(2,1) = -SIN(ROTA)
         RMAT(2,2) = COS(ROTA)
         RMAT(2,3) = 0.0
         RMAT(3,1) = 0.0
         RMAT(3,2) = 0.0
         RMAT(3,3) = 1.0
C
C Now multiply matrices: we rotate out the rotation of the uv plane
C before applying the shift in position. Then we will rotate back.
C This makes me glad I took linear algebra in high school.
C
         DO 100 J = 1, 3
            DO 110 I = 1, 3
               SMAT(I,J) = 0.0
               DO 120 K = 1, 3
                  SMAT(I,J) = SMAT(I,J) + TMAT(K,J) * RMAT(I,K)
 120           CONTINUE
 110        CONTINUE
 100     CONTINUE
C
C Now apply conjugate matrix to rotate the u,v coordinates back!
C
         DO 200 J = 1, 3
            DO 210 I = 1, 3
               SHIFT(I,J) = 0.0
               DO 220 K = 1, 3
                  SHIFT(I,J) = SHIFT(I,J) + RMAT(J,K) * SMAT(I,K)
 220           CONTINUE
 210        CONTINUE
 200     CONTINUE
      ELSE
         DO 300 J = 1, 3
            DO 310 I = 1, 3
               SHIFT(I,J) = TMAT(I,J)
 310        CONTINUE
 300     CONTINUE
      END IF
C
C Can jump to here if an error found
C
  990 IF (ERROR) THEN
         CALL ERRTRACE (ROUTINE)
      END IF
C
  999 CONTINUE
      END
